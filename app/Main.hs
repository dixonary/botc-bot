
module Main (main) where

import Network.HTTP.Client.Conduit (Response (responseBody))
import Network.HTTP.Simple

import Discord
import Discord.Requests
import Discord.Types hiding (Role)

import GHC.Generics (Generic)

import Data.List (sortOn)
import Data.List qualified as L

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

import Text.Regex.TDFA (getAllTextMatches, (=~))

import Data.Function (on)

import UnliftIO (IOException, MonadIO, liftIO)
import UnliftIO.Exception (try)
import UnliftIO.MVar

import Data.Maybe
import System.Environment (getEnv)

import Control.Applicative
import Control.Monad

import Data.Either (partitionEithers)
import Prelude hiding (id)

--------------------------------------------------------------------------------
-- DATA

-- A role in Blood on the Clocktower (with some data trimmed from the
-- official version on GitHub)
data Role = Role
  { id :: Text
  , name :: Text
  , team :: Text
  , edition :: Text
  , ability :: Text
  , flavor :: Text
  }
  deriving (Show, Read, Eq, Generic)

instance ToJSON Role
instance FromJSON Role

-- Internal bot state
data AppData = AppData
  { -- Pre-constructed embed data for each character
    embeds :: Map Text (Maybe CreateEmbed)
  , -- Pre-loaded role data from botc-data
    roles :: Map Text Role
  , -- Mappings from character names to their canonical id
    nameMappings :: Map Text Text
  , -- Mappings from request messageId to the response messageId
    linkedMessages :: Map (ChannelId, MessageId) MessageId
  }
  deriving (Show, Read)

dataFile :: FilePath
dataFile = "./cached_state"

--------------------------------------------------------------------------------
-- APPLICATION LOOP

main :: IO ()
main = do
  mdata <- do
    mfile <- try $ read <$> readFile "./cached_state"
    s <- case mfile of
      Right file -> do
        liftIO $ putStrLn "loaded existing state"
        pure file
      Left (_ :: IOException) -> do
        liftIO $ putStrLn "created new state"
        pure
          AppData
            { embeds = Map.empty
            , roles = Map.empty
            , nameMappings = Map.empty
            , linkedMessages = Map.empty
            }
    newMVar s

  -- On a bot restart, always pull the latest data
  reloadRoles mdata

  token <- T.pack <$> getEnv "DISCORD_TOKEN"

  userFacingError <-
    runDiscord $ do
      def
        { discordToken = token
        , discordOnStart = liftIO (putStrLn "Ready!")
        , discordOnEvent = eventHandler mdata
        , discordOnLog = \s -> T.putStrLn s >> T.putStrLn ""
        }
  T.putStrLn userFacingError

eventHandler :: MVar AppData -> Event -> DiscordHandler ()
eventHandler mdata event = case event of
  MessageCreate m
    | not (userIsBot (messageAuthor m)) -> do
        constructResponse m >>= mapM_ \resp -> do
          r <- restCall (CreateMessageDetailed (messageChannelId m) resp)
          printErrorOr (linkMessages mdata m) r
  MessageUpdate cid mid ->
    getLink (cid, mid) mdata >>= mapM_ \rid -> do
      r <- restCall $ GetChannelMessage (cid, mid)
      flip printErrorOr r $ \m' -> do
        constructResponse m' >>= mapM_ (restCall . EditMessage (cid, rid))
  MessageDelete cid mid ->
    getLink (cid, mid) mdata >>= mapM_ \_ ->
      unlinkMessage mdata cid mid
  _ -> pure ()
 where
  matchRegex :: Text
  matchRegex = "\\[\\[[a-zA-Z ]+\\]\\]"

  constructResponse :: Message -> DiscordHandler (Maybe MessageDetailedOpts)
  constructResponse m =
    case getAllTextMatches $ messageContent m =~ matchRegex of
      [] -> pure Nothing
      titles -> do
        let
          inners = map (T.drop 2 . T.dropEnd 2) titles

        (notFound, ts) <- partitionEithers <$> mapM search inners

        let
          nfMessage = case notFound of
            [] -> []
            [x] -> [[i|Could not find character: #{x}|]]
            xs -> [[i|Could not find characters: #{T.intercalate ", " xs}|]]

          truncateMessage = ["Showing first 3 characters only." | length ts > 3]

        embeds <- mapM getOrLoadEmbed (take 3 $ L.nub ts)

        pure $
          Just
            def
              { messageDetailedEmbeds = Just embeds
              , messageDetailedContent =
                  T.intercalate "\n" $ nfMessage <> truncateMessage
              }

  search :: Text -> DiscordHandler (Either Text Role)
  search t = do
    chars <- roles <$> readMVar mdata

    let
      -- Find the results, then take all the best matches, where:
      -- - Prefix matches beat suffix matches
      -- - Among those, shorter names are more likely to be the intended match
      xs = filter ((T.isInfixOf `on` T.toLower) t) (Map.keys chars)
      xs' = sortOn T.length xs
      (as, bs) = L.partition (t `T.isPrefixOf`) xs'

      bestMatch = listToMaybe as <|> listToMaybe bs

    pure $ case bestMatch >>= (chars Map.!?) of
      Just r -> Right r
      Nothing -> Left t

  getOrLoadEmbed :: Role -> DiscordHandler CreateEmbed
  getOrLoadEmbed role = do
    AppData{embeds} <- readMVar mdata

    case Map.lookup (id role) embeds of
      Just (Just cdata) -> pure cdata
      _ -> do
        embed <- makeEmbed role
        updateData mdata $ \s' ->
          s'{embeds = Map.insert (id role) (Just embed) embeds}
        pure embed

-- | Either prints an error and moves on, or does something with the given value
printErrorOr :: (Show e) => (a -> DiscordHandler ()) -> Either e a -> DiscordHandler ()
printErrorOr f r = case r of
  Left err -> liftIO $ print err
  Right x -> f x

--------------------------------------------------------------------------------
-- LINKED MESSAGES

{-
We link messages together so that, if a message is updated, its corresponding embed responses are updated. This means that if someone makes a typo and gets an error, they can fix it and the bot will update the response.

The bot will not (currently) delete responses if the request is deleted.
-}

-- | Find the response message for a given request message
getLink :: (ChannelId, MessageId) -> MVar AppData -> DiscordHandler (Maybe MessageId)
getLink key mdata = do
  s <- readMVar mdata
  pure $ Map.lookup key (linkedMessages s)

-- | Link a request message to a response message
linkMessages :: MVar AppData -> Message -> Message -> DiscordHandler ()
linkMessages mdata o r = do
  updateData mdata $ \s ->
    s
      { linkedMessages =
          Map.insert (messageChannelId o, messageId o) (messageId r) (linkedMessages s)
      }

-- | Unlink a request message from its response message
unlinkMessage :: MVar AppData -> ChannelId -> MessageId -> DiscordHandler ()
unlinkMessage mdata cid mid = do
  updateData mdata $ \s ->
    s
      { linkedMessages =
          Map.delete (cid, mid) (linkedMessages s)
      }

-- | Update the AppData in the MVar and write it to disk
updateData :: (MonadIO m) => MVar AppData -> (AppData -> AppData) -> m ()
updateData mdata f = liftIO $ do
  modifyMVar_ mdata (pure . f)
  s <- readMVar mdata
  writeFile dataFile (show s)

--------------------------------------------------------------------------------
-- EMBEDS

{- | Construct a Discord embed for a given role.
We don't do this upfront because it downloads the character image. In theory
we could ask for them all upfront which would speed up the first response for each character by ~100ms, at the expense of downloading all the images on first run.
-}
makeEmbed :: Role -> DiscordHandler CreateEmbed
makeEmbed role = do
  let
    wikiName = T.replace " " "_" $ name role
    wikiURL = [i|https://wiki.bloodontheclocktower.com/#{wikiName}|]

  liftIO $ print role

  let
    imgSrc = [i|https://botc-data.dixonary.co.uk/image/#{id role}.webp|]
  charImg <- responseBody <$> (httpBS =<< parseRequest (T.unpack imgSrc))

  pure $
    def
      { createEmbedTitle = name role
      , createEmbedAuthorName = teamAndEdition role
      , createEmbedDescription = ability role
      , createEmbedThumbnail = Just $ CreateEmbedImageUpload charImg
      , createEmbedFooterText = "\"" <> flavor role <> "\""
      , createEmbedColor = Just $ color (team role)
      , createEmbedUrl = wikiURL
      }

teamAndEdition :: Role -> Text
teamAndEdition role =
  T.intercalate " · " $
    catMaybes
      [ Just $ T.toTitle (team role)
      , case edition role of
          "tb" -> Just "Trouble Brewing"
          "snv" -> Just "Sects & Violets"
          "bmr" -> Just "Bad Moon Rising"
          "carousel" -> Just "The Carousel"
          _ -> Nothing
      ]

color :: Text -> DiscordColor
color "demon" = DiscordColorRed
color "minion" = DiscordColorOrange
color "outsider" = DiscordColorAqua
color "townsfolk" = DiscordColorBlue
color "traveller" = DiscordColorPurple
color "fabled" = DiscordColorGold
color "loric" = DiscordColorGreen
color _ = DiscordColorGray

--------------------------------------------------------------------------------
-- FETCHING ROLES

getChars :: IO [Role]
getChars = do
  -- Get data from botc-data.dixonary.co.uk
  req <- parseRequest "https://botc-data.dixonary.co.uk/roles.json"
  res <- httpJSONEither req

  case responseBody res of
    Left err -> do
      putStrLn [i|Error fetching roles: #{show err}|]
      pure []
    Right roles -> pure roles

reloadRoles :: MVar AppData -> IO ()
reloadRoles mdata = do
  putStrLn "Fetching characters from source..."
  chars <- getChars
  putStrLn [i|Found #{show (length chars)} characters.|]
  updateData mdata $ \s ->
    s
      { roles =
          Map.fromList
            [(id r, r) | r <- chars]
      , nameMappings =
          Map.fromList
            [(T.toLower (name r), id r) | r <- chars]
      }
