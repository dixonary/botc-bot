
module BotC.BotState
  ( BotState (..)
  , BotCHandler
  , liftD
  , readBotState
  , modifyBotState
  , dataFile

  , ImageData (..)

  , restCall_

  , getLink
  , linkMessages
  , unlinkMessage

  , reloadRoles
  )
  where

import Network.HTTP.Client.Conduit (Response (responseBody))
import Network.HTTP.Simple

import Discord
import Discord.Types hiding (Role, RoleId)

import Control.Monad.Trans.Reader qualified as Reader
import Control.Monad.Trans.Class (MonadTrans(lift))

import GHC.Generics (Generic)

import GHC.Stack (HasCallStack)

import UnliftIO (MonadIO(liftIO), throwIO)
import UnliftIO.Concurrent (MVar, readMVar, modifyMVar_)

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Data.String.Interpolate (i)
import Data.Text (Text)

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BS.L

import Data.Aeson qualified as Aeson

import BotC.Role

newtype ImageData = MkImageData (Map Text BS.ByteString)
  deriving (Show)

instance FromJSON ImageData where
  parseJSON _ = pure $ MkImageData mempty

instance ToJSON ImageData where
  toJSON _ = Aeson.Null

-- | Bot state for the entire program, stored in an MVar.
--
-- Uses strict fields to try and reduce memory use.
data BotState = MkBotState
  { -- | Pre-loaded role data from botc-data.
    roles :: !(Map RoleId Role)
  , -- | Mappings from request message and command to the response message.
    linkedMessages :: !(Map (Text, ChannelId, MessageId) MessageId)
  , cachedIcons :: !ImageData
  }
  deriving (Show, Generic)

instance ToJSON BotState
instance FromJSON BotState

type BotCHandler = Reader.ReaderT (MVar BotState) DiscordHandler

liftD :: DiscordHandler a -> BotCHandler a
liftD = lift

readBotState :: BotCHandler BotState
readBotState = Reader.ask >>= readMVar

dataFile :: FilePath
dataFile = "./cached_state"

modifyBotState :: (BotState -> BotState) -> BotCHandler ()
modifyBotState f = do
  botStateMVar <- Reader.ask
  modifyMVar_ botStateMVar (pure . f)
  s <- readMVar botStateMVar
  liftIO $ BS.L.writeFile dataFile (Aeson.encode s)

-- | Throws the restCall error as an exception.
--
-- It's generally fine if we throw I think - if something's gone wrong, we can't always recover.
restCall_ :: HasCallStack => (Discord.Request (r a), FromJSON a) => r a -> BotCHandler a
restCall_ r = liftD (restCall r) >>= either throwIO pure


--------------------------------------------------------------------------------
-- LINKED MESSAGES

{-
We link messages together so that, if a message is updated, its corresponding embed responses are updated. This means that if someone makes a typo and gets an error, they can fix it and the bot will update the response.

The bot will not (currently) delete responses if the request is deleted.
-}

-- | Find the response message for a given request message
getLink :: BotState -> (Text, ChannelId, MessageId) -> (Maybe MessageId)
getLink s key = Map.lookup key (linkedMessages s)

-- | Link a request message to a response message
linkMessages :: Text -> Message -> Message -> BotCHandler ()
linkMessages commandName o r = do
  modifyBotState \s ->
    s
      { linkedMessages =
          Map.insert (commandName, messageChannelId o, messageId o) (messageId r) (linkedMessages s)
      }

-- | Unlink a request message from its response message
unlinkMessage :: Text -> ChannelId -> MessageId -> BotCHandler ()
unlinkMessage commandName cid mid = do
  modifyBotState \s ->
    s
      { linkedMessages =
          Map.delete (commandName, cid, mid) (linkedMessages s)
      }


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

reloadRoles :: BotCHandler ()
reloadRoles = do
  liftIO $ putStrLn "Fetching characters from source..."
  chars <- liftIO getChars
  liftIO $ putStrLn [i|Found #{show (length chars)} characters.|]
  modifyBotState \s ->
    s
      { roles =
          Map.fromList
            [(r.id, r) | r <- chars]
      }

