
module BotC.Commands
  ( Command (..)
  , CommandData (..)
  , commandToHandlers
  )
  where


import Network.HTTP.Conduit (parseRequest, Response (responseBody))
import Network.HTTP.Simple (httpBS)

import Discord
import Discord.Requests
import Discord.Handle (DiscordHandle (discordHandleCache))
import Discord.Internal.Gateway.Cache
import Discord.Types hiding (Role)
import Discord.Interactions

import Control.Monad.Trans.Reader qualified as Reader

import qualified Data.Map as Map

import Data.Text (Text)
import qualified Data.Text as T

import Data.Functor

import UnliftIO.MVar

import Data.Maybe

import Data.Traversable

import Control.Applicative

import Prelude hiding (id)

import BotC.BotState

data Command = MkCommand
  { name :: Text -- ^ Name of the command, shared with application command
  , helpText :: Text -- ^ Text to display to users
  , inlineAction :: Text -> Maybe (BotState -> MessageDetailedOpts)
  -- ^ Parses a message to determine whether to trigger, then returns a message after being given bot context
  , applicationCommand :: CommandData
  -- ^ The associated slash command
  }

data CommandData = MkCommandData
  { createCommand :: CreateApplicationCommand
  , interactionResponse :: Interaction -> Maybe (BotState -> InteractionResponse)
  -- ^ How to respond to an interaction triggered by this slash command
  }

-- | Register the given command's application command, and then return the event handler for this command.
commandToHandlers :: Command -> BotCHandler (BotState -> Event -> Maybe (BotCHandler ()))
commandToHandlers MkCommand {..} = do
  discordHandle <- liftD Reader.ask
  cache <- readMVar discordHandle.discordHandleCache.cacheHandleCache
  let applicationId = cache.cacheApplication.fullApplicationID
  ac <- restCall_ (CreateGlobalApplicationCommand applicationId applicationCommand.createCommand)
  let commandId = ac.applicationCommandId
  pure \botState -> \case
    MessageCreate m
      | not (userIsBot (messageAuthor m))
      , Just getMessage <- inlineAction (messageContent m)
      -> Just do
        md <- processMessageDetailed (getMessage botState)
        message <- restCall_ (CreateMessageDetailed (messageChannelId m) md)
        linkMessages name m message
    MessageUpdate cid mid
      | Just linkedId <- getLink botState (name, cid, mid)
      -> Just do
        m <- restCall_ $ GetChannelMessage (cid, mid)
        case inlineAction (messageContent m) of
          Nothing -> -- couldn't parse a new action, assume it's a user mistake and do nothing
            pure ()
          Just getMessage -> do
            md <- processMessageDetailed (getMessage botState)
            void $ restCall_ (EditMessage (cid, linkedId) md)
    MessageDelete cid mid
      | Just linkedId <- getLink botState (name, cid, mid)
      -> Just $ unlinkMessage name cid mid >> restCall_ (DeleteMessage (cid, linkedId))
    InteractionCreate ic
      | Just acId <- commandOrAutocompleteACId ic
      , acId == commandId
      , Just ir <- applicationCommand.interactionResponse ic
      -> Just $ do
        ir' <- processInteraction (ir botState)
        restCall_ $
          CreateInteractionResponse ic.interactionId ic.interactionToken ir'
    _ -> Nothing
  where
  commandOrAutocompleteACId = \case
    InteractionApplicationCommand {..}
      -> Just applicationCommandData.applicationCommandDataId
    InteractionApplicationCommandAutocomplete {..}
      -> Just applicationCommandData.applicationCommandDataId
    _ -> Nothing

  downloadCacheImage url = do
    botState <- readBotState
    let MkImageData imageMap = botState.cachedIcons
    case Map.lookup url imageMap of
      Just bs -> pure bs
      Nothing -> do
        charImg <- responseBody <$> (httpBS =<< parseRequest (T.unpack url))
        modifyBotState \s@MkBotState {cachedIcons = MkImageData imageMap'} -> s
          { cachedIcons = MkImageData (Map.insert url charImg imageMap') }
        pure charImg

  processMessageDetailed mdopts@MessageDetailedOpts {..} = do
    embeds' <- for messageDetailedEmbeds $ traverse \embed@CreateEmbed {..} -> do
      thumbail <- case createEmbedThumbnail of
        Just (CreateEmbedImageUrl url) ->
          Just . CreateEmbedImageUpload <$> downloadCacheImage url
        _ -> pure createEmbedThumbnail
      pure $ embed { createEmbedThumbnail = thumbail }
    pure $ mdopts {messageDetailedEmbeds = embeds'}

  processInteraction = \case
    InteractionResponseChannelMessage irm ->
      InteractionResponseChannelMessage <$> onInteractionResponseMessage irm
    i -> pure i

  onInteractionResponseMessage mdopts@InteractionResponseMessage {..} = do
    embeds' <- for interactionResponseMessageEmbeds $ traverse \embed@CreateEmbed {..} -> do
      thumbail <- case createEmbedThumbnail of
        Just (CreateEmbedImageUrl url) ->
          Just . CreateEmbedImageUpload <$> downloadCacheImage url
        _ -> pure createEmbedThumbnail
      pure $ embed { createEmbedThumbnail = thumbail }
    pure $ mdopts {interactionResponseMessageEmbeds = embeds'}
