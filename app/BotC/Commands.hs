
module BotC.Commands
  ( Command (..)
  , CommandData (..)
  , commandToHandlers
  )
  where


import Discord
import Discord.Requests
import Discord.Handle (DiscordHandle (discordHandleCache))
import Discord.Internal.Gateway.Cache
import Discord.Types hiding (Role)
import Discord.Interactions

import Control.Monad.Trans.Reader qualified as Reader

import Data.Text (Text)

import Data.Functor

import UnliftIO.MVar

import Data.Maybe

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
        message <- restCall_ (CreateMessageDetailed (messageChannelId m) (getMessage botState))
        linkMessages name m message
    MessageUpdate cid mid
      | Just linkedId <- getLink botState (name, cid, mid)
      -> Just do
        m <- restCall_ $ GetChannelMessage (cid, mid)
        case inlineAction (messageContent m) of
          Nothing -> -- couldn't parse a new action, assume it's a user mistake and do nothing
            pure ()
          Just getMessage ->
            void $ restCall_ (EditMessage (cid, linkedId) (getMessage botState))
    MessageDelete cid mid
      | Just linkedId <- getLink botState (name, cid, mid)
      -> Just $ unlinkMessage name cid mid >> restCall_ (DeleteMessage (cid, linkedId))
    InteractionCreate ic
      | Just acId <- commandOrAutocompleteACId ic
      , acId == commandId
      , Just ir <- applicationCommand.interactionResponse ic
      -> Just $ restCall_ $
        CreateInteractionResponse ic.interactionId ic.interactionToken (ir botState)
    _ -> Nothing
  where
  commandOrAutocompleteACId = \case
    InteractionApplicationCommand {..}
      -> Just applicationCommandData.applicationCommandDataId
    InteractionApplicationCommandAutocomplete {..}
      -> Just applicationCommandData.applicationCommandDataId
    _ -> Nothing
