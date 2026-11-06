
module BotC.Commands.Help (help) where

import Discord
import Discord.Interactions

import Data.List qualified as L

import Data.String.Interpolate (i)
import Data.Text qualified as T

import Data.Functor

import Data.Maybe

import Prelude hiding (id)

import BotC.Commands

help :: [Command] -> Command
help commands = MkCommand
  { name = "help"
  , helpText = "Displays help text for commands"
  , inlineAction = \_ -> Nothing
  , applicationCommand = MkCommandData { createCommand, interactionResponse }
  }
  where
  createCommand = CreateApplicationCommandChatInput
    { createName = "help"
    , createLocalizedName = Nothing
    , createDescription = "Help with commands"
    , createLocalizedDescription = Nothing
    , createOptions = Just $ OptionsValues
      [ OptionValueString
          { optionValueName = "name"
          , optionValueLocalizedName = Nothing
          , optionValueDescription = "What command to search for."
          , optionValueLocalizedDescription = Nothing
          , optionValueRequired = False
          , optionValueStringChoices = Left True
          , optionValueStringMinLen = Just 2
          , optionValueStringMaxLen = Just 20
          }
      ]
    , createDefaultMemberPermissions = Nothing
    , createDMPermission = Just True
    }
  interactionResponse = \case
    InteractionApplicationCommand {..}
      | ApplicationCommandDataChatInput {..} <- applicationCommandData
      -> Just \_botState -> case optionsData of
        Just (OptionsDataValues [OptionDataValueString _ (Right nameFragment)])
          | Just command <- L.find (\c -> c.name == nameFragment) commands
          ->
            InteractionResponseChannelMessage def
              { interactionResponseMessageContent = Just command.helpText
              }
          | otherwise ->
            InteractionResponseChannelMessage def
              { interactionResponseMessageContent = Just $ [i|Sorry, `#{nameFragment}` is not recognised as a valid command|]
              }
        _ ->
            InteractionResponseChannelMessage def
              { interactionResponseMessageContent = Just $ T.intercalate "\n" $
                "Here are all the commands you have access to:"
                : fmap
                  (\c -> [i|- `#{c.name}`: #{c.helpText}|])
                  commands
              }
    InteractionApplicationCommandAutocomplete {..}
      | ApplicationCommandDataChatInput {..} <- applicationCommandData
      , Just (OptionsDataValues [OptionDataValueString _ (Left nameFragment)]) <- optionsData
      -> Just \_botState ->
          InteractionResponseAutocompleteResult
        $ InteractionResponseAutocompleteString
        $ (\command -> Choice command.name Nothing command.name) <$> filter ((nameFragment `T.isInfixOf`) . (.name)) commands
    _ -> Nothing
