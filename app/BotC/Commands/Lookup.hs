
module BotC.Commands.Lookup (lookupCharacter) where

import Discord
import Discord.Requests
import Discord.Types hiding (Role)
import Discord.Interactions

import Data.List (sortOn)
import Data.List qualified as L

import Data.Map.Strict qualified as Map

import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as T

import Text.Regex.TDFA (getAllTextMatches, (=~))

import Data.Function (on, (&))
import Data.Functor
import Control.Monad ((<=<))

import Data.Maybe

import Data.Either (partitionEithers)

import Prelude hiding (id)

import BotC.Role
import BotC.Commands
import BotC.BotState

lookupCharacter :: Command
lookupCharacter = MkCommand
  { name = "lookup"
  , helpText = "Lookup a character by name. Use as `[[character_name]]` or `/lookup character_name`."
  , inlineAction = lookupText
  , applicationCommand = MkCommandData { createCommand, interactionResponse }
  }
  where
  matchRegex :: Text
  matchRegex = "\\[\\[[a-zA-Z \\'\\-]+\\]\\]"

  lookupEmbedsAndText botState chars = 
    let (notFound, ts) = partitionEithers $ search botState <$> chars

        nfMessage = case notFound of
          [] -> []
          [x] -> [[i|Could not find character: #{x}|]]
          xs -> [[i|Could not find characters: #{T.intercalate ", " xs}|]]

        truncateMessage = ["Showing first 3 characters only." | length ts > 3]

        embeds = fmap makeEmbed (take 3 $ L.nub ts)
    in (embeds, T.intercalate "\n" $ nfMessage <> truncateMessage)

  lookupText t = case getAllTextMatches $ t =~ matchRegex of
    [] -> Nothing
    titles -> Just \botState ->
      let inners = map (T.drop 2 . T.dropEnd 2) titles
      in lookupEmbedsAndText botState inners & \(embeds, content) -> def
            { messageDetailedEmbeds = Just embeds
            , messageDetailedContent = content
            }

  createCommand = CreateApplicationCommandChatInput
    { createName = "lookup"
    , createLocalizedName = Nothing
    , createDescription = "Lookup a character"
    , createLocalizedDescription = Nothing
    , createOptions = Just $ OptionsValues
      [ OptionValueString
          { optionValueName = "name"
          , optionValueLocalizedName = Nothing
          , optionValueDescription = "What character to search for."
          , optionValueLocalizedDescription = Nothing
          , optionValueRequired = True
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
      , Just (OptionsDataValues [OptionDataValueString _ (Right nameFragment)]) <- optionsData
      -> Just \botState -> lookupEmbedsAndText botState [nameFragment] &
        \(embeds, content) -> InteractionResponseChannelMessage def
          { interactionResponseMessageEmbeds = Just embeds
          , interactionResponseMessageContent = Just content
          }
    InteractionApplicationCommandAutocomplete {..}
      | ApplicationCommandDataChatInput {..} <- applicationCommandData
      , Just (OptionsDataValues [OptionDataValueString _ (Left nameFragment)]) <- optionsData
      -> Just \botState ->
          InteractionResponseAutocompleteResult
        $ InteractionResponseAutocompleteString
        $ take 25
        $ (\role -> Choice role.name Nothing role.name)
          <$> searchResults botState nameFragment
    _ -> Nothing

  search botState t = case searchResults botState t of
    [] -> Left t
    (r : _) -> Right r

  searchResults :: BotState -> Text -> [Role]
  searchResults botState t =
    let chars = roles botState
        nameToIds = Map.fromList $ Map.toList chars <&> \(rId, r) -> (r.name, rId)

        -- Find the results, then take all the best matches, where:
        -- - Prefix matches beat suffix matches
        -- - Among those, shorter names are more likely to be the intended match
        xs = filter ((T.isInfixOf `on` T.toLower) t) (Map.keys nameToIds)
        xs' = sortOn T.length xs
        (as, bs) = L.partition (t `T.isPrefixOf`) xs'

        bestMatches = as <> bs

    in mapMaybe ((chars Map.!?) <=< (nameToIds Map.!?)) bestMatches

  {- | Construct a Discord embed for a given role.
  -}
  makeEmbed :: Role -> CreateEmbed
  makeEmbed role =
    let
      wikiName = T.replace " " "_" $ role.name
      wikiURL = [i|https://wiki.bloodontheclocktower.com/#{wikiName}|]

      imgSrc = [i|https://botc-data.dixonary.co.uk/image/#{role.id.unRoleId}.webp|]

    in
      def
        { createEmbedTitle = role.name
        , createEmbedAuthorName = teamAndEdition role
        , createEmbedDescription = ability role
        , createEmbedThumbnail = Just $ CreateEmbedImageUrl imgSrc
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