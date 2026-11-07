
module BotC.Role (Role (..), RoleId (..)) where

import Discord
import Discord.Types hiding (Role, RoleId)

import GHC.Generics (Generic)

import Data.Text (Text)

import Data.Aeson (ToJSONKey, FromJSONKey)

newtype RoleId = MkRoleId Text
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

-- | A role in Blood on the Clocktower (with some data trimmed from the
-- official version on GitHub).
data Role = Role
  { id :: RoleId
  , name :: Text
  , team :: Text
  , edition :: Text
  , ability :: Text
  , flavor :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON Role
instance FromJSON Role

