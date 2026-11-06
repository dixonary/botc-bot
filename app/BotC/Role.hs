
module BotC.Role (Role (..)) where


import Discord
import Discord.Types hiding (Role)

import GHC.Generics (Generic)

import Data.Text (Text)

-- | A role in Blood on the Clocktower (with some data trimmed from the
-- official version on GitHub).
data Role = Role
  { id :: Text
  , name :: Text
  , team :: Text
  , edition :: Text
  , ability :: Text
  , flavor :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON Role
instance FromJSON Role

