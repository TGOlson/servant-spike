module Server.Types
    ( Note(..)
    ) where

import Data.Aeson
import GHC.Generics

data Note = Note
    { sId   :: Int
    , sNote :: String
    }
  deriving (Generic)

instance ToJSON Note
instance FromJSON Note
