module Server.Types
    ( Note(..)
    , NoteId
    , NewNote(..)
    , ServerState(..)
    , ServerDataStore(..)
    ) where

import Data.Aeson
import GHC.Generics

type NoteId = Int

data ServerDataStore = ServerDataStore
    { sdsGet    :: IO [Note]
    , sdsFind   :: NoteId -> IO (Maybe Note)
    , sdsPut    :: Note -> IO ()
    , sdsRemove :: NoteId -> IO ()
    }

data ServerState = ServerState { serverDataStore :: ServerDataStore }


data Note = Note
    { nId   :: NoteId
    , nNote :: String
    }
  deriving (Generic)

instance ToJSON Note

newtype NewNote = NewNote { nnNote :: String }
  deriving (Generic)

instance FromJSON NewNote
