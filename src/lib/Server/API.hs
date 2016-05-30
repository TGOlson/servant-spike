module Server.API
    ( API
    , server
    ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.List
import Servant hiding (Handler)
import Safe

import Server.Types

type API
    =    "note" :> QueryParam "like" String :> Get '[JSON] [Note]
    :<|> "note" :> Capture "noteId" Int :> Get '[JSON] Note
    :<|> "note" :> ReqBody '[JSON] NewNote :> Post '[JSON] Note
    :<|> "note" :> Capture "noteId" Int :> Delete '[JSON] ()


server :: ServerState -> Servant.Server API
server st = Servant.enter (runReaderTNat st) serverH
  where
    serverH =    getNotes
            :<|> getNote
            :<|> createNote
            :<|> removeNote


type Handler a = ReaderT ServerState (ExceptT Servant.ServantErr IO) a

getNotes :: Maybe String -> Handler [Note]
getNotes mSearch = do
    store <- serverDataStore <$> ask
    notes <- liftIO (sdsGet store)

    return $ case mSearch of
        Nothing -> notes
        Just x  -> filter (\n -> x `isInfixOf` nNote n) notes

getNote :: Int -> Handler Note
getNote nId = do
    store <- serverDataStore <$> ask
    liftIO (sdsFind store nId) >>= \case
        Nothing -> throwError err404
        Just n  -> return n


createNote :: NewNote -> Handler Note
createNote (NewNote n) = do
    store <- serverDataStore <$> ask

    notes <- liftIO (sdsGet store)
    let i = maybe 0 (succ . nId) (headMay notes)
        note = Note i n
    liftIO (sdsPut store note)

    return note

removeNote :: NoteId -> Handler ()
removeNote i = do
    store <- serverDataStore <$> ask
    liftIO (sdsRemove store i)
