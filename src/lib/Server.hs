module Server
    ( main
    ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Proxy
import Servant hiding (Handler)
import qualified Servant
import Network.Wai
import Network.Wai.Handler.Warp

import Server.API
import Server.Types

data ServerState = ServerState { serverDataStore :: State () [Note] }

main :: IO ()
main = run 8000 app

app :: Application
app = Servant.serve (Proxy :: Proxy API) server

toServantHandler' :: forall a. ServerState -> Handler a -> Servant.Handler a
toServantHandler' = flip runReaderT

server :: Servant.Server API
server = Servant.enter toServantHandler serverH
  where
    toServantHandler = Nat (toServantHandler' initialState)
    initialState = ServerState (return [])
    serverH =    getNotes
            :<|> getNotesWithSearch
            :<|> getNote
            :<|> createNote

type Handler a = ReaderT ServerState (ExceptT Servant.ServantErr IO) a

getNotes :: Handler [Note]
getNotes = undefined

getNotesWithSearch :: Maybe String -> Handler [Note]
getNotesWithSearch = undefined

getNote :: Int -> Handler Note
getNote = undefined

createNote :: Note -> Handler ()
createNote = undefined
