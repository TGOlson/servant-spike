module Server
    ( main
    ) where

import Control.Concurrent.STM
import Data.List
import Data.Proxy
import qualified Servant
import Network.Wai
import Network.Wai.Handler.Warp

import Server.API
import Server.Types


main :: IO ()
main = do
    st <- ServerState <$> makeServerDataStore
    run 8000 (app st)


app :: ServerState -> Application
app st = Servant.serve (Proxy :: Proxy API) (server st)


makeServerDataStore :: IO ServerDataStore
makeServerDataStore = do
    var <- newTVarIO
        [ Note 1 "Hello"
        , Note 2 "Hello Again!"
        , Note 3 "Foo"
        ]

    let findNote i = find ((==) i . nId)
        sdsGet = atomically $ readTVar var
        sdsFind nId = findNote nId <$> sdsGet
        sdsPut note = atomically . modifyTVar var $ \notes ->
            maybe (note:notes) (const notes) (findNote (nId note) notes)
        sdsRemove i = atomically . modifyTVar var $ filter ((/=) i . nId)

    return ServerDataStore{..}
