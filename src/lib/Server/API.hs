module Server.API
    ( API
    ) where


import Servant

import Server.Types

type API
    =    "note" :> Get '[JSON] [Note]
    :<|> "note" :> QueryParam "like" String :> Get '[JSON] [Note]
    :<|> "note" :> Capture "noteId" Int :> Get '[JSON] Note
    :<|> "note" :> ReqBody '[JSON] Note :> Post '[JSON] ()
