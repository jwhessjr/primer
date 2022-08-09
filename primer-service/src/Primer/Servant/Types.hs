-- | Common types shared across both APIs.
module Primer.Servant.Types (
  CopySession,
  CreateSession,
  GetSessionList,
  GetSessionName,
  GetVersion,
  SetSessionName,
) where

import Foreword

import Primer.Database (
  Session,
  SessionId,
 )
import Primer.Pagination (
  Paginated,
  PaginationParams,
 )
import Servant (
  Description,
  Get,
  JSON,
  Post,
  Put,
  QueryFlag,
  ReqBody,
  Summary,
  (:>),
 )
import Servant.API.Generic (
  GenericMode ((:-)),
 )
import Servant.OpenApi.OperationId (OpId)

-- Note: the 'OpId' types in the following signatures are only used by
-- the OpenAPI API, but they're harmless in the Servant API.

type CopySession mode =
  mode
    :- "copy-session"
      :> Summary "Copy a session to a new session"
      :> Description
          "Copy the session whose ID is given in the request body to a \
          \new session, and return the new session's ID. Note that this \
          \method can be called at any time and is not part of the \
          \session-specific API, as it's not scoped by the current \
          \session ID like those methods are."
      :> ReqBody '[JSON] SessionId
      :> OpId "copySession" Post '[JSON] SessionId

type GetVersion mode =
  mode
    :- "version"
      :> Summary "Get the current server version"
      :> OpId "getVersion" Get '[JSON] Text

type CreateSession mode =
  mode
    :- Summary "Create a new session and return its ID"
      :> OpId "createSession" Post '[JSON] SessionId

type GetSessionList mode =
  mode
    :- QueryFlag "inMemory"
      :> PaginationParams
      :> Summary "Get the list of sessions"
      :> Description
          "Get a list of all sessions and their human-readable names. By \
          \default, this method returns the list of all sessions in the \
          \persistent database, but optionally it can return just the list \
          \of all sessions in memory, which is mainly useful for \
          \testing. Note that in a production system, this endpoint should \
          \obviously be authentication-scoped and only return the list of \
          \sessions that the caller is authorized to see."
      :> OpId "getSessionList" Get '[JSON] (Paginated Session)

type GetSessionName mode =
  mode
    :- "name"
      :> Summary "Get the specified session's name"
      :> OpId "getSessionName" Get '[JSON] Text

type SetSessionName mode =
  mode
    :- "name"
      :> Summary "Set the specified session's name"
      :> Description
          "Attempt to set the current session name. Returns the actual \
          \new session name. (Note that this may differ from the name \
          \provided.)"
      :> ReqBody '[JSON] Text
      :> OpId "setSessionName" Put '[JSON] Text