module AgentProfile where

import Prelude

import Data.Maybe
import Data.Either
import Data.Show
import Data.Argonaut (jsonParser, class EncodeJson, class DecodeJson, Json, decodeJson, (.?), (:=), (~>), jsonEmptyObject)
import Data.Argonaut.Decode ((.??))
import Data.Array (filter)
import Data.Traversable (traverse)
import Data.Generic.Rep
import Data.Generic.Rep.Show

class WithName a where
  getName :: a -> String
newtype User = User {
  name :: String
  , email :: String
  }

derive instance genericUser :: Generic User _
instance showUser :: Show User where
  show = genericShow

instance decodeUser :: DecodeJson User where
  decodeJson json = do
    obj <- decodeJson json
    name <- obj .? "name"
    email <- obj .? "email"
    pure $ User { name, email }

instance userName :: WithName User where
  getName (User { name }) = name

instance encodeUser :: EncodeJson User where
  encodeJson (User user)
     = "name" := user.name
    ~> "email" := user.email
    ~> jsonEmptyObject

newtype Organisation = Organisation {
  name :: String
  , users :: Array User
  }

derive instance genericOrg :: Generic Organisation _
instance showOrg :: Show Organisation where
  show = genericShow

instance orgName :: WithName Organisation where
  getName (Organisation { name }) = name

instance decodeOrganisation :: DecodeJson Organisation where
  decodeJson json = do
    obj <- decodeJson json
    name <- obj .? "name"
    users <- obj .? "users" >>= traverse decodeJson
    pure $ Organisation { name, users }

newtype AgentProfile = AgentProfile {
  organisation :: Maybe Organisation
  , name :: String
  }

derive instance genericAP :: Generic AgentProfile _
instance showAP :: Show AgentProfile where
  show = genericShow

instance apName :: WithName AgentProfile where
  getName (AgentProfile { name }) = name

instance decodeAgentProfile :: DecodeJson AgentProfile where
  decodeJson json = do
    obj <- decodeJson json
    name <- obj .? "name"
    organisation <- obj .?? "organisation"
    pure $ AgentProfile { name, organisation }

newtype ImportFile = ImportFile {
    users :: Maybe User
    , organisations :: Maybe Organisation
    , agentProfiles :: Maybe AgentProfile
  }

derive instance genericImport :: Generic ImportFile _
instance showImport :: Show ImportFile where
  show = genericShow

instance decodeImportFile :: DecodeJson ImportFile where
  decodeJson json = do
     obj <- decodeJson json
     users <- obj .?? "users" >>= traverse decodeJson
     organisations <- obj .?? "organisations"
     agentProfiles <- obj .?? "agentProfiles"
     pure $ ImportFile { users, organisations, agentProfiles }

eguser :: String
eguser = """
  {"name": "Jo", "email": "a@b.c" }
"""

egusers :: String
egusers = "[" <> eguser <> "," <> eguser <> "]"

egorg :: String
egorg = """
  {
    "name": "My Org",
    "users": [
      { "name": "a", "email": "b@c.d" }
      , { "name": "a", "email": "b@c.d" }
    ]
  }
"""

egap :: String
egap = """
  {
    "name": "AP"
  }
"""

asOrg :: Organisation -> Organisation
asOrg = identity

asAp :: AgentProfile -> AgentProfile
asAp = identity
