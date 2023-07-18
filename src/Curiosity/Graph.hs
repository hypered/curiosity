module Curiosity.Graph
  ( graphDot
  , graphSvg
  ) where

import qualified Curiosity.Data                as Data
import qualified Curiosity.Types.Business      as Business
import qualified Curiosity.Types.Legal         as Legal
import qualified Curiosity.Types.User          as User
import qualified Data.Text                     as T
import           System.Process                 ( readProcess )


--------------------------------------------------------------------------------
graphDot :: Data.HaskDb -> [Text]
graphDot state' =
  [ "// Generated by `cty graph`." ]
  <> header
  <> userNodes state'
  <> unitNodes state'
  <> entityNodes state'
  <> entityUsersNodes state'
  <> unitHoldersNodes state'
  <> footer


--------------------------------------------------------------------------------
header :: [Text]
header =
  [ "digraph hello {"
  , "  graph ["
  , "    label = \"state\\n\\n\""
  , "    labelloc = t"
  , "    fontname = \"Helvetica,Arial,sans-serif\""
  , "    fontsize = 20"
  , "    layout = dot"
  , "    rankdir = LR"
  , "    newrank = true"
  , "  ]"
  , "  node ["
  , "    style=filled"
  , "    shape=rect"
  , "    pencolor=\"#eeeeee\" // node border"
  , "    fontname=\"Helvetica,Arial,sans-serif\""
  , "    shape=plaintext"
  , "  ]"
  , "  edge ["
  , "    arrowsize=0.5"
  , "    fontname=\"Helvetica,Arial,sans-serif\""
  , "    labeldistance=3"
  , "    labelfontcolor=\"#cccccc\""
  , "    penwidth=2"
  , "    style=dashed"
  , "  ]"
  ]

footer :: [Text]
footer = [ "}" ]


--------------------------------------------------------------------------------
userNodes :: Data.HaskDb -> [Text]
userNodes state' = 
  concatMap userNode $ reverse userProfiles
 where
  Identity userProfiles = Data._dbUserProfiles state'

userNode :: User.UserProfile -> [Text]
userNode User.UserProfile {..} =
  [ userId' <> " ["
  , "  color=\"#ffffff\""
  , "  label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\" cellpadding=\"4\">"
  , "    <tr> <td> <b>" <> username <> "</b><br/>" <> userId <> "</td> </tr>"
  , "  </table>>"
  , "  shape=plain"
  , "]"
  ]
 where
  userId = User.unUserId _userProfileId
  userId' = T.filter (/= '-') userId
  username = User.unUserName $ User._userCredsName _userProfileCreds


--------------------------------------------------------------------------------
entityNodes :: Data.HaskDb -> [Text]
entityNodes state' = 
  concatMap entityNode entities
 where
  Identity entities = Data._dbLegalEntities state'

entityNode :: Legal.Entity -> [Text]
entityNode Legal.Entity {..} =
  [ "" <> slug <> " ["
  , "  color=\"#ffffff\""
  , "  label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\" cellpadding=\"4\">"
  , "    <tr> <td> <b>" <> slug <> "</b><br/>" <> id <> "</td> </tr>"
  ]
  <> supervised
  <> host <>
  [ "  </table>>"
  , "  shape=plain"
  , "]"
  ]
 where
  id = Legal.unEntityId _entityId
  slug = _entitySlug
  supervised =
    if _entityIsSupervised
    then [ "    <tr> <td align=\"left\">Supervised</td> </tr>" ]
    else []
  host =
    if _entityIsHost
    then [ "    <tr> <td align=\"left\">Host</td> </tr>" ]
    else []

entityUsersNodes :: Data.HaskDb -> [Text]
entityUsersNodes state' = 
  concatMap entityUsersNode entities
 where
  Identity entities = Data._dbLegalEntities state'

entityUsersNode :: Legal.Entity -> [Text]
entityUsersNode entity@Legal.Entity {..} =
  concatMap (entityUserNode entity) _entityUsersAndRoles

entityUserNode :: Legal.Entity -> Legal.ActingUserId -> [Text]
entityUserNode Legal.Entity {..} (Legal.ActingUserId userId role) =
  [ userId' <> " -> " <> slug <> " [label=\"" <> label <> "\" color=\"#bbbbbb\"]" ]
 where
  userId' = T.filter (/= '-') $ User.unUserId userId
  slug = _entitySlug
  label = T.unwords [ "is" , T.toLower (show role) , "in" ] 


--------------------------------------------------------------------------------
unitNodes :: Data.HaskDb -> [Text]
unitNodes state' = 
  concatMap unitNode $ reverse units
 where
  Identity units = Data._dbBusinessUnits state'

unitNode :: Business.Unit -> [Text]
unitNode Business.Unit {..} =
  [ "" <> slug <> " ["
  , "  color=\"#ffffff\""
  , "  label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\" cellpadding=\"4\">"
  , "    <tr> <td> <b>" <> slug <> "</b><br/>" <> id <> "</td> </tr>"
  , "  </table>>"
  , "  shape=plain"
  , "]"
  ]
 where
  id = Business.unUnitId _entityId
  slug = _entitySlug

unitHoldersNodes :: Data.HaskDb -> [Text]
unitHoldersNodes state' = 
  concatMap unitHoldersNode units
 where
  Identity units = Data._dbBusinessUnits state'

unitHoldersNode :: Business.Unit -> [Text]
unitHoldersNode unit@Business.Unit {..} =
  concatMap (unitHolderNode unit) _entityHolders

unitHolderNode :: Business.Unit -> User.UserId -> [Text]
unitHolderNode Business.Unit {..} userId =
  [ userId' <> " -> " <> slug <> " [label=\"" <> label <> "\" color=\"#bbbbbb\"]" ]
 where
  userId' = T.filter (/= '-') $ User.unUserId userId
  slug = _entitySlug
  label = "is holder of"


--------------------------------------------------------------------------------
graphSvg :: Data.HaskDb -> IO Text
graphSvg state' = T.pack <$> readProcess "dot" ["-Tsvg"] input
 where
  input = T.unpack . unlines $ graphDot state'
