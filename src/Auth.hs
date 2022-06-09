module Auth where

import Data.Text
import qualified Database.Persist as P
import Database.Persist.Sqlite hiding (get)

import Entity.User


checkPassword :: Entity User -> Text -> Bool
checkPassword (Entity _ (User _ pass _ _)) password =
    pass == password


--getByToken :: Text -> Maybe User
--getByToken token =



--authUser :: Text -> Text -> Entity User
--authUser email pass = do
--        user <- getBy (UniqueEmail email) :: Entity User
--        user
--        user <- getBy (UniqueEmail email)
--        if (password user) == pass
--            then (Just (Entity user))
--            else Nothing

--        users <- runSQL $ selectList [Entity.User.UserEmail ==. (email user)] []
--        users <- selectList [Entity.User.UserEmail email] []
--        if Prelude.length users > 0
--            then (Just (users !! 0))
--            else Nothing