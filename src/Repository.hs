module Repository where

import Database.PostgreSQL.Simple

import Entity.Id


data Status = Success | Failure


connection :: IO Connection
connection = connect defaultConnectInfo
    { connectHost = "127.0.0.1"
    , connectDatabase = "ioutcomes"
    , connectUser = "ioutcomes"
    , connectPassword = "password"
    }


class (GetId entity) => Find entity where
    find :: [entity] -> Id -> Maybe entity
    find [] _ = Nothing
    find (e:es) i =
        case Entity.Id.id e of
            Nothing -> find es i
            Just id' ->
                if i == id'
                    then Just e
                    else find es i



--class FindAll entity where
--    findAll :: [entity] -> [entity]
--    findAll [] = []
--    findAll (e:es) getter value =
--        if getter e == value
--            then Just e
--            else findOneBy es getter value


class FindOneBy entity where
    findOneBy :: (Eq eq) => [entity] -> (entity -> eq) -> eq -> Maybe entity
    findOneBy [] _ _ = Nothing
    findOneBy (e:es) getter value =
        if getter e == value
            then Just e
            else findOneBy es getter value


class FindBy entity where
    findBy :: (Eq eq) => [entity] -> (entity -> eq) -> eq -> [entity]
    findBy [] _ _ = []
    findBy (e:es) getter value =
        if getter e == value
            then (e : findBy es getter value)
            else findBy es getter value


class Insert entity where
    insert :: (FromRow r) => IO Connection -> entity -> IO [r]


class Update entity where
    update :: (FromRow r) => IO Connection -> entity -> IO [r]


class Delete entity where
    delete :: (FromRow r) => IO Connection -> entity -> IO [r]

