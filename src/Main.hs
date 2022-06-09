{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}


{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
--{-# LANGUAGE InstanceSigs #-}

module Main where

import Data.Semigroup ((<>))
import Data.String
import Data.Text
import Data.DateTime
import Data.Traversable
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (forM_)
import Data.Text (Text)
import Data.IORef
import Web.Spock
import Web.Spock.Api hiding((<//>))
import Web.Spock.Config
import Control.Monad.Trans
import Data.Aeson.Types
import Network.URI
import Text.Megaparsec
import qualified Data.Text.Lazy.IO as TIO
import Data.Typeable
import Data.Time
import Data.ByteString.Char8
import Data.Text.Encoding
import Data.Char (ord)
import Data.Text.Lazy.IO as TL

import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import           Database.Persist        hiding (get) -- To avoid a naming clash with Web.Spock.get
import qualified Database.Persist        as P         -- We'll be using P.get later for GET /people/<id>.
import           Database.Persist.Sqlite hiding (get)
import           Database.Persist.TH

import Network.HTTP.Types.Status

--import Repository
--import Entity.Tag
--import Entity.Category
import Entity.User
import Entity.Token
import Entity.Tag
import Persistence
import Validation
import Mail
import Auth


type Api = SpockM SqlBackend () () ()

type ApiAction a = SpockAction SqlBackend () () a


runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn


errorJson :: Int -> Text -> ApiAction ()
errorJson code message =
    json $ object
        [ "result" .= String "failure"
        , "error" .= object ["code" .= code, "message" .= message]
        ]


entityErrorJson :: [(String, String)] -> ApiAction ()
entityErrorJson errors =
    json $ object
        [ "result" .= String "failure"
        , "errors" .= (object . entityErrorJsonPart $ errors)
        ]


entityErrorJsonPart :: [(String, String)] -> [Pair]
entityErrorJsonPart [] = []
entityErrorJsonPart ((fieldName, error) : errors) =
    [ fromString fieldName .= String (fromString error) ] <> entityErrorJsonPart errors


naked :: Entity a -> a
naked (Entity _ a) = a


getBearerFromByteString :: ByteString -> Text
getBearerFromByteString = (Data.Text.drop 7) . fromString . Data.ByteString.Char8.unpack


getBearerToken :: MonadIO m => ActionCtxT ctx m (Maybe Text)
getBearerToken = do
    bearerToken <- rawHeader (fromString "Authorization")
    case bearerToken of
        Nothing -> return Nothing
        Just token -> return (Just (getBearerFromByteString token))


getValidTokenUserId :: ActionCtxT () (WebStateM SqlBackend () ()) (Maybe UserId)
getValidTokenUserId = do
    maybeBearerToken <- getBearerToken
    case maybeBearerToken of
        Nothing -> return Nothing
        Just bearerToken -> do
            storedTokens <- runSQL $ selectList
                [ TokenType ==. (fromString . show $ AuthorizationType)
                , TokenValue ==. bearerToken
                ] [Asc TokenId]
            if Prelude.length storedTokens == 0
                then return Nothing
                else do
                    storedToken <- (return (storedTokens !! 0) >>= \token -> return token)
                    userId <- return . userId . naked $ storedToken
                    maybeUser <- runSQL $ P.get userId :: ApiAction (Maybe User)
                    case maybeUser of
                        Nothing -> return Nothing
                        Just user -> return (Just userId)


authorize :: ActionCtxT () (WebStateM SqlBackend () ()) () -> ActionCtxT () (WebStateM SqlBackend () ()) ()
authorize rest = do
    maybeUser <- getValidTokenUserId
    case maybeUser of
        Nothing -> jsonErrorAccessDenied
        Just user -> do rest


app :: Api
app = do

    post "register" $ do
        maybeUser <- jsonBody :: ApiAction (Maybe User)
        case maybeUser of
            Nothing -> errorJson 1 "Failed to parse request body as User"
            Just user ->
                if (Prelude.length . validateEntity $ user) > 0
                    then entityErrorJson . validateEntity $ user
                    else do
                        newId <- runSQL $ insert user
                        return (sendRegistrationConfirmationEmail [] (email user))
                        json $ object ["result" .= String "success"]

    post "login" $ do
        maybeUser <- jsonBody :: ApiAction (Maybe User)
        case maybeUser of
            Nothing -> errorJson 1 "Failed to parse request body as User"
            Just user -> do
                storedUser <- runSQL $ P.getBy (Entity.User.UniqueEmail (email user))
                case storedUser of
                    Nothing -> jsonErrorEntityNotFound "user"
                    Just u ->
                        if checkPassword u (password user)
                            then do
                                token <- liftIO . createAuthorizationToken $ (entityKey u)
                                runSQL $ deleteWhere
                                    [ TokenUserId ==. (entityKey u)
                                    , TokenType ==. (fromString . show $ AuthorizationType)
                                    ]
                                newId <- runSQL $ insert token
                                json token
                            else json $ object
                                [ "result" .= String "failure"
                                , "error" .= String "Wrong email or password"
                                ]

    get "users" $ do
        allUsers <- runSQL $ selectList [] [Asc UserId]
        bearerToken <- getBearerToken
        case bearerToken of
            Nothing -> errorJson 1 "Failed to find stored Token"
            Just token -> json token

--    get "people" $ do
--        allPeople <- runSQL $ selectList [] [Asc PersonId]
--        json allPeople

--    get ("tags" <//> var) $ \tagId -> do
--        maybeTag <- runSQL $ P.get tagId :: ApiAction (Maybe Tag)
--        case maybeTag of
--            Nothing -> jsonErrorEntityNotFound "tag"
--            Just tag -> json thePerson


    post "tags" $ do
        maybeUserId <- getValidTokenUserId
        case maybeUserId of
            Nothing -> jsonErrorAccessDenied
            Just userId -> do
                maybeTag <- jsonBody :: ApiAction (Maybe Tag)
                case maybeTag of
                    Nothing -> errorJson 1 "Failed to parse request body as Tag"
                    Just tag -> do
                        tagToStore <- return (Tag (Entity.Tag.name tag) Nothing (Just userId))
                        newId <- runSQL $ insert tagToStore
                        json $ object ["result" .= String "success", "id" .= newId]


--    post "tags" $ do
--        maybeBearerToken <- getBearerToken
--        case maybeBearerToken of
--            Nothing -> jsonErrorAccessDenied
--            Just bearerToken -> do
--                storedTokens <- runSQL $ selectList
--                    [ TokenType ==. (fromString . show $ AuthorizationType)
--                    , TokenValue ==. bearerToken
--                    ] [Asc TokenId]
--                if Prelude.length storedTokens == 0
--                    then jsonErrorAccessDenied
--                    else do
--                        storedToken <- (return (storedTokens !! 0) >>= \token -> return token)
--                        userId <- return . userId . naked $ storedToken
--                        maybeUser <- runSQL $ P.get userId :: ApiAction (Maybe User)
--                        case maybeUser of
--                            Nothing -> jsonErrorAccessDenied
--                            Just user -> do
--                                maybeTag <- jsonBody :: ApiAction (Maybe Tag)
--                                case maybeTag of
--                                    Nothing -> errorJson 1 "Failed to parse request body as Tag"
--                                    Just tag -> do
--                                        tagToStore <- return (Tag (Entity.Tag.name tag) Nothing (Just userId))
--                                        newId <- runSQL $ insert tagToStore
--                                        json $ object ["result" .= String "success", "id" .= newId]

--        maybeTag <- jsonBody :: ApiAction (Maybe Tag)
--        case maybeTag of
--            Nothing -> errorJson 1 "Failed to parse request body as Tag"
--            Just tag ->
--                if (Prelude.length . validate $ tag) > 0
--                    then entityErrorJson . validatePerson $ tag
--                    else do
--                        newId <- runSQL $ insert tag
--                        json $ object ["result" .= String "success", "id" .= newId]

--    put ("people" <//> var) $ \personId -> do
--        maybePerson <- jsonBody :: ApiAction (Maybe Person)
--        maybePersonStored <- runSQL $ P.get personId :: ApiAction (Maybe Person)
--        case maybePersonStored of
--            Nothing -> jsonErrorEntityNotFound "person"
--            Just storedPerson ->
--                case maybePerson of
--                    Nothing -> errorJson 1 "Failed to parse request body as Person"
--                    Just thePerson ->
--                        if (Prelude.length . validatePerson $ thePerson) > 0
--                            then entityErrorJson . validatePerson $ thePerson
--                            else do
--                                runSQL $ Database.Persist.Sqlite.replace personId thePerson
--                                json thePerson
--
--    Web.Spock.delete ("people" <//> var) $ \personId -> do
--        maybePerson <- runSQL $ P.get personId :: ApiAction (Maybe Person)
--        case maybePerson of
--            Nothing -> jsonErrorEntityNotFound "person"
--            Just thePerson -> do
--                runSQL $ Database.Persist.Sqlite.delete personId
--                json $ object ["result" .= String "success", "id" .= personId]


--jsonErrorEntityNotFound :: (MonadIO m) => String -> ActionCtxT ctx m b
jsonErrorEntityNotFound entityName = do
    setStatus status404
    errorJson 2 . fromString $ "Could not find a " ++ entityName ++ " with matching id"


jsonErrorAccessDenied = do
    setStatus status403
    errorJson 2 . fromString $ "Access denied"


stringifyErrors :: [(String, String)] -> String
stringifyErrors [] = ""
stringifyErrors ((fieldName, error) : rest) =
    fieldName ++ ": " ++ error ++ stringifyErrors rest


main :: IO ()
main = do
--    emailSend <- sendRegistrationConfirmationEmail [] (fromString "1234567890localhost@gmail.com")
--    putStrLn "Done"
    pool <- runStdoutLoggingT $ createSqlitePool "database/api.db" 5
    config <- defaultSpockCfg () (PCPool pool) ()
    runStdoutLoggingT $ runSqlPool (do
        runMigrationUnsafe migrateUser
        runMigrationUnsafe migrateToken
        runMigrationUnsafe migrateTag) pool
    runSpock 8080 (spock config app)
