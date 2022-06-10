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
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Database.Persist hiding (get)
import qualified Database.Persist as P
import Database.Persist.Sqlite hiding (get)
import Database.Persist.TH
import Network.HTTP.Types.Status

import Entity.Common
import Entity.User
import Entity.Token
import Entity.Tag
import Entity.Category
import Entity.Payment
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
            currentTime <- liftIO $ Data.DateTime.getCurrentTime
            storedTokens <- runSQL $ selectList
                [ TokenType ==. (fromString . show $ AuthorizationType)
                , TokenValue ==. bearerToken
                , TokenValidTo >=. currentTime
                ] [Asc TokenId]
            if Prelude.length storedTokens == 0
                then return Nothing
                else do
                    storedToken <- (return (storedTokens !! 0) >>= \token -> return token)
                    userId <- return . Entity.Token.userId . naked $ storedToken
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
                        user <- liftIO . initCreatedAt $ user
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


    get "tags" $ do
        maybeUserId <- getValidTokenUserId
        case maybeUserId of
            Nothing -> jsonErrorAccessDenied
            Just userId -> do
                tags <- runSQL $ selectList [ TagUserId ==. (Just userId) ] [Asc TagId]
                json tags

    get ("tags" <//> var) $ \tagId -> do
        maybeUserId <- getValidTokenUserId
        case maybeUserId of
            Nothing -> jsonErrorAccessDenied
            Just userId -> do
                maybeTag <- runSQL $ P.get tagId :: ApiAction (Maybe Tag)
                case maybeTag of
                    Nothing -> jsonErrorEntityNotFound "tag"
                    Just tag -> json tag

    post "tags" $ do
        maybeUserId <- getValidTokenUserId
        case maybeUserId of
            Nothing -> jsonErrorAccessDenied
            Just userId -> do
                maybeTag <- jsonBody :: ApiAction (Maybe Tag)
                case maybeTag of
                    Nothing -> errorJson 1 "Failed to parse request body as Tag"
                    Just tag -> do
                        tag2 <- liftIO . initCreatedAt $ tag
                        tagToStore <- return (Tag (Entity.Tag.name tag2) (Entity.Tag.createdAt tag2) (Just userId))
                        newId <- runSQL $ insert tagToStore
                        json $ object ["result" .= String "success", "id" .= newId]


    put ("tags" <//> var) $ \tagId -> do
        maybeUserId <- getValidTokenUserId
        case maybeUserId of
            Nothing -> jsonErrorAccessDenied
            Just userId -> do
                maybeTagStored <- runSQL $ P.get tagId :: ApiAction (Maybe Tag)
                case maybeTagStored of
                    Nothing -> jsonErrorEntityNotFound "tag"
                    Just storedTag -> do
                        maybeTag <- jsonBody :: ApiAction (Maybe Tag)
                        case maybeTag of
                            Nothing -> errorJson 1 "Failed to parse request body as Tag"
                            Just tag ->
                                if (Prelude.length . validateEntity $ tag) > 0
                                    then entityErrorJson . validateEntity $ tag
                                    else do
                                        newTag <- return (copyValues tag storedTag)
                                        runSQL $ Database.Persist.Sqlite.replace tagId newTag
                                        json newTag

    Web.Spock.delete ("tags" <//> var) $ \tagId -> do
        maybeUserId <- getValidTokenUserId
        case maybeUserId of
            Nothing -> jsonErrorAccessDenied
            Just userId -> do
                maybeTag <- runSQL $ P.get tagId :: ApiAction (Maybe Tag)
                case maybeTag of
                    Nothing -> jsonErrorEntityNotFound "tag"
                    Just tag -> do
                        runSQL $ Database.Persist.Sqlite.delete tagId
                        json $ object ["result" .= String "success", "id" .= tagId]


    get "categories" $ do
        maybeUserId <- getValidTokenUserId
        case maybeUserId of
            Nothing -> jsonErrorAccessDenied
            Just userId -> do
                categories <- runSQL $ selectList [ CategoryUserId ==. (Just userId) ] [Asc CategoryId]
                json categories

    get ("categories" <//> var) $ \categoryId -> do
        maybeUserId <- getValidTokenUserId
        case maybeUserId of
            Nothing -> jsonErrorAccessDenied
            Just userId -> do
                maybeCategory <- runSQL $ P.get categoryId :: ApiAction (Maybe Category)
                case maybeCategory of
                    Nothing -> jsonErrorEntityNotFound "category"
                    Just category -> json category

    post "categories" $ do
        maybeUserId <- getValidTokenUserId
        case maybeUserId of
            Nothing -> jsonErrorAccessDenied
            Just userId -> do
                maybeCategory <- jsonBody :: ApiAction (Maybe Category)
                case maybeCategory of
                    Nothing -> errorJson 1 "Failed to parse request body as Category"
                    Just category -> do
                        category2 <- liftIO . initCreatedAt $ category
                        categoryToStore <- return (Category (Entity.Category.name category2) (Entity.Category.createdAt category2) (Just userId))
                        newId <- runSQL $ insert categoryToStore
                        json $ object ["result" .= String "success", "id" .= newId]


    put ("categories" <//> var) $ \categoryId -> do
        maybeUserId <- getValidTokenUserId
        case maybeUserId of
            Nothing -> jsonErrorAccessDenied
            Just userId -> do
                maybeCategoryStored <- runSQL $ P.get categoryId :: ApiAction (Maybe Category)
                case maybeCategoryStored of
                    Nothing -> jsonErrorEntityNotFound "category"
                    Just storedCategory -> do
                        maybeCategory <- jsonBody :: ApiAction (Maybe Category)
                        case maybeCategory of
                            Nothing -> errorJson 1 "Failed to parse request body as Category"
                            Just category ->
                                if (Prelude.length . validateEntity $ category) > 0
                                    then entityErrorJson . validateEntity $ category
                                    else do
                                        newCategory <- return (copyValues category storedCategory)
                                        runSQL $ Database.Persist.Sqlite.replace categoryId newCategory
                                        json newCategory

    Web.Spock.delete ("categories" <//> var) $ \categoryId -> do
        maybeUserId <- getValidTokenUserId
        case maybeUserId of
            Nothing -> jsonErrorAccessDenied
            Just userId -> do
                maybeCategory <- runSQL $ P.get categoryId :: ApiAction (Maybe Category)
                case maybeCategory of
                    Nothing -> jsonErrorEntityNotFound "category"
                    Just category -> do
                        runSQL $ Database.Persist.Sqlite.delete categoryId
                        json $ object ["result" .= String "success", "id" .= categoryId]

    get "payments" $ do
        maybeUserId <- getValidTokenUserId
        case maybeUserId of
            Nothing -> jsonErrorAccessDenied
            Just userId -> do
                payments <- runSQL $ selectList [ PaymentUserId ==. (Just userId) ] [Asc PaymentId]
                json payments

    post "payments" $ do
        maybeUserId <- getValidTokenUserId
        case maybeUserId of
            Nothing -> jsonErrorAccessDenied
            Just userId -> do
                maybePayment <- jsonBody :: ApiAction (Maybe Payment)
                case maybePayment of
                    Nothing -> errorJson 1 "Failed to parse request body as Payment"
                    Just payment -> do
                        case Entity.Payment.categoryId payment of
                            Nothing -> do
                                payment2 <- liftIO . initCreatedAt $ setUserId payment (Just userId)
                                newId <- runSQL $ insert payment2
                                json $ object ["result" .= String "success", "id" .= newId]
                            Just categoryId -> do
                                maybeCategory <- runSQL $ P.get categoryId :: ApiAction (Maybe Category)
                                case maybeCategory of
                                    Nothing -> jsonErrorEntityNotFound "category"
                                    Just category -> do
                                        case Entity.Category.userId category of
                                            Nothing -> jsonErrorEntityNotFound "category"
                                            Just categoryUserId -> do
                                                if categoryUserId /= userId
                                                    then jsonErrorEntityNotFound "category"
                                                    else do
                                                        payment2 <- liftIO . initCreatedAt $ setUserId payment (Just userId)
                                                        newId <- runSQL $ insert payment2
                                                        json $ object ["result" .= String "success", "id" .= newId]

    put ("payments" <//> var) $ \paymentId -> do
        maybeUserId <- getValidTokenUserId
        case maybeUserId of
            Nothing -> jsonErrorAccessDenied
            Just userId -> do
                maybePaymentStored <- runSQL $ P.get paymentId :: ApiAction (Maybe Payment)
                case maybePaymentStored of
                    Nothing -> jsonErrorEntityNotFound "payment"
                    Just storedPayment -> do
                        maybePayment <- jsonBody :: ApiAction (Maybe Payment)
                        case maybePayment of
                            Nothing -> errorJson 1 "Failed to parse request body as Payment"
                            Just payment -> do
                                case Entity.Payment.categoryId payment of
                                    Nothing -> do
                                        newPayment <- return (copyValues payment storedPayment)
                                        runSQL $ Database.Persist.Sqlite.replace paymentId newPayment
                                        json newPayment
                                    Just categoryId -> do
                                        maybeCategory <- runSQL $ P.get categoryId :: ApiAction (Maybe Category)
                                        case maybeCategory of
                                            Nothing -> jsonErrorEntityNotFound "category"
                                            Just category -> do
                                                case Entity.Category.userId category of
                                                    Nothing -> jsonErrorEntityNotFound "category"
                                                    Just categoryUserId -> do
                                                        if categoryUserId /= userId
                                                            then jsonErrorEntityNotFound "category"
                                                            else do
                                                                newPayment <- return (copyValues payment storedPayment)
                                                                runSQL $ Database.Persist.Sqlite.replace paymentId newPayment
                                                                json newPayment

    Web.Spock.delete ("payments" <//> var) $ \paymentId -> do
        maybeUserId <- getValidTokenUserId
        case maybeUserId of
            Nothing -> jsonErrorAccessDenied
            Just userId -> do
                maybePayment <- runSQL $ P.get paymentId :: ApiAction (Maybe Payment)
                case maybePayment of
                    Nothing -> jsonErrorEntityNotFound "payment"
                    Just payment -> do
                        runSQL $ Database.Persist.Sqlite.delete paymentId
                        json $ object ["result" .= String "success", "id" .= paymentId]


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

{-
monadicLoop
    :: (Migration -> ReaderT SqlBackend m ())
    -> [Migration]
    -> ReaderT SqlBackend (LoggingT IO) a2
monadicLoop fun [] = return
monadicLoop fun (x:xs) = do
    fun x
    monadicLoop fun xs
-}

main :: IO ()
main = do
--    emailSend <- sendRegistrationConfirmationEmail [] (fromString "1234567890localhost@gmail.com")
--    putStrLn "Done"
    pool <- runStdoutLoggingT $ createSqlitePool "database/api.db" 5
    config <- defaultSpockCfg () (PCPool pool) ()
    runStdoutLoggingT $ runSqlPool (do
        runMigrationUnsafe migrateUser
        runMigrationUnsafe migrateToken
        runMigrationUnsafe migrateTag
        runMigrationUnsafe migrateCategory
        runMigrationUnsafe migratePayment) pool
    runSpock 8081 (spock config app)
