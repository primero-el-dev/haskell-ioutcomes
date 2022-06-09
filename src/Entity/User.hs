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
module Entity.User where

import GHC.Generics
import Data.Text
import Data.DateTime
import Data.Aeson
import Data.ByteString
import Data.Text.Encoding
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Database.Persist hiding (get)
import Database.Persist.Sqlite hiding (get)
import Database.Persist.TH
import Database.Persist.Sqlite

import Validation


share [mkPersist sqlSettings, mkMigrate "migrateUser"] [persistLowerCase|
User
    email Text sqltype=varchar(255)
    password Text sqltype=varchar(255)
    createdAt DateTime Maybe default=CURRENT_TIME
    emailVerified Bool default=FALSE
    UniqueEmail email
    deriving Show
|]


email :: User -> Text
email (User email _ _ _) = email


password :: User -> Text
password (User _ password _ _) = password


createdAt :: User -> Maybe DateTime
createdAt (User _ _ createdAt _) = createdAt


emailVerified :: User -> Bool
emailVerified (User _ _ _ emailVerified) = emailVerified


makeUser :: Text -> Text -> User
makeUser email password =
    User email password Nothing True


instance FromJSON User where
    parseJSON (Object v) = makeUser
        <$> v .: "email"
        <*> v .: "password"

instance ToJSON User where
    toJSON (User email password createdAt emailVerified) = object
        [ "email" .= email
        , "createdAt" .= createdAt
        , "emailVerified" .= emailVerified
        ]

instance ToJSON (Entity User) where
    toJSON (Entity userId user) = toJSON user

instance ValidateEntity User where
    validateEntity = validateEntity_
        []
        [   ( "email"
            , email
            ,   [ (notBlank, "Email is erquired")
                , (minLength 7, "Email must be at least 7 characteres long")
                , (maxLength 150, "Email must be at most 150 characteres long")
                , (isEmail, "Given email is not valid")
                ]
            )
        ,   ( "password"
            , password
            ,   [ (notBlank, "Password is erquired")
                , (minLength 12, "Password must be at least 12 characteres long")
                , (maxLength 80, "Password must be at most 80 characteres long")
                ]
            )
        ]