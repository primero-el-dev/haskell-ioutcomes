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
module Entity.Token where

import Data.Text
import Data.String
import Data.DateTime
import Data.Aeson
import Data.Time.Clock
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Database.Persist hiding (get)
import Database.Persist.Sqlite hiding (get)
import Database.Persist.TH

import DateTime
import String
import Validation
import Entity.User


share [mkPersist sqlSettings, mkMigrate "migrateToken"] [persistLowerCase|
Token json
    type Text
    value Text sqltype=varchar(255)
    validTo DateTime
    userId UserId
    deriving Show
|]


data Type = RegistrationConfirmationType | AuthorizationType

instance Show Type where
    show RegistrationConfirmationType = "registration_confirmation"
    show AuthorizationType = "authorization"


userId :: Token -> UserId
userId (Token _ _ _ userId) =
    userId


createToken :: Int -> Type -> NominalDiffTime -> UserId -> IO Token
createToken len tokenType lifetimeInMinutes userId = do
    validTo <- dateMinutesFromNow lifetimeInMinutes
    value <- generateRandomAlphanumString len
    return (Token (fromString . show $ tokenType) (fromString value) validTo userId)


createRegistrationConfirmationToken :: UserId -> IO Token
createRegistrationConfirmationToken =
    createToken 80 RegistrationConfirmationType 20


createAuthorizationToken :: UserId -> IO Token
createAuthorizationToken =
    createToken 80 AuthorizationType 20


--instance ValidateEntity Token where
--    validateEntity = validateEntity_
--        []
--        [
--            ( "email"
--            , email
--            ,   [ (notBlank, "Email is erquired")
--                , (minLength 7, "Email must be at least 7 characteres long")
--                , (maxLength 150, "Email must be at most 150 characteres long")
--                ]
--            )
--        ,   ( "password"
--            , password
--            ,   [ (notBlank, "Password is erquired")
--                , (minLength 12, "Password must be at least 12 characteres long")
--                , (maxLength 80, "Password must be at most 80 characteres long")
--                ]
--            )
--        ]