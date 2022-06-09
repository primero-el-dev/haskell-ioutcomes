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
module Entity.Category where

import Data.Text
import Data.DateTime
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Database.Persist hiding (get)
import Database.Persist.Sqlite hiding (get)
import Database.Persist.TH

import Validation
import Entity.User

share [mkPersist sqlSettings, mkMigrate "migrateCategory"] [persistLowerCase|
Category json
    name Text sqltype=varchar(255)
    craetedAd DateTime Maybe default=CURRENT_TIME
    user UserId
    UniqueName name
    deriving Show
|]

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