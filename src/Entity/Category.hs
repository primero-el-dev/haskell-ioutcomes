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

import Entity.Common
import Entity.User
import Validation


share [mkPersist sqlSettings, mkMigrate "migrateCategory"] [persistLowerCase|
Category
    name Text sqltype=varchar(255)
    createdAt DateTime Maybe default=CURRENT_TIME
    userId UserId Maybe
    deriving Show
|]


name :: Category -> Text
name (Category name _ _) = name


createdAt :: Category -> Maybe DateTime
createdAt (Category _ createdAt _) = createdAt


userId :: Category -> Maybe UserId
userId (Category _ _ userId) = userId


makeCategory :: Text -> Category
makeCategory name =
    Category name Nothing Nothing


instance InitCreatedAt Category where
    initCreatedAt (Category name _ userId) = do
        createdAt <- getCurrentTime
        return (Category name (Just createdAt) userId)

instance FromJSON Category where
    parseJSON (Object v) = makeCategory
        <$> v .: "name"

instance ToJSON Category where
    toJSON (Category name createdAt userId) = object
        [ "name" .= name
        , "createdAt" .= createdAt
        ]

instance CopyValues Category where
    copyValues from to =
        Category (Entity.Category.name from) (Entity.Category.createdAt to) (Entity.Category.userId to)

instance ToJSON (Entity Category) where
    toJSON (Entity _ category) = toJSON category

instance ValidateEntity Category where
    validateEntity = validateEntity_
        []
        [   ( "name"
            , Entity.Category.name
            ,   [ (notBlank, "Category name is required")
                , (maxLength 150, "Category name must be at most 150 characteres long")
                ]
            )
        ]