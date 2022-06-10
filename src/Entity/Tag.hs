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
module Entity.Tag where

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


share [mkPersist sqlSettings, mkMigrate "migrateTag"] [persistLowerCase|
Tag
    name Text sqltype=varchar(255)
    createdAt DateTime Maybe default=CURRENT_TIME
    userId UserId Maybe
    deriving Show
|]


name :: Tag -> Text
name (Tag name _ _) = name


createdAt :: Tag -> Maybe DateTime
createdAt (Tag _ createdAt _) = createdAt


userId :: Tag -> Maybe UserId
userId (Tag _ _ userId) = userId


makeTag :: Text -> Tag
makeTag name =
    Tag name Nothing Nothing


instance InitCreatedAt Tag where
    initCreatedAt (Tag name _ userId) = do
        createdAt <- getCurrentTime
        return (Tag name (Just createdAt) userId)

instance FromJSON Tag where
    parseJSON (Object v) = makeTag
        <$> v .: "name"

instance ToJSON Tag where
    toJSON (Tag name createdAt userId) = object
        [ "name" .= name
        , "createdAt" .= createdAt
        ]

instance CopyValues Tag where
    copyValues from to =
        Tag (Entity.Tag.name from) (Entity.Tag.createdAt to) (Entity.Tag.userId to)

instance ToJSON (Entity Tag) where
    toJSON (Entity _ tag) = toJSON tag

instance ValidateEntity Tag where
    validateEntity = validateEntity_
        []
        [   ( "name"
            , Entity.Tag.name
            ,   [ (notBlank, "Tag name is erquired")
                , (maxLength 150, "Tag name must be at most 150 characteres long")
                ]
            )
        ]