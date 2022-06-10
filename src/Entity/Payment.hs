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
module Entity.Payment where

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
import qualified Data.ByteString.UTF8

import Entity.Common
import Entity.User
import Entity.Category
import Validation


share [mkPersist sqlSettings, mkMigrate "migratePayment"] [persistLowerCase|
Payment
    name Text sqltype=varchar(255)
    paymentType PaymentType sqltype=varchar(255)
    price Double sqltype=decimal(10,2)
    currency Currency sqltype=varchar(255)
    categoryId CategoryId Maybe
    createdAt DateTime Maybe default=CURRENT_TIME
    userId UserId Maybe
    deriving Show
|]

data PaymentType = Income | Outcome deriving (Generic, Show, Read, Eq, FromJSON, ToJSON, PersistFieldSql)

instance PersistField PaymentType where
    toPersistValue value = PersistText (Data.Text.pack . show $ value)
    fromPersistValue (PersistText paymentType) = Right (read . Data.Text.unpack $ paymentType)


data Currency = PLN | EUR | USD deriving (Generic, Show, Read, Eq, FromJSON, ToJSON, PersistFieldSql)

instance PersistField Currency where
    toPersistValue value = PersistText (Data.Text.pack . show $ value)
    fromPersistValue (PersistText currency) = Right (read . Data.Text.unpack $ currency)


name :: Payment -> Text
name (Payment name _ _ _ _ _ _) = name


paymentType :: Payment -> PaymentType
paymentType (Payment _ paymentType _ _ _ _ _) = paymentType


price :: Payment -> Double
price (Payment _ _ price _ _ _ _) = price


currency :: Payment -> Currency
currency (Payment _ _ _ currency _ _ _) = currency


categoryId :: Payment -> Maybe CategoryId
categoryId (Payment _ _ _ _ categoryId _ _) = categoryId


createdAt :: Payment -> Maybe DateTime
createdAt (Payment _ _ _ _ _ createdAt _) = createdAt


userId :: Payment -> Maybe UserId
userId (Payment _ _ _ _ _ _ userId) = userId


makePayment :: Text -> PaymentType -> Double -> Currency -> Maybe CategoryId -> Payment
makePayment name paymentType price currency categoryId =
    Payment name paymentType price currency categoryId Nothing Nothing


instance SetUserId Payment where
    setUserId (Payment name paymentType price currency categoryId createdAt _) userId =
        (Payment name paymentType price currency categoryId createdAt userId)


instance InitCreatedAt Payment where
    initCreatedAt (Payment name paymentType price currency categoryId _ userId) = do
        createdAt <- getCurrentTime
        return (Payment name paymentType price currency categoryId (Just createdAt) userId)

instance FromJSON Payment where
    parseJSON (Object v) = makePayment
        <$> v .: "name"
        <*> v .: "paymentType"
        <*> v .: "price"
        <*> v .: "currency"
        <*> v .: "categoryId"

instance ToJSON Payment where
    toJSON (Payment name paymentType price currency categoryId createdAt userId) = object
        [ "name" .= name
        , "paymentType" .= paymentType
        , "price" .= price
        , "currency" .= currency
        , "categoryId" .= categoryId
        , "createdAt" .= createdAt
        ]

instance CopyValues Payment where
    copyValues from to =
        Payment (Entity.Payment.name from) (Entity.Payment.paymentType from) (Entity.Payment.price from) (Entity.Payment.currency from) (Entity.Payment.categoryId from) (Entity.Payment.createdAt to) (Entity.Payment.userId to)

instance ToJSON (Entity Payment) where
    toJSON (Entity _ payment) = toJSON payment

instance ValidateEntity Payment where
    validateEntity = validateEntity_
        []
        [   ( "name"
            , Entity.Payment.name
            ,   [ (notBlank, "Payment name is required")
                , (maxLength 255, "Payment name must be at most 255 characteres long")
                ]
            )
        {-,   ( "price"
            , Entity.Payment.price
            ,   [ (minValue 0, "Payment price must be positive")
                , (maxValue 99999999.99, "Payment price is too big")
                ]
            )-}
        ]