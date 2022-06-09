{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Entity.Id where

import GHC.Generics
import Data.Aeson
import Data.Text
import Data.Scientific


data Id = Id Integer | Uuid String deriving (Generic, Show, Eq, FromJSON)

instance ToJSON Id where
    toJSON (Id id) = Data.Aeson.Number (scientific id 0)
    toJSON (Uuid id) = Data.Aeson.String . pack $ id

class Entity a

class GetId a where
    id :: a -> (Maybe Id)