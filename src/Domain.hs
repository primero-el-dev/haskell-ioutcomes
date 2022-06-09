module Domain where

import Data.Dates


data Id = Uuid String | Id Int

data Category = Category Id String

data Tag = Tag Id String

type Tags = [Tag]

data Currency = EUR | PLN | USD deriving (Show)

data Money = Money Float Currency

data User = User Id String

data PaymentType = Income | Outcome deriving (Show)

data Payment = Payment Id PaymentType String User Money DateTime (Maybe Category) [Tag]


instance Show Money where
    show (Money value currency) = show value ++ " " ++ show currency

instance Show Id where
    show (Id id) = show id
    show (Uuid id) = id


class GetId a where
    getId :: a -> Id

instance GetId User where
    getId (User id _) = id

instance GetId Category where
    getId (Category id _) = id

instance GetId Tag where
    getId (Tag id _) = id

instance GetId Payment where
    getId (Payment id _ _ _ _ _ _ _) = id


class GetName a where
    getName :: a -> String

instance GetName Category where
    getName (Category _ name) = name

instance GetName Tag where
    getName (Tag _ name) = name

instance GetName User where
    getName (User _ name) = name

instance GetName Payment where
    getName (Payment _ _ name _ _ _ _ _) = name


getNames :: Tags -> String
getNames [] = ""
getNames (x:[]) = getName x
getNames (x:xs) = getName x ++ ", " ++ getNames xs


--class Convert a where
--    convert :: a -> b -> a

--instance Convert Money Currency where
--    convert (Money value EUR) PLN = Money (3.2 * value) PLN

