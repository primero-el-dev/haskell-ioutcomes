{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE ExtendedDefaultRules #-}
module Main where

import Data.Semigroup ((<>))
import Data.String
import Data.Text
import Data.Traversable
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_)
import Data.Text (Text)
import Data.IORef
import Web.Spock
import Web.Spock.Config
import Web.Spock.Lucid (lucid)
import Lucid

import Data.Aeson
import Text.Megaparsec
import Text.Mustache
import qualified Data.Text.Lazy.IO as TIO

import Domain


data ServerState = ServerState { payments :: IORef [Payment] }

type Server a = SpockM () () ServerState a


printPayment :: Payment -> Html ()
printPayment (Payment id paymentType name user money dateTime category tags) = do
    h3_ $ fromString $ "Name: " ++ name
    p_ $ fromString $ show paymentType
    p_ $ fromString $ "Name: " ++ name
    p_ $ fromString $ "User: " ++ getName user
    p_ $ fromString $ "Value: " ++ (show money)
    p_ $ fromString $ "Date: " ++ (show dateTime)
    p_ $ fromString $ "Category: " ++ (getMaybeName category)
    p_ $ fromString $ "Tags: " ++ (getNames tags)
    where
        getMaybeName :: Maybe Category -> String
        getMaybeName cat =
            case cat of
                Just category -> getName category
                Nothing -> "None"


app :: Server ()
app = do
    get "/" $ do
        payments' <- getState >>= (liftIO . readIORef . payments)
        lucid $ do
            h1_ "Payments"
            ul_ $ forM_ payments'
                $ \payment -> printPayment payment
            h2_ "New note"
            form_ [method_ "POST"] $ do
                label_ $ do
                    "Name: "
                    input_ [name_ "name"]
                label_ $ do
                    "User: "
                    input_ [name_ "user"]
                label_ $ do
                    "Date: "
                    input_ [name_ "date", type_ "date"]
                label_ $ do
                    "Price: "
                    input_ [name_ "price", type_ "number", min_ "0", step_ "0.01"]
                label_ $ do
                    "Currency: "
                    printSelect "currency"
                        [ (EUR, EUR)
                        , (PLN, PLN)
                        , (USD, USD)
                        ]
                label_ $ do
                    "Category: "
                    printEntitySelect "category"
                        [ Category (Id 1) "cat1"
                        , Category (Id 2) "cat2"
                        ]
                button_ [type_ "submit"] "Add payment"
    post root $ do
--        author <- param' "author"
--        content <- param' "content"
--        notesRef <- notes <$> getState
--        liftIO $ atomicModifyIORef' notesRef $
--            \notes -> (notes <> [Income author content], ())
        redirect "/"
    where
        printOption :: (Show a, Show b) => a -> b -> Html ()
        printOption value =
            option_ [value_ . pack . show $ value] . fromString . show

        printEntityOption :: (GetId a, GetName a) => a -> Html ()
        printEntityOption entity =
            option_ [value_ . pack . show . getId $ entity] . fromString $ (getName entity)

        printSelect :: Show a => String -> [(a, a)] -> Html ()
        printSelect name tuples =
            select_ [name_ . pack $ name] $ do
                monadicHtmlLoop (\x -> printOption (fst x) (snd x)) tuples

        printEntitySelect :: (GetId a, GetName a) => String -> [a] -> Html ()
        printEntitySelect name entities =
            select_ [name_ . pack $ name] $ do
                monadicHtmlLoop printEntityOption entities


monadicHtmlLoop :: (a -> Html ()) -> [a] -> Html ()
monadicHtmlLoop _ [] = ""
monadicHtmlLoop func (x:xs) = do
    func x
    monadicHtmlLoop func xs


--main :: IO ()
--main = do
--    state <- ServerState <$> newIORef []
--    config <- defaultSpockCfg () PCNoDatabase state
--    runSpock 8081 (spock config app)


main :: IO ()
main = do
    let res = compileMustacheText "foo"
        "Hi, {{name}}! You have:\n{{#things}}\n  * {{.}}\n{{/things}}\n"
    case res of
        Left bundle -> putStrLn (errorBundlePretty bundle)
        Right template -> TIO.putStr $ template $ Text.Mustache.object
            [ "name"   .= ("John" :: Text)
            , "things" .= ["pen" :: Text, "candle", "egg"]
            ]