module Validation where

import Data.Text
import Data.Text.Encoding
import Text.Email.Validate


notBlank :: Text -> Bool
notBlank value = Data.Text.length value > 0


minLength :: Int -> Text -> Bool
minLength len value = Data.Text.length value >= len


maxLength :: Int -> Text -> Bool
maxLength len value = Data.Text.length value <= len


isEmail :: Text -> Bool
isEmail = isValid . encodeUtf8


minValue :: Double -> Double -> Bool
minValue min value = value >= min


maxValue :: Double -> Double -> Bool
maxValue max value = value <= max


inArray :: Eq a => [a] -> a -> Bool
inArray [] _ = False
inArray (x:xs) value =
    if value == x
        then True
        else inArray xs value


allCharsIn :: [Char] -> String -> Bool
allCharsIn chars string =
    Data.Text.all (inArray chars) (pack string)


isAlphanum :: String -> Bool
isAlphanum =
    allCharsIn "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"


validate :: [((a -> Bool), String)] -> Either String a -> Either String a
validate _ (Left error) = Left error
validate [] (Right value) = Right value
validate ((validator, error) : vs) (Right value) =
    if validator value
        then Validation.validate vs (Right value)
        else Left error


validateEntityField :: (entity -> a) -> [((a -> Bool), String)] -> entity -> Either String a
validateEntityField getter [] entity = Right (getter entity)
validateEntityField getter validators entity =
    Validation.validate validators (Right (getter entity))


validateEntity_
    :: [(String, String)]
    -> [(String, (entity -> a), [((a -> Bool), String)])]
    -> entity
    -> [(String, String)]
validateEntity_ errors [] _ = errors
validateEntity_ errors ((fieldName, getter, []) : tail) entity =
    validateEntity_ errors tail entity

validateEntity_ errors ((fieldName, getter, validators) : tail) entity =
    case validateEntityField getter validators entity of
        Left error -> validateEntity_ ((fieldName, error) : errors) tail entity
        Right _ -> validateEntity_ errors tail entity


class ValidateEntity entity where
    validateEntity :: entity -> [(String, String)]