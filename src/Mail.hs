{-# LANGUAGE OverloadedStrings #-}
module Mail where

import Data.Text
import Data.ByteString
import Data.String
import Network.Mail.SMTP
import Network.Socket
import Text.Printf
import System.IO
import Text


getEmailAddress :: Text -> Address
getEmailAddress = Address Nothing


sendEmail_ :: HostName -> UserName -> Password -> Text -> FilePath -> [String] -> Text -> IO ()
sendEmail_ host fromEmail password subject htmlPath args toEmail = do
    html <- printfFile htmlPath args
    mail <- return (simpleMail (Address Nothing (fromString fromEmail)) [(Address Nothing toEmail)] [] [] subject [htmlPart . fromString $ html])
    sendMailWithLogin (fromString host) fromEmail password mail


sendEmail :: Text -> FilePath -> [String] -> Text -> IO ()
sendEmail = sendEmail_ "smtp.gmail.com" "ioutcome.app@gmail.com" "1qaz@WSX3edc"


sendRegistrationConfirmationEmail :: [String] -> Text -> IO ()
sendRegistrationConfirmationEmail = sendEmail "Registration confirmation" "resources/view/email/registration_confirmation.html"

