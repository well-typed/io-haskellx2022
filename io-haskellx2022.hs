{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe (mapMaybe)
import Network.HTTP.Types
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html5 (body, p, ul, li, a, string, (!), stringValue, Html)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes (href)
import Web.Scotty

-- Hello

main = putStrLn  "Hello HaskellX!"

-- A conversation

main1 = do
  putStrLn "Who are you?"
  name  <- getLine
  putStrLn ("Nice to meet you, " <> name)

-- Abstraction

main2 :: IO ()
main2 = do
  putStrLn "Who are you?"
  name1 <- getLine
  putStrLn "Who are you?"
  name2 <- getLine
  putStrLn
    ("Nice to meet you, " <> name1 <> " and " <> name2)

whoAreYou :: IO String
whoAreYou = do
  putStrLn "Who are you?"
  getLine

main3 :: IO ()
main3 = do
  name1 <- whoAreYou
  name2 <- whoAreYou
  putStrLn
    ("Nice to meet you, " <> name1 <> " and " <> name2)

prompt :: String -> IO String
prompt text = do
  putStrLn text
  getLine

whoAreYou2 :: IO String
whoAreYou2 = prompt "Who are you?"

main4 :: IO ()
main4 = do
  name1 <- whoAreYou2
  name2 <- whoAreYou2
  putStrLn
    ("Nice to meet you, " <> name1 <> " and " <> name2)

-- Asking many questions

questions :: [ String]
questions =
  ["Who are you?", "Are you a Haskeller yet?"]

prompts :: [ IO String]
prompts =
  map prompt questions

askQuestions :: IO [ String]
askQuestions =
  sequence prompts

-- Separation of concerns

data Dialogue =
    Ask String Dialogue Dialogue
  | Done String

haskellXConversation :: Dialogue
haskellXConversation =
  Ask "Are you also at HaskellX?"
    (Done "Oh, too bad.")
    (Ask "Are you a Haskeller yet?"
      (Done "Perhaps after this day.")
      (Done "That's great.")
    )

interactiveDialogue :: Dialogue -> IO ()
interactiveDialogue (Ask question no yes) = do
  response <- askBooleanQuestion question
  if response
    then interactiveDialogue yes
    else interactiveDialogue no
interactiveDialogue (Done response) =
  putStrLn response

askBooleanQuestion :: String -> IO Bool
askBooleanQuestion question = do
  putStrLn question
  getBool

getBool :: IO Bool
getBool = do
  c <- getChar
  putStrLn ""
  if c == 'y'
    then pure True
    else if c == 'n'
      then pure False
      else do
        putStrLn "Please type 'y' or 'n'"
        getBool

webDialogue :: Dialogue -> IO ()
webDialogue d =
  scotty 8000 $ do
    get "/" $ from ""
    get "/:responses" $ do
      responseString <- param "responses"
      from responseString
  where
    from responseString = do
      let responses = mapMaybe parseResponse responseString
      case replay d responses of
        Just (Ask question _ _) ->
          htmlPage $ do
            p (string question)
            ul $ do
              li (a ! href (stringValue (responseString <> "y")) $ "yes")
              li (a ! href (stringValue (responseString <> "n")) $ "no")
        Just (Done response) ->
          htmlPage $
            p (string response)
        Nothing -> status status404

htmlPage :: Html -> ActionM ()
htmlPage =
  html . renderHtml . H.html . H.body

parseResponse :: Char -> Maybe Bool
parseResponse 'y'  = Just True
parseResponse 'n'  = Just False
parseResponse _    = Nothing

replay :: Dialogue -> [ Bool] -> Maybe Dialogue
replay (Ask _ _  yes) (True  : responses) = replay yes responses
replay (Ask _ no _  ) (False : responses) = replay no  responses
replay d              []                  = Just d
replay _              _                   = Nothing
