{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Map.Strict as Map
import qualified Network.HTTP.Simple as HTTP
import Data.Aeson ((.=), object, eitherDecode, FromJSON(..), ToJSON(..))
import Data.Aeson.Types (parseMaybe, (.:), withObject)
import Control.Monad (forM_, when, filterM)
import System.Random (randomRIO)
import Data.List (permutations)
import Data.Maybe (fromMaybe)
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Char8 as S8

data GameState = GameState
  { gsSession :: HTTP.Request
  , gsWords :: [String]
  , gsId :: String
  , gsChances :: Int
  , gsGuess :: String
  , gsResponse :: String
  , gsAttemptNum :: Int
  , gsStatus :: String
  , gsAvailableWords :: [String]
  , gsCookie :: S8.ByteString
  }

instructions :: String
instructions = "For every guessed word, give a spaced string for each character, like: 'g a b b g'.\n\
               \ g = Green (correct position)\n\
               \ y = Yellow (wrong position, but present)\n\
               \ r = Red (not present in the word)"

main :: IO ()
main = do
  -- Initialize the game state
  let initialState = GameState
        { gsSession = HTTP.defaultRequest
        , gsWords = []
        , gsId = ""
        , gsChances = 6
        , gsGuess = ""
        , gsResponse = ""
        , gsAttemptNum = 0
        , gsStatus = "PLAY"
        , gsAvailableWords = []
        , gsCookie = ""
        }

  -- Get the list of words
  wordLines <- lines <$> readFile "5words.txt"
  let fiveLetterWords = filter ((== 5) . length) wordLines
      initialState' = initialState { gsWords = fiveLetterWords, gsAvailableWords = fiveLetterWords }

  -- Register the player
  let registerRequest = HTTP.setRequestBodyJSON (object ["mode" .= ("wordle" :: String), "name" .= ("durga" :: String)])
                        $ HTTP.setRequestMethod "POST"
                        $ HTTP.setRequestPath "/game/register"
                        $ HTTP.setRequestHost "wordle.we4shakthi.in"
                        $ gsSession initialState'
  registerResponse <- HTTP.httpLBS registerRequest
  let registerBody = eitherDecode (HTTP.getResponseBody registerResponse) :: Either String RegisterResponse
      cookie = S8.intercalate "; " $ map (S8.takeWhile (/= ';')) $ HTTP.getResponseHeader "Set-Cookie" registerResponse

  case registerBody of
    Left err -> putStrLn $ "Failed to register: " ++ err
    Right regRes -> do
      let initialState'' = initialState' { gsId = registerId regRes, gsCookie = cookie }
      play initialState''

data RegisterResponse = RegisterResponse { registerId :: String }
instance FromJSON RegisterResponse where
  parseJSON = withObject "RegisterResponse" $ \v -> RegisterResponse <$> v .: "id"

play :: GameState -> IO ()
play gameState = do
  -- Create the game
  let createRequest = HTTP.addRequestHeader "Cookie" (gsCookie gameState)
                    $ HTTP.setRequestBodyJSON (object ["id" .= gsId gameState, "overwrite" .= True])
                    $ HTTP.setRequestMethod "POST"
                    $ HTTP.setRequestPath "/game/create"
                    $ HTTP.setRequestHost "wordle.we4shakthi.in"
                    $ gsSession gameState
  createResponse <- HTTP.httpLBS createRequest
  let statusCode = HTTP.getResponseStatusCode createResponse
  if statusCode == 200
    then putStrLn "Game already exists."
    else if statusCode == 201
      then putStrLn "Game has been created."
      else do
        putStrLn $ "Game creation failed: HTTP " ++ show statusCode
        L8.putStrLn $ HTTP.getResponseBody createResponse

  putStrLn instructions
  gameLoop gameState

gameLoop :: GameState -> IO ()
gameLoop gameState@GameState{..} = do
  if gsAttemptNum >= gsChances || gsStatus /= "PLAY"
    then when (gsStatus /= "WON") $ putStrLn "The computer failed to guess the word."
    else do
      let newAttemptNum = gsAttemptNum + 1
      shuffledWords <- head . permutations <$> pure gsAvailableWords
      let newGuess = head shuffledWords
          remainingWords = tail shuffledWords

      putStrLn $ "\nAttempt " ++ show newAttemptNum ++ ": Is it '" ++ newGuess ++ "'?'"

      let guessRequest = HTTP.addRequestHeader "Cookie" gsCookie
                       $ HTTP.setRequestBodyJSON (object ["guess" .= newGuess, "id" .= gsId])
                       $ HTTP.setRequestMethod "POST"
                       $ HTTP.setRequestPath "/game/guess"
                       $ HTTP.setRequestHost "wordle.we4shakthi.in"
                       $ gsSession
      guessResponse <- HTTP.httpLBS guessRequest
      let guessBody = eitherDecode (HTTP.getResponseBody guessResponse) :: Either String GuessResponse
      case guessBody of
        Left err -> do
          putStrLn $ "No feedback received — skipping this guess. Error: " ++ err
          gameLoop gameState { gsAttemptNum = newAttemptNum, gsAvailableWords = remainingWords }
        Right guessRes -> do
          let feedback = fromMaybe "" (guessFeedback guessRes)
              message = fromMaybe "No message" (guessMessage guessRes)
          putStrLn $ "Message: " ++ message
          putStrLn $ "Feedback: " ++ feedback

          if all (== 'g') feedback
            then do
              putStrLn "The computer guessed the correct word!"
              pure ()
            else do
              let newAvailableWords = dropImpossibles feedback newGuess gsAvailableWords
              if null newAvailableWords
                then putStrLn "No more possible words match the given feedback."
                else gameLoop gameState { gsAttemptNum = newAttemptNum, gsAvailableWords = newAvailableWords, gsGuess = newGuess }

data GuessResponse = GuessResponse
  { guessFeedback :: Maybe String
  , guessMessage :: Maybe String
  }
instance FromJSON GuessResponse where
  parseJSON = withObject "GuessResponse" $ \v -> GuessResponse <$> v .: "feedback" <*> v .: "message"

dropImpossibles :: String -> String -> [String] -> [String]
dropImpossibles feedback guess' wordList =
  let
    (greens, ambers, blacks) = foldr processFeedback (Map.empty, Map.empty, "") (zip3 feedback guess' [0..])
    processFeedback (feed, guessChar, i) (g, a, b) =
      case feed of
        'g' -> (Map.insert i guessChar g, a, b)
        'y' -> (g, Map.insertWith (++) guessChar [i] a, b)
        'r' -> (g, a, guessChar : b)
        _   -> (g, a, b)

    filterBlacks = filter (\word -> all (`notElem` word) blacks)
    filterGreens = filter (\word -> all (\(i, c) -> (word !! i) == c) (Map.toList greens))
    filterAmbers = filter (\word -> all (\(c, pos) -> c `elem` word && not (any (\p -> (word !! p) == c) pos)) (Map.toList ambers))

  in filterBlacks . filterGreens . filterAmbers $ wordList