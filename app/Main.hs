
module Main (main) where

import Discord

import qualified Control.Monad.Trans.Reader as Reader

import Data.Map.Strict qualified as Map

import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Text.IO qualified as T

import Data.ByteString.Lazy qualified as BS.L

import Data.Aeson qualified as Aeson

import UnliftIO (liftIO)
import UnliftIO.MVar

import Data.Maybe
import System.Environment (getEnv)

import Control.Applicative
import Control.Monad

import Prelude hiding (id)

import BotC.Commands
import BotC.BotState

--------------------------------------------------------------------------------
-- DATA

-- Internal bot state

--------------------------------------------------------------------------------
-- APPLICATION LOOP

main :: IO ()
main = do
  mdata <- do
    mfile <- Aeson.decode <$> BS.L.readFile "./cached_state"
    s <- case mfile of
      Just file -> do
        liftIO $ putStrLn "loaded existing state"
        pure file
      Nothing -> do
        liftIO $ putStrLn "created new state"
        pure
          MkBotState
            { roles = Map.empty
            , nameMappings = Map.empty
            , linkedMessages = Map.empty
            }
    newMVar s


  token <- T.pack <$> getEnv "DISCORD_TOKEN"

  eventMVar <- newEmptyMVar

  let start = flip Reader.runReaderT mdata do
        -- On a bot restart, always pull the latest data
        reloadRoles
        -- Initialise all the application commands, and get their event handlers
        handlers <- traverse commandToHandlers commands
        putMVar eventMVar \botstate event ->
          asum $
            (\handler -> handler botstate event)
              <$> handlers
        liftIO $ putStrLn "Ready!"

  userFacingError <-
    runDiscord $ do
      def
        { discordToken = token
        , discordOnStart = start
        , discordOnEvent = \event ->
            tryReadMVar eventMVar >>=
              \case
                Nothing -> liftIO $ putStrLn [i|Tried to process event #{show event} before event handler was ready!|]
                Just evHandler -> flip Reader.runReaderT mdata do
                  botState <- readBotState
                  fromMaybe (pure ()) $ evHandler botState event
        , discordOnLog = \s -> T.putStrLn s >> T.putStrLn ""
        }
  T.putStrLn userFacingError
