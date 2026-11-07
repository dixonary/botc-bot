
module Main (main) where

import Discord

import qualified Control.Monad.Trans.Reader as Reader

import Data.Map.Strict qualified as Map

import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Text.IO qualified as T

import Data.ByteString.Lazy qualified as BS.L

import Data.Aeson qualified as Aeson

import UnliftIO (liftIO, newIORef, writeIORef, readIORef)
import UnliftIO.MVar

import Data.Maybe
import System.Environment (getEnv)

import Control.Applicative
import Control.Monad

import Prelude hiding (id)

import BotC.Commands
import BotC.BotState
import BotC.Commands.Lookup
import BotC.Commands.Help

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
            , linkedMessages = Map.empty
            , cachedIcons = MkImageData Map.empty
            }
    newMVar s


  token <- T.pack <$> getEnv "DISCORD_TOKEN"

  -- Uses an IORef instead of an MVar because they're faster and there should
  -- only ever be one writer but many readers.
  eventRef <- newIORef \_botState event -> Just $ liftIO $ putStrLn
    [i|Tried to process event #{show event} before event handler was ready!|]

  let start = flip Reader.runReaderT mdata do
        -- On a bot restart, always pull the latest data
        reloadRoles
        -- Initialise all the application commands, and get their event handlers
        handlers <- traverse commandToHandlers commands
        writeIORef eventRef \botstate event ->
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
            readIORef eventRef >>= \evHandler -> flip Reader.runReaderT mdata do
              botState <- readBotState
              fromMaybe (pure ()) $ evHandler botState event
        , discordOnLog = \s -> T.putStrLn s >> T.putStrLn ""
        }
  T.putStrLn userFacingError

commands :: [Command]
commands = [help commands, lookupCharacter]
