{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- https://hackage.haskell.org/package/dunai
-- https://hackage.haskell.org/package/bearriver
-- https://hackage.haskell.org/package/Yampa
-- https://wiki.haskell.org/Yampa
-- https://hackage.haskell.org/package/base-4.16.0.0/docs/Control-Arrow.html

{-
  type Effect     = Proxy X () () X
  type Producer b = Proxy X () () b
  type Pipe   a b = Proxy () a () b
  type Consumer a = Proxy () a () X
-}

module Main where

import Control.Applicative (Applicative (liftA2))
import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Control
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.Trans.Class as Trans
import Control.Monad.Trans.Cont (ContT (..), evalContT)
import qualified Control.Monad.Trans.State as State
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Foldable as Foldable
import Data.IORef (IORef)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified FlowControl
import GHC.Float (double2Float)
import GHC.Generics (Generic)
import qualified GHC.IORef as IORef
import qualified GHC.MVar as Concurrent
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as OpenGL
import qualified Graphics.UI.GLFW as GLFW
import qualified System.FSNotify as FSNotify

fsManager :: ContT r IO FSNotify.WatchManager
fsManager = ContT FSNotify.withManager

fsWatchDir :: FSNotify.WatchManager -> IO (IO [FSNotify.Event])
fsWatchDir mgr = do
  mvar <- Concurrent.newMVar []
  _ <- FSNotify.watchDir mgr "./res" filterModified \ev ->
    Concurrent.modifyMVar_ mvar $ pure . (<> [ev])
  pure $ Concurrent.modifyMVar mvar $ pure . ([],)
  where
    filterModified :: FSNotify.Event -> Bool
    filterModified = \case FSNotify.Modified _ _ False -> True; _ -> False

newtype Settings = Settings
  { clearColor :: (Float, Float, Float)
  }
  deriving (Eq, Generic, Aeson.FromJSON)

readSettings :: IO (Maybe Settings)
readSettings = do
  x <- Aeson.decode <$> BL.readFile "./res/settings.json"
  Control.when (Maybe.isNothing x) $ putStrLn "Unable to read settings!"
  pure x

settingsSetting :: MonadIO m => Bool -> Settings -> Settings -> m ()
settingsSetting checkDiff Settings {clearColor = prevCol} Settings {clearColor = nextCol@(r, g, b)} = do
  Control.when (not checkDiff || prevCol /= nextCol) $ OpenGL.clearColor $= OpenGL.Color4 r g b 1

setSettings :: MonadIO m => Settings -> m ()
setSettings s = settingsSetting False s s

setSettingsOnChange :: MonadIO m => Settings -> Settings -> m ()
setSettingsOnChange = settingsSetting True

initState :: IO ()
initState = pure ()

nextState :: Bool -> IO (Double, Maybe ())
nextState _ = do
  GLFW.pollEvents
  pure (0.1, Just ())

output :: GLFW.Window -> Bool -> Double -> IO Bool
output window _ (double2Float -> r) = do
  shouldStop <- GLFW.windowShouldClose window
  Control.unless shouldStop do
    OpenGL.clearColor $= OpenGL.Color4 r (1 - r) 0 1
    OpenGL.clear [OpenGL.ColorBuffer]
    GLFW.swapBuffers window
  pure shouldStop

main :: IO ()
main = evalContT do
  FlowControl.continueWhen =<< FlowControl.manage GLFW.init (const GLFW.terminate)
  window <- FlowControl.shortCircuitMaybe () $ GLFW.createWindow 800 600 "Hello, window" Nothing Nothing
-- (wholeGame &&& arr id)
  Trans.lift do
    GLFW.makeContextCurrent (Just window)
    -- Yampa.reactimate initState nextState (output window) signalFunction

-- main :: IO ()
-- main = evalContT do
--   settings :: IORef Settings <- Trans.lift . IORef.newIORef =<< FlowControl.shortCircuitMaybe () readSettings
--   dirChanges :: IO [FSNotify.Event] <- Trans.lift . fsWatchDir =<< fsManager

--   FlowControl.continueWhen =<< FlowControl.manage GLFW.init (const GLFW.terminate)
--   window <- FlowControl.shortCircuitMaybe () $ GLFW.createWindow 800 600 "Hello, window" Nothing Nothing

--   Trans.lift do
--     GLFW.makeContextCurrent (Just window)
--     setSettings =<< IORef.readIORef settings
--     Yampa.reactimate initState (nextState window) (output window) undefined

--   FlowControl.loopUntil $ GLFW.windowShouldClose window
--   Trans.lift do
--     (null -> dirIsUnChanged) <- dirChanges
--     Control.unless dirIsUnChanged do
--       prevSettings <- IORef.readIORef settings
--       maybe (pure ()) (setSettingsOnChange prevSettings) =<< readSettings
--     OpenGL.clear [OpenGL.ColorBuffer]
--     GLFW.swapBuffers window
--     GLFW.pollEvents
