module Sokoban (run) where

import SFML.Graphics
import SFML.Window

data GameState = GameState

run :: IO ()
run = do
  let game = GameState
  let context = Just $ ContextSettings 24 8 0 1 2 [ContextDefault]
  window <- createRenderWindow (VideoMode 640 480 32) "SFML Haskell Demo" [SFDefaultStyle] context
  loop window game
  destroy window

loop :: RenderWindow -> GameState -> IO ()
loop window game = do
  result <- processEvents window game
  case result of
    Nothing -> return ()
    Just game' -> do
      clearRenderWindow window $ Color 240 240 240 255
      display window
      loop window game'

processEvents :: RenderWindow -> GameState -> IO (Maybe GameState)
processEvents window game = do
  event <- pollEvent window
  case event of
    Just SFEvtClosed -> return Nothing
    Just SFEvtKeyPressed{code = key} -> case key of
      KeyEscape -> return Nothing
      _ -> processEvents window game
    Nothing -> return . Just $ game
    _ -> processEvents window game
