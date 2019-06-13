module Sokoban (run) where

import SFML.Graphics
import SFML.Window

data GameState = GameState

data Objects = Objects {
  window :: RenderWindow,
  figure :: RectangleShape
}

run :: IO ()
run = do
  let game = GameState
  let context = Just $ ContextSettings 24 8 0 1 2 [ContextDefault]
  wnd <- createRenderWindow (VideoMode 640 480 32) "SFML Haskell Demo" [SFDefaultStyle] context
  fig <- err $ createRectangleShape
  setFillColor fig blue
  setPosition fig $ Vec2f 320 240
  setSize fig $ Vec2f 64 64
  let objects = Objects wnd fig
  loop objects game
  destroy wnd

loop :: Objects -> GameState -> IO ()
loop objects game = do
  let wnd = window objects
  let fig = figure objects
  result <- processEvents wnd game
  case result of
    Nothing -> return ()
    Just game' -> do
      clearRenderWindow wnd $ Color 240 240 240 255
      drawRectangle wnd fig Nothing
      display wnd
      loop objects game'

processEvents :: RenderWindow -> GameState -> IO (Maybe GameState)
processEvents wnd game = do
  event <- pollEvent wnd
  case event of
    Just SFEvtClosed -> return Nothing
    Just SFEvtKeyPressed{code = key} -> case key of
      KeyEscape -> return Nothing
      _ -> processEvents wnd game
    Nothing -> return . Just $ game
    _ -> processEvents wnd game
