module Sokoban (run) where

import qualified SFML.Graphics as Sf
import qualified SFML.Window as Sf

data GameState = GameState {
  getFigPos :: (Int,Int)
}

data Graphics = Graphics {
  getWindow :: Sf.RenderWindow,
  getFigure :: Sf.RectangleShape
}

run :: IO ()
run = do
  let game = GameState (5,5)
  let context = Just $ Sf.ContextSettings 24 8 0 1 2 [Sf.ContextDefault]
  window <- Sf.createRenderWindow (Sf.VideoMode 640 480 32) "SFML Haskell Demo" [Sf.SFDefaultStyle] context
  figure <- Sf.err Sf.createRectangleShape
  Sf.setFillColor figure Sf.blue
  Sf.setSize figure $ Sf.Vec2f 64 64
  let graphics = Graphics window figure
  loop graphics game
  Sf.destroy figure
  Sf.destroy window

loop :: Graphics -> GameState -> IO ()
loop graphics game = do
  let window = getWindow graphics
  result <- processEvents window game
  case result of
    Nothing -> return ()
    Just game' -> do
      let figure = getFigure graphics
      Sf.clearRenderWindow window $ Sf.Color 240 240 240 255
      Sf.setPosition figure $ figurePosition game'
      Sf.drawRectangle window (getFigure graphics) Nothing
      Sf.display window
      loop graphics game'

processEvents :: Sf.RenderWindow -> GameState -> IO (Maybe GameState)
processEvents window game = do
  event <- Sf.pollEvent window
  case event of
    Just Sf.SFEvtClosed -> return Nothing
    Just Sf.SFEvtKeyPressed{Sf.code = Sf.KeyEscape} -> return Nothing
    Nothing -> return . Just $ game
    _ -> processEvents window game

figurePosition :: GameState -> Sf.Vec2f
figurePosition game =
  let (x,y) = getFigPos game
  in Sf.Vec2f (fromIntegral $ 64 * x) (fromIntegral $ 64 * y)
