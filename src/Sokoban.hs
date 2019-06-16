module Sokoban (run) where

import qualified SFML.Graphics as Sf
import qualified SFML.Window as Sf

data GameState = GameState {
  getPosition :: (Int,Int)
}

data Graphics = Graphics {
  getWindow :: Sf.RenderWindow,
  getFigure :: Sf.RectangleShape
}

data Event = MoveDown | MoveUp | MoveLeft | MoveRight
type Events = [Event]

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
  result <- processEvents $ getWindow graphics
  case result of
    Nothing -> return ()
    Just events -> do
      let game' = updateGame game events
      render graphics game'
      loop graphics game'

updateGame :: GameState -> Events -> GameState
updateGame game [] = game
updateGame game (MoveDown:es) = updateGame (move game (0,1)) es
updateGame game (MoveUp:es) = updateGame (move game (0,-1)) es
updateGame game (MoveLeft:es) = updateGame (move game (-1,0)) es
updateGame game (MoveRight:es) = updateGame (move game (1,0)) es

render :: Graphics -> GameState -> IO ()
render graphics game = do
  let figure = getFigure graphics
  let window = getWindow graphics
  Sf.clearRenderWindow window $ Sf.Color 240 240 240 255
  Sf.setPosition figure $ figurePosition game
  Sf.drawRectangle window (getFigure graphics) Nothing
  Sf.display window

processEvents :: Sf.RenderWindow -> IO (Maybe Events)
processEvents window = do
  event <- Sf.pollEvent window
  case event of
    Just Sf.SFEvtClosed -> return Nothing
    Just Sf.SFEvtKeyPressed{Sf.code = Sf.KeyEscape} -> return Nothing
    Just Sf.SFEvtKeyPressed{Sf.code = Sf.KeyDown} -> fmap (MoveDown:) <$> processEvents window
    Just Sf.SFEvtKeyPressed{Sf.code = Sf.KeyUp} -> fmap (MoveUp:) <$> processEvents window
    Just Sf.SFEvtKeyPressed{Sf.code = Sf.KeyLeft} -> fmap (MoveLeft:) <$> processEvents window
    Just Sf.SFEvtKeyPressed{Sf.code = Sf.KeyRight} -> fmap (MoveRight:) <$> processEvents window
    Nothing -> return (Just [])
    _ -> processEvents window

figurePosition :: GameState -> Sf.Vec2f
figurePosition game =
  let (x,y) = getPosition game
  in Sf.Vec2f (fromIntegral $ 64 * x) (fromIntegral $ 64 * y)

move :: GameState -> (Int,Int) -> GameState
move game@GameState{getPosition = (x,y)} (dx,dy) = game { getPosition = (x+dx,y+dy) }
