module Graphics where

import qualified SFML.Graphics as Sf
import qualified SFML.Window as Sf

import Game

data Graphics = Graphics {
  getWindow :: Sf.RenderWindow,
  getFigure :: Sf.RectangleShape
}

initialize :: IO Graphics
initialize = do
  let context = Just $ Sf.ContextSettings 24 8 0 1 2 [Sf.ContextDefault]
  window <- Sf.createRenderWindow (Sf.VideoMode 640 480 32) "Sokoban" [Sf.SFDefaultStyle] context
  figure <- Sf.err Sf.createRectangleShape
  Sf.setFillColor figure Sf.blue
  Sf.setSize figure $ Sf.Vec2f 64 64
  return $ Graphics window figure

cleanup :: Graphics -> IO ()
cleanup g = do
  Sf.destroy $ getFigure g
  Sf.destroy $ getWindow g

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
