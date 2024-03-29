module Graphics (
  Graphics,
  initialize,
  cleanup,
  render,
  processEvents
) where

import qualified SFML.Graphics as Sf
import qualified SFML.Window as Sf

import Paths_Sokoban

import Game

data Graphics = Graphics {
  getWindow :: Sf.RenderWindow,
  getFigure :: Sf.RectangleShape,
  getTilesheet :: Sf.Texture
}

data Sprite = FigureUp | FigureDown | FigureLeft | FigureRight | Box | Ground | Wall

initialize :: IO Graphics
initialize = do
  let context = Just $ Sf.ContextSettings 24 8 0 1 2 [Sf.ContextDefault]
  window <- Sf.createRenderWindow (Sf.VideoMode 640 480 32) "Sokoban" [Sf.SFDefaultStyle] context
  tilesheetPath <- getDataFileName "sokoban_tilesheet.png"
  tilesheet <- Sf.err $ Sf.textureFromFile tilesheetPath Nothing
  figure <- Sf.err Sf.createRectangleShape
  Sf.setSize figure $ Sf.Vec2f 64 64
  Sf.setTexture figure tilesheet True
  Sf.setTextureRect figure $ tile FigureUp
  return $ Graphics window figure tilesheet

cleanup :: Graphics -> IO ()
cleanup g = do
  Sf.destroy $ getFigure g
  Sf.destroy $ getTilesheet g
  Sf.destroy $ getWindow g

render :: Graphics -> GameState -> IO ()
render graphics game = do
  let figure = getFigure graphics
  let window = getWindow graphics
  Sf.clearRenderWindow window Sf.white
  Sf.setTextureRect figure $ figureTile game
  Sf.setPosition figure $ figurePosition game
  Sf.drawRectangle window (getFigure graphics) Nothing
  Sf.display window

processEvents :: Graphics -> IO (Maybe Events)
processEvents graphics = do
  event <- Sf.pollEvent $ getWindow graphics
  case event of
    Just Sf.SFEvtClosed -> return Nothing
    Just Sf.SFEvtKeyPressed{Sf.code = Sf.KeyEscape} -> return Nothing
    Just Sf.SFEvtKeyPressed{Sf.code = Sf.KeyDown} -> fmap (MoveDown:) <$> processEvents graphics
    Just Sf.SFEvtKeyPressed{Sf.code = Sf.KeyUp} -> fmap (MoveUp:) <$> processEvents graphics
    Just Sf.SFEvtKeyPressed{Sf.code = Sf.KeyLeft} -> fmap (MoveLeft:) <$> processEvents graphics
    Just Sf.SFEvtKeyPressed{Sf.code = Sf.KeyRight} -> fmap (MoveRight:) <$> processEvents graphics
    Nothing -> return (Just [])
    _ -> processEvents graphics

figurePosition :: GameState -> Sf.Vec2f
figurePosition game =
  let (x,y) = getPosition game
  in Sf.Vec2f (fromIntegral $ 64 * x) (fromIntegral $ 64 * y)

figureTile :: GameState -> Sf.IntRect
figureTile game = case getDirection game of
  LookDown -> tile FigureDown
  LookUp -> tile FigureUp
  LookLeft -> tile FigureLeft
  LookRight -> tile FigureRight

tile :: Sprite -> Sf.IntRect
tile sprite = Sf.IntRect (64*x) (64*y) 64 64
  where
    (x,y) = case sprite of
      FigureUp -> (3, 5)
      FigureDown -> (0, 5)
      FigureLeft -> (4, 7)
      FigureRight -> (1, 7)
      Box -> (1,0)
      Ground -> (11,6)
      Wall -> (8,6)
