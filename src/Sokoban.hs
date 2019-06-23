module Sokoban (run) where

import Game
import Graphics

run :: IO ()
run = do
  let game = createGame
  graphics <- initialize
  loop graphics game
  cleanup graphics

loop :: Graphics -> GameState -> IO ()
loop graphics game = do
  result <- processEvents graphics
  case result of
    Nothing -> return ()
    Just events -> do
      let game' = updateGame game events
      render graphics game'
      loop graphics game'
