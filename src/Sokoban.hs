module Sokoban (run) where

import SFML.Graphics
import SFML.Window

run :: IO ()
run = do
  let ctxSettings = Just $ ContextSettings 24 8 0 1 2 [ContextDefault]
  wnd <- createRenderWindow (VideoMode 640 480 32) "SFML Haskell Demo" [SFDefaultStyle] ctxSettings
  destroy wnd
