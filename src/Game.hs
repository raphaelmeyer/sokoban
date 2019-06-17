module Game where

data GameState = GameState {
  getPosition :: (Int,Int)
}

data Event = MoveDown | MoveUp | MoveLeft | MoveRight
type Events = [Event]

updateGame :: GameState -> Events -> GameState
updateGame game [] = game
updateGame game (MoveDown:es) = updateGame (move game (0,1)) es
updateGame game (MoveUp:es) = updateGame (move game (0,-1)) es
updateGame game (MoveLeft:es) = updateGame (move game (-1,0)) es
updateGame game (MoveRight:es) = updateGame (move game (1,0)) es

move :: GameState -> (Int,Int) -> GameState
move game@GameState{getPosition = (x,y)} (dx,dy) = game { getPosition = (x+dx,y+dy) }
