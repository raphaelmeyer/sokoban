module Game (
  GameState(..),
  Event(..),
  Events,
  Direction(..),
  createGame,
  updateGame
) where

data Direction = LookUp | LookDown | LookLeft | LookRight

data GameState = GameState {
  getPosition :: (Int,Int),
  getDirection :: Direction
}

data Event = MoveDown | MoveUp | MoveLeft | MoveRight
type Events = [Event]

createGame :: GameState
createGame = GameState (5,5) LookDown

updateGame :: GameState -> Events -> GameState
updateGame game [] = game
updateGame game (MoveDown:es) = updateGame (move game ((0,1), LookDown)) es
updateGame game (MoveUp:es) = updateGame (move game ((0,-1), LookUp)) es
updateGame game (MoveLeft:es) = updateGame (move game ((-1,0), LookLeft)) es
updateGame game (MoveRight:es) = updateGame (move game ((1,0), LookRight)) es

move :: GameState -> ((Int,Int), Direction) -> GameState
move game@GameState{getPosition = (x,y)} ((dx,dy),direction) =
  game { getPosition = (x+dx,y+dy), getDirection = direction }
