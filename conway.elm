
-- Conway's game of life
-- -----------------------------------
-- adapted algorithms from Haskell
-- implementation here http://pastebin.com/K3DCyKj3


import List exposing (map, length, member, filter, concat)
import Graphics.Element exposing (Element, show)

type alias Board = List Cell
type alias Cell = (Int, Int)

-- board dimensions
board =
  {
    height = 100
  , width = 100
  }


-- test board
glider : Board
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]


-- check if a cell is in the board (element member of list)
-- flip arguments for currying
alive : Board -> Cell -> Bool
alive b c = member c b


-- check if a cell is not in the board
dead : Board -> Cell -> Bool
dead b c = not <| alive b c


-- get list of neighbor coordinates, bounded by board dimensions
neighbors : Cell -> Board
neighbors (x, y) =
  map wrap [ (x-1,y-1) , (x  ,y-1),
             (x+1,y-1) , (x-1,y  ),
             (x+1,y  ) , (x-1,y+1),
             (x  ,y+1) , (x+1,y+1) ]


-- wrap a cells coordinates around the edge of the board
wrap : Cell -> Cell
wrap (x, y) =
  (
    ((x - 1) % board.width) + 1
  , ((y - 1) % board.height + 1)
  )


-- Count the number of living neighbors in a cell
livingNeighbors : Board -> Cell -> Int
livingNeighbors b c =
  neighbors c
    |> filter (alive b)
    |> length


-- Evolution Rules
shouldSurvive : Board -> Cell -> Bool
shouldSurvive b c = member (livingNeighbors b c) [2, 3]

shouldBeBorn : Board -> Cell -> Bool
shouldBeBorn b c = (dead b c) && (livingNeighbors b c == 3)


-- filter board by rules
survivors : Board -> Board
survivors b = filter ( shouldSurvive b ) b

births : Board -> Board
births b =
  map neighbors b
    |> concat
    |> removeDuplicates
    |> filter ( shouldBeBorn b )


-- remove duplicate cells generated by finding neighbors
removeDuplicates : Board -> Board
removeDuplicates b =
  case b of
    [] -> []
    x :: xs -> x :: removeDuplicates (filter ((/=) x) xs)


-- evolve state of game
evolve : Board -> Board
evolve b = survivors b ++ births b


main : Element
main = show <| evolve glider
