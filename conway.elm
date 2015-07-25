import Color exposing (..)
import List exposing (..)
import Basics exposing (round)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Random exposing (int, generate, initialSeed, float, list)
import Window



-- config
cellSize = 10
gridSize = 50
gridPixels = cellSize*gridSize
gridOffset = gridSize/2
seed = initialSeed 1
activeColor = blue
inactiveColor = gray
side = [-gridOffset..gridOffset]



main : Signal Element
main =
  Signal.map view Window.dimensions


view : (Int,Int) -> Element
view (w,h)  =
  plot randGrid
    |> container w h middle



plot bools =
  map gridElement (zip coords bools)
    |> collage gridPixels gridPixels


gridElement (x, y, active) =
  square cellSize
    |> filled (if active then activeColor else inactiveColor)
    |> move ( x , y )


coords =
  let position v = (v*cellSize - cellSize/2)
      makePoint x y = (position x, position y)
      mapSide f = map f side
      addXcoord = \x -> mapSide (makePoint x)
  in  concat (mapSide addXcoord)


zip al bl =
  case (al, bl) of
    ( (x,y)::al', (x')::bl' ) -> (x,y,x') :: zip al' bl'
    (_, _) -> []


randGrid = concat (genRandBoolGrid gridSize seed)


genRandBoolGrid size initial =
    if size == 0 then
      []
    else
      let (values, newseed) = generate (list gridSize (int 0 1)) initial
      in (map toBool values) :: (genRandBoolGrid (size - 1) newseed)


toBool n =
  case n of
    0 -> False
    1 -> True
