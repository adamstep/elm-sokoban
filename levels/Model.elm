module Model exposing (..)

import Matrix exposing (..)
import Keyboard.Extra exposing (..)
import List exposing (..)
import Element exposing (Element)
import Maybe exposing (withDefault, andThen)

type WorldCell
  = Wall
  | None
  | Floor Item FloorType
  | Goal Item FloorType

type WallType
  = TopLeft
  | TopRight
  | TopBottom
  | Top
  | Bottom
  | TopRightBottom
  | BottomRightLeft
  | BottomLeft
  | BottomRight
  | RightLeft
  | Left
  | Right
  | TopRightLeft
  | TopBottomLeft

type Item
  = Empty
  | Package PackageType
  | Player

type FloorType
  = Wood
  | Tile

type PackageType
  = Box
  | SmallBox
  | Television
  | Chair
  | Plant

type alias Grid = Matrix WorldCell

type alias Direction = { x : Int, y : Int}

noDirection : Direction
noDirection =
    Direction 0 0

type alias Model =
    { keyboardModel : Keyboard.Extra.Model
    , counter: Int
    , grid: Grid
    , position: Location
    , direction: Direction
    , ornaments: List Ornament
    }

type alias Ornament = 
    { position: Location
    , degrees: Float
    , element: Element
    }

removeItem: Location -> Grid -> Maybe Grid
removeItem loc grid =
    let 
        cell = withDefault None (get loc grid)
        newCell = case cell of
            Floor i t ->
                Floor Empty t
            Goal i t ->
                Goal Empty t
            _ ->
                cell
    in
        Just (set loc newCell grid)

setItem: Location -> Item -> Grid -> Maybe Grid
setItem loc item grid =
    let
        cell = withDefault None (get loc grid)
        newCell = case cell of
            Floor Empty t->
                Just (Floor item t)
            Goal Empty t ->
                Just (Goal item t)
            _ ->
                Nothing
    in
        case newCell of
            Nothing ->
                Nothing
            Just cell ->
                Just (set loc cell grid)

moveItem: Item -> Location -> Location -> Grid -> Maybe Grid
moveItem item fromLoc toLoc grid =
    setItem toLoc item grid
    `andThen`
    removeItem fromLoc

wrap: Location -> Direction -> Grid -> Location
wrap loc dir grid =
    let
        maxX = (Matrix.colCount grid) - 1
        maxY = (Matrix.rowCount grid) - 1
        x = min maxX (max 0 (fst loc) - dir.y)
        y = min maxY (max 0 (snd loc) + dir.x)
    in
        (x, y)

updateLoc: Direction -> Model -> Model
updateLoc dir model =
    let
        newLoc = wrap model.position dir model.grid
        newPackageLoc = wrap newLoc dir model.grid
        cell = withDefault None (Matrix.get newLoc model.grid)
        newGrid = case cell of
            Wall ->
                Nothing

            None ->
                Nothing

            Floor (Package p) t ->
                moveItem (Package p) newLoc newPackageLoc model.grid
                `andThen`
                moveItem Player model.position newLoc

            Goal (Package p) t ->
                moveItem (Package p) newLoc newPackageLoc model.grid
                `andThen`
                moveItem Player model.position newLoc

            _ ->
                moveItem Player model.position newLoc model.grid
    in
        case newGrid of
            Nothing ->
                model
            Just grid ->
                { model | grid=grid, position=newLoc }
