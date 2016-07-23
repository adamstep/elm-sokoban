import Array exposing (Array)
import List exposing (map)
import Maybe exposing (andThen)
import Debug exposing (log)

import Html exposing (Html, Attribute, button, div, text, table, tr, td)
import Html.Attributes exposing (style, title, class, classList, attribute, href)
import Html.App as Html
import Html.Events exposing (onClick)

main =
    Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }

-- MODEL

type alias Grid =
  Array Row

type alias Row =
  Array Cell

type alias Cell =
  { status : CellStatus
  }

type CellStatus
  = Clear
  | Block
  | Wall

type alias Position =
    { x : Int
    , y : Int
    }

createCell : Int -> Int -> Cell
createCell y x =
  Cell Clear

createRow : Int -> Int -> Row
createRow y xLen =
    Array.initialize xLen (\x -> createCell y x)

createGrid : Int -> Int -> Grid
createGrid yLen xLen =
    Array.initialize yLen (\y -> createRow y xLen)

getCell : Int -> Int -> Grid -> Maybe Cell
getCell y x grid =
    (Array.get y grid) `andThen` (Array.get x)


setCellInRow : Int -> Cell -> Row -> Row
setCellInRow x cell row =
    Array.set x cell row


setCell : Cell -> Int -> Int -> Grid -> Grid
setCell cell y x grid =
    let
        row = (Array.get y grid)
    in
        case row of
            Nothing ->
                grid
            Just r ->
                Array.set y (setCellInRow x cell r) grid

setWall : Int -> Int -> Grid -> Grid
setWall y x grid =
    setCell (Cell Wall) y x grid

setBlock : Int -> Int -> Grid -> Grid
setBlock y x grid =
    setCell (Cell Block) y x grid

type alias Model =
    { grid: Grid
    , position: Position
    }

model : Model
model =
    let
        position = Position 7 5
        grid = createGrid 10 10
            |> setWall 1 1
            |> setWall 1 2
            |> setWall 1 3
            |> setWall 2 3
            |> setBlock 3 3
            |> setBlock 4 4
            |> setBlock 5 5
    in
        Model grid position



-- UPDATE


type Msg
    = Up
    | Down
    | Left
    | Right


update : Msg -> Model -> Model
update msg model =
    case msg of
    Up ->
        model

    Down ->
        model

    Left ->
        model

    Right ->
        model



-- VIEW


view : Model -> Html Msg
view model =
    div [] [
        tgrid model.position model.grid
    ]

tgrid : Position -> Grid -> Html Msg
tgrid pos grid =
  let
    rows = Array.toList grid
        |> List.map (trow pos)
  in
    table [class "grid"] rows

trow : Position -> Row -> Html Msg
trow pos row =
    let
        cells = Array.toList row
            |> List.map (tcell pos)
    in
        tr [] cells

tcell : Position -> Cell -> Html Msg
tcell pos cell =
    case cell.status of
        Clear ->
            td [ style [("backgroundColor", "#eee"), ("width", "20px"), ("height", "20px")] ] [ ]
        Block ->
            td [ style [("backgroundColor", "red"), ("width", "20px"), ("height", "20px")] ] [ ]
        Wall ->
            td [ style [("backgroundColor", "#333"), ("width", "20px"), ("height", "20px")] ] [ ]
