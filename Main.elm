import Array exposing (Array)
import Char
import List exposing (map)
import String
import Maybe exposing (withDefault, andThen)
import Debug exposing (log)
import Platform.Cmd exposing (..)

import Html exposing (Html, Attribute, button, div, text, table, tr, td)
import Html.Attributes exposing (style, title, class, classList, attribute, href)
import Html.App as Html
import Html.Events exposing (onClick)
import Matrix exposing (..)
import Keyboard.Extra exposing (..)

main =
    Html.program
    { init = init level4
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type WorldCell
  = Wall
  | None
  | Floor Item
  | Goal Item

type Item
  = Empty
  | Package
  | Player

type alias Grid = Matrix WorldCell

type alias Direction = { x : Int, y : Int}

type alias Model =
    { keyboardModel : Keyboard.Extra.Model
    , grid: Grid
    , position: Location
    }

type alias LevelFile =
    { name : String
    , width: Int
    , height: Int
    , text : List String
    }

level1 =
    LevelFile "Level 1" 10 10 [
        "   ####",
        "   #  #",
        "   #  #",
        "   #$.#",
        "####  #",
        "#     #",
        "#@$.  #",
        "#######"
    ]

level6 =
    LevelFile "Level 6" 11 11 [
        "###########",
        "#        @#",
        "# #$ $ $  #",
        "# # $$$$  #",
        "# #$ $ $  #",
        "# #     $ #",
        "# #$### ###",
        "# # #.....#",
        "# # #.   .#",
        "#    .....#",
        "###########"
    ]

level4 =
    LevelFile "Level 6" 8 11 [
        " ########  ",
        " # @#   #  ",
        " # $  $ #  ",
        " #$ #######",
        "## $#     #",
        "#. . .    #",
        "#.  #######",
        "#####      "
    ]

parseLevelFile : LevelFile -> (Location, Grid)
parseLevelFile file =
    let
        charMatrix = Matrix.fromList (List.map (\a -> String.toList a) file.text)
        charToCell = \c -> case c of
            '#' -> Wall
            '$' -> Floor Package
            '@' -> Floor Player
            ' ' -> Floor Empty
            '*' -> Goal Package
            '+' -> Goal Player
            '.' -> Goal Empty
            _ -> None

        charToPlayerLoc = \loc c -> case c of
            '@' -> Just loc
            '+' -> Just loc
            _ -> Nothing
            
        grid = Matrix.map charToCell charMatrix
        position = Matrix.mapWithLocation charToPlayerLoc charMatrix
            |> Matrix.flatten
            |> List.filter (\c -> c /= Nothing)
            |> List.head
            |> withDefault (Just (loc 0 0))
            |> withDefault (loc 0 0)
    in
        (position, grid)

init : LevelFile -> ( Model, Cmd Msg )
init levelFile =
    let
        ( keyboardModel, keyboardCmd ) = Keyboard.Extra.init
        ( position, grid ) = parseLevelFile levelFile
    in
        ( Model keyboardModel grid position
        , Cmd.batch
            [ Cmd.map KeyboardExtraMsg keyboardCmd
            ]
        )

setNone : Location -> Grid -> Grid
setNone loc grid =
    set loc None grid

setWall : Location -> Grid -> Grid
setWall loc grid =
    set loc Wall grid

setGoal : Location -> Grid -> Grid
setGoal loc grid =
    set loc (Goal Empty) grid

setPackage : Location -> Grid -> Grid
setPackage loc grid =
    let
        cell = withDefault (Floor Empty) (get loc grid)
        val = case cell of
            Floor Empty -> Floor Package
            Goal Empty -> Goal Package
            _ -> cell
    in
        set loc val grid

setPlayer : Location -> Grid -> Grid
setPlayer loc grid =
    let
        cell = withDefault (Floor Empty) (get loc grid)
        val = case cell of
            Floor Empty -> Floor Player
            Goal Empty -> Goal Player
            _ -> cell
    in
        set loc val grid

numGoals model =
    List.length (List.filter (\a -> a == Goal Empty || a == Goal Player) (flatten model.grid))

numFullGoals model =
    List.length (List.filter (\a -> a == Goal Package) (flatten model.grid))

-- UPDATE
type Msg
    = KeyboardExtraMsg Keyboard.Extra.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
    KeyboardExtraMsg keyMsg ->
        let
            ( keyboardModel, keyboardCmd ) =
                Keyboard.Extra.update keyMsg model.keyboardModel
            direction = Keyboard.Extra.arrows keyboardModel
            newModel = updateLoc direction model
        in
            ( { newModel | keyboardModel = keyboardModel }
                , Cmd.map KeyboardExtraMsg keyboardCmd
                )

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

            Floor Package ->
                moveItem Package newLoc newPackageLoc model.grid
                `andThen`
                moveItem Player model.position newLoc

            Goal Package ->
                moveItem Package newLoc newPackageLoc model.grid
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


removeItem: Location -> Grid -> Maybe Grid
removeItem loc grid =
    let 
        cell = withDefault None (get loc grid)
        newCell = case cell of
            Floor i ->
                Floor Empty
            Goal i ->
                Goal Empty
            _ ->
                cell
    in
        Just (set loc newCell grid)

setItem: Location -> Item -> Grid -> Maybe Grid
setItem loc item grid =
    let
        cell = withDefault None (get loc grid)
        newCell = case cell of
            Floor Empty ->
                Just (Floor item)
            Goal Empty ->
                Just (Goal item)
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
    setItem toLoc item grid `andThen` removeItem fromLoc

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map KeyboardExtraMsg Keyboard.Extra.subscriptions

-- VIEW
view : Model -> Html Msg
view model =
    div []
        [ tcells model
          , div [ ] [ text ("num empty: " ++ toString (numGoals model)) ]
          , div [ ] [ text ("num full: " ++ toString (numFullGoals model)) ]
        ]

tcells: Model -> Html Msg
tcells model =
  let
    cells = mapWithLocation tcell model.grid
    rows = List.map (\r -> tr [] r) (Matrix.toList cells)
  in
    table [class "grid", style [("backgroundColor", "#ddd"), ("fontSize", "30px"), ("textAlign", "center"), ("lineHeight", "40px")]] rows

playerCell =
   td [style [("backgroundColor", "#eee"), ("width", "40px"), ("height", "40px")] ] [ text "👷" ] 


wellWithPlayerCell =
   td [style [("backgroundColor", "lightblue"), ("width", "40px"), ("height", "40px")] ] [ text "👷" ] 

wallCell =
    td [ style [("backgroundColor", "#666"), ("width", "40px"), ("height", "40px")] ] [ text "⬛" ]


emptyCell =
    td [ style [("backgroundColor", "white"), ("width", "40px"), ("height", "40px")] ] [ ]

boxCell =
    td [ style [("backgroundColor", "#eee"), ("width", "40px"), ("height", "40px")] ] [ text "📦" ]

wellCell =
    td [ style [("backgroundColor", "lightblue"), ("width", "40px"), ("height", "40px")] ] [ text "🚢" ]


wellWithBoxCell =
    td [ style [("backgroundColor", "lightblue"), ("width", "40px"), ("height", "40px")] ] [ text "📦" ]

clearCell =
    td [ style [("backgroundColor", "#eee"), ("width", "40px"), ("height", "40px")] ] [ ]

baseStyles =
    [("width", "40px"), ("height", "40px")]

tcell : Location -> WorldCell -> Html Msg
tcell pos cell =
    case cell of
        Floor Empty ->
            clearCell
        Floor Package ->
            boxCell
        Floor Player ->
            playerCell
        Wall ->
            wallCell
        None ->
            emptyCell
        Goal Empty ->
            wellCell
        Goal Package ->
            wellWithBoxCell
        Goal Player ->
            wellWithPlayerCell
