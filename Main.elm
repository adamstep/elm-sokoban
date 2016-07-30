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

import Collage exposing (collage, move, toForm, rotate, Form)
import Element exposing (image, Element)

import Random exposing (map, int, step, initialSeed)

main =
    Html.program
    { init = init level3
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

noDirection : Direction
noDirection =
    Direction 0 0

type alias Model =
    { keyboardModel : Keyboard.Extra.Model
    , grid: Grid
    , position: Location
    , direction: Direction
    , ornaments: List Ornament
    }

type alias Ornament = 
    { position: Location
      , width: Int
      , height: Int
      , degrees: Float
      , image: String
    }

type alias LevelFile =
    { name : String
    , text : List String
    , ornaments : List Ornament
    }

level1 =
    LevelFile 
        "Level 1"
        [ "xxx####"
        , "xxx#  #"
        , "xxx#  #"
        , "xxx#$.#"
        , "####  #"
        , "#     #"
        , "#@$.  #"
        , "#######"
        ]
        []

level2 =
    LevelFile
        "Level 6"
        [ "###########"
        , "#        @#"
        , "# #$ $ $  #"
        , "# # $$$$  #"
        , "# #$ $ $  #"
        , "# #     $ #"
        , "# #$### ###"
        , "# # #.....#"
        , "# # #.   .#"
        , "#    .....#"
        , "###########"
        ]
        []

level3 =
    LevelFile
        "Level 6"
        [ "xxxxxxxxxxxxx"
        , "xx#########xx"
        , "xx# @#   ##xx"
        , "xx# $  $ ##xx"
        , "xx#$ #######x"
        , "x## $#     #x"
        , "x#. . .    #x"
        , "x#.  #######x"
        , "x#####xxxxxxx"
        , "xxxxxxxxxxxxx"
        ]
        [ Ornament (loc 5 1) 64 64 0 "/assets/topdown-shooter/PNG/Tiles/tile_142.png"
        , Ornament (loc 6 1) 64 64 0 "/assets/topdown-shooter/PNG/Tiles/tile_07.png"
        , Ornament (loc 6 1) 64 64 0 "/assets/topdown-shooter/PNG/Tiles/tile_488.png"
        , Ornament (loc 7 1) 64 64 0 "/assets/topdown-shooter/PNG/Tiles/tile_07.png"
        , Ornament (loc 7 1) 64 64 0 "/assets/topdown-shooter/PNG/Tiles/tile_488.png"
        , Ornament (loc 8 1) 64 64 0 "/assets/topdown-shooter/PNG/Tiles/tile_142.png"

        , Ornament (loc 7 6) 64 64 0 "/assets/topdown-shooter/PNG/Tiles/tile_07.png"
        , Ornament (loc 8 6) 64 64 0 "/assets/topdown-shooter/PNG/Tiles/tile_07.png"
        , Ornament (loc 9 6) 64 64 0 "/assets/topdown-shooter/PNG/Tiles/tile_07.png"
        , Ornament (loc 7 6) 64 64 0 "/assets/topdown-shooter/PNG/Tiles/tile_470.png"
        , Ornament (loc 7 5) 64 64 0 "/assets/topdown-shooter/PNG/Tiles/tile_141.png"
        , Ornament (loc 7 7) 64 64 0 "/assets/topdown-shooter/PNG/Tiles/tile_142.png"


        , Ornament (loc 8 7) 64 64 0 "/assets/topdown-shooter/PNG/Tiles/tile_183.png"
        , Ornament (loc 9 5) 64 64 0 "/assets/topdown-shooter/PNG/Tiles/tile_183.png"

        , Ornament (loc 1 9) 64 64 0 "/assets/topdown-shooter/PNG/Tiles/tile_111.png"
        , Ornament (loc 2 10) 64 64 0 "/assets/topdown-shooter/PNG/Tiles/tile_138.png"
        , Ornament (loc 3 10) 64 64 0 "/assets/topdown-shooter/PNG/Tiles/tile_138.png"
        , Ornament (loc 2 9) 64 64 90 "/assets/topdown-shooter/PNG/Tiles/tile_324.png"
        , Ornament (loc 3 9) 64 64 90 "/assets/topdown-shooter/PNG/Tiles/tile_323.png"
        , Ornament (loc 4 9) 64 64 0 "/assets/topdown-shooter/PNG/Tiles/tile_111.png"
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
        direction = Direction 0 1
    in
        ( Model keyboardModel grid position direction levelFile.ornaments
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
            playerDirection = if direction == noDirection then model.direction else direction
        in
            ( { newModel | keyboardModel = keyboardModel, direction = playerDirection }
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
        [ renderBoard model
          , div [ ] [ text ("num empty: " ++ toString (numGoals model)) ]
          , div [ ] [ text ("num full: " ++ toString (numFullGoals model)) ]
        ]

type alias NeighborCells =
    { north: WorldCell
    , east: WorldCell
    , south: WorldCell
    , west: WorldCell
    }

wallMap : NeighborCells -> String
wallMap neighbors =
    let
        toChar = \n ->
            case n of
                Wall -> '1'
                _ -> '0'
    in
        String.fromList (List.map toChar [neighbors.north, neighbors.east, neighbors.south, neighbors.west])

defaultTile =
    toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_171.png")

wallTile : WorldCell -> NeighborCells -> Collage.Form
wallTile cell neighbors =
    case wallMap neighbors of
        "1011" -> toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_167.png")
        "1001" -> toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_136.png")
        "1010" -> toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_138.png")
        "1000" -> toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_115.png")

        "1111" -> toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_169.png")
        "1101" -> toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_113.png")
        "1110" -> toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_140.png")
        "1100" -> toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_137.png")

        "0011" -> toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_109.png")
        "0001" -> toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_142.png")
        "0010" -> toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_141.png")
        "0000" -> defaultTile

        "0111" -> toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_112.png")
        "0101" -> toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_111.png")
        "0110" -> toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_110.png")
        "0100" -> toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_114.png")

        _ -> toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_171.png")

playerTile : Direction -> Collage.Form
playerTile dir =
    let
        x = dir.x
        y = dir.y
        deg = case (x, y) of
            (1, 0) -> (degrees 0)
            (0, 1) -> (degrees 90)
            (-1, 0) -> (degrees 180)
            (0, -1) -> (degrees 270)
            _ -> (degrees 0)
    in
        toForm (image 33 43 "/assets/topdown-shooter/PNG/Man Old/manOld_hold.png")
        |> rotate deg


floorTile : Collage.Form
floorTile =
    toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_11.png")

tile : WorldCell -> NeighborCells -> Direction -> Collage.Form
tile cell neighbors playerDir =
    case cell of
        Wall ->
            wallTile cell neighbors
        None ->
            toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_01.png")
        Floor Empty ->
            floorTile
        Floor Package ->
            toForm (collage 64 64
                [ floorTile
                , toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_129.png")
                ])
        Floor Player ->
            toForm (collage 64 64
                [ floorTile
                , playerTile playerDir
                ])
        Goal Empty ->
            toForm (collage 64 64
                [ floorTile
                , toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_133.png")
                ])
        Goal Package ->
            toForm (collage 64 64
                [ floorTile
                , toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_129.png")
                , toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_132.png")
                ])
        Goal Player ->
            toForm (collage 64 64
                [ floorTile
                , toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_133.png")
                , playerTile playerDir
                ])

renderBoard : Model -> Html Msg
renderBoard model =
    let
        numRows = rowCount model.grid
        numCols = colCount model.grid
        height = 64 * numRows
        width = 64 * numCols

        cellForm = \l c ->
            let
                x = (snd l) * 64
                y = (fst l) * 64
                posX = (fst l)
                posY = (snd l)
                neighbors = (NeighborCells
                    (withDefault None (get (loc (posX - 1) posY) model.grid)))
                    (withDefault None (get (loc posX (posY - 1)) model.grid))
                    (withDefault None (get (loc (posX + 1) posY) model.grid))
                    (withDefault None (get (loc posX (posY + 1)) model.grid))
            in
                tile c neighbors model.direction
                |> move (toFloat x, toFloat -y)
                |> move (toFloat -width / 2, toFloat height / 2)


        ornamentForm = \o ->
            let
                x = (snd o.position) * 64
                y = (fst o.position) * 64
            in
                (image o.width o.height o.image)
                |> toForm
                |> rotate (degrees o.degrees)
                |> move (toFloat x, toFloat -y)
                |> move (toFloat -width / 2, toFloat height / 2)
                

        gameForms = mapWithLocation cellForm model.grid
        ornamentForms = List.map ornamentForm model.ornaments
        tiles = collage width height (List.append (flatten gameForms) ornamentForms)
    in
        Element.toHtml tiles
