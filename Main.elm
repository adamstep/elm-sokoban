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

import Collage exposing (collage, move, toForm, rotate, scale, Form)
import Element exposing (image, Element)

import Time exposing (Time, millisecond)
import AnimationFrame
import Ease
import Assets
import Model exposing (WorldCell, WallType, Item, FloorType, PackageType, Grid, Direction, Model, removeItem, setItem, moveItem, wrap)
import Levels exposing (..)

main =
    Html.program
    { init = init Levels.level3File
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

init : LevelFile -> ( Model.Model, Cmd Msg )
init levelFile =
    let
        ( keyboardModel, keyboardCmd ) = Keyboard.Extra.init
        ( position, grid ) = Levels.parseLevelFile levelFile
        direction = Direction 0 1
    in
        ( Model.Model keyboardModel 0 grid position direction levelFile.ornaments
        , Cmd.batch
            [ Cmd.map KeyboardExtraMsg keyboardCmd
            ]
        )

-- UPDATE
type Msg
    = KeyboardExtraMsg Keyboard.Extra.Msg
    | Tick Time

update : Msg -> Model.Model -> (Model.Model, Cmd Msg)
update msg model =
    case msg of
    KeyboardExtraMsg keyMsg ->
        let
            ( keyboardModel, keyboardCmd ) =
                Keyboard.Extra.update keyMsg model.keyboardModel
            direction = Keyboard.Extra.arrows keyboardModel
            newModel = updateLoc direction model
            playerDirection = if direction == Model.noDirection then model.direction else direction
        in
            ( { newModel | keyboardModel = keyboardModel, direction = playerDirection }
                , Cmd.map KeyboardExtraMsg keyboardCmd
                )

    Tick newTime ->
        ({ model | counter = (model.counter + 1) % 200 }, Cmd.none)

updateLoc: Model.Direction -> Model.Model -> Model.Model
updateLoc dir model =
    let
        newLoc = wrap model.position dir model.grid
        newPackageLoc = wrap newLoc dir model.grid
        cell = withDefault Model.None (Matrix.get newLoc model.grid)
        newGrid = case cell of
            Model.Wall ->
                Nothing

            Model.None ->
                Nothing

            Model.Floor (Model.Package p) t ->
                moveItem (Model.Package p) newLoc newPackageLoc model.grid
                `andThen`
                moveItem Model.Player model.position newLoc

            Model.Goal (Model.Package p) t ->
                moveItem (Model.Package p) newLoc newPackageLoc model.grid
                `andThen`
                moveItem Model.Player model.position newLoc

            _ ->
                moveItem Model.Player model.position newLoc model.grid
    in
        case newGrid of
            Nothing ->
                model
            Just grid ->
                { model | grid=grid, position=newLoc }




-- SUBSCRIPTIONS
subscriptions : Model.Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardExtraMsg Keyboard.Extra.subscriptions
        , AnimationFrame.times Tick
        ]

-- VIEW
view : Model.Model -> Html Msg
view model =
    div []
        [ renderBoard model
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
                Model.Wall -> '1'
                _ -> '0'
    in
        String.fromList (List.map toChar [neighbors.north, neighbors.east, neighbors.south, neighbors.west])

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
        "0000" -> toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_171.png")

        "0111" -> toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_112.png")
        "0101" -> toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_111.png")
        "0110" -> toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_110.png")
        "0100" -> toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_114.png")

        _ -> toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_171.png")

playerTile : Model.Direction -> Collage.Form
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
        toForm Assets.playerHold
        |> rotate deg


floorTile : FloorType -> Collage.Form
floorTile t =
    case t of
        Model.Tile -> toForm Assets.slate
        Model.Wood -> toForm Assets.wood

packageTile : PackageType -> Collage.Form
packageTile p =
    case p of
        Model.Box ->
            toForm Assets.box
        Model.SmallBox ->
            toForm Assets.smallBox
        Model.Television ->
            toForm Assets.television
        Model.Chair ->
            toForm Assets.chair
        Model.Plant ->
            toForm Assets.plant

ease time =
    if time < 0.5 then
        Ease.inOutQuad(time * 2)
    else
        (Ease.reverse Ease.inOutQuad) ((time - 0.5) * 2)

goalIndicator : Int -> Collage.Form
goalIndicator counter =
    let
        s = (toFloat counter) / 200
        sf = ease s
    in
        toForm Assets.goal
        |> scale ((sf / 5) + 0.8)

checkmark : Int -> Collage.Form
checkmark counter =
    let
        s = (toFloat counter) / 200
        sf = ease s
    in
        toForm (image 21 20 "/assets/uipack_fixed/PNG/green_checkmark.png")
        |> scale ((sf / 5) + 0.8)

tile : WorldCell -> NeighborCells -> Model.Model -> Collage.Form
tile cell neighbors model =
    case cell of
        Model.Wall ->
            wallTile cell neighbors
        Model.None ->
            toForm Assets.none
        Model.Floor Model.Empty t ->
            floorTile t
        Model.Floor (Model.Package p) t ->
            toForm (collage 64 64
                [ floorTile t
                , packageTile p
                ])
        Model.Floor Model.Player t->
            toForm (collage 64 64
                [ floorTile t
                , playerTile model.direction
                ])
        Model.Goal Model.Empty t ->
            toForm (collage 64 64
                [ floorTile t
                , goalIndicator model.counter
                ])
        Model.Goal (Model.Package p) t ->
            toForm (collage 64 64
                [ floorTile t
                , packageTile p
                , checkmark model.counter
                ])
        Model.Goal Model.Player t ->
            toForm (collage 64 64
                [ floorTile t
                , goalIndicator model.counter
                , playerTile model.direction
                ])

renderBoard : Model.Model -> Html Msg
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
                    (withDefault Model.None (get (loc (posX - 1) posY) model.grid)))
                    (withDefault Model.None (get (loc posX (posY - 1)) model.grid))
                    (withDefault Model.None (get (loc (posX + 1) posY) model.grid))
                    (withDefault Model.None (get (loc posX (posY + 1)) model.grid))
            in
                tile c neighbors model
                |> move (toFloat x, toFloat -y)
                |> move (toFloat -width / 2, toFloat height / 2)


        ornamentForm = \o ->
            let
                x = (snd o.position) * 64
                y = (fst o.position) * 64
            in
                toForm o.element
                |> rotate (degrees o.degrees)
                |> move (toFloat x, toFloat -y)
                |> move (toFloat -width / 2, toFloat height / 2)
                

        gameForms = mapWithLocation cellForm model.grid
        ornamentForms = List.map ornamentForm model.ornaments
        tiles = collage width height (List.append (flatten gameForms) ornamentForms)
    in
        Element.toHtml tiles
