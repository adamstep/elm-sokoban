module View exposing (view)

import String
import Maybe exposing (withDefault, andThen)
import Model exposing (WorldCell, WallType, Item, FloorType, PackageType, Grid, Direction, Ornament, Model, removeItem, setItem, moveItem, wrap)
import Html exposing (Html, Attribute, button, div, text, table, tr, td)
import Html.Attributes exposing (style, title, class, classList, attribute, href)

import Collage exposing (collage, move, toForm, rotate, scale, Form)
import Element exposing (image, Element)
import Assets exposing(..)
import Ease
import Matrix exposing (..)

import Update exposing(Msg)

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
        "1001" -> toForm Assets.wallBottomLeft
        "1010" -> toForm Assets.wallRightLeft
        "1000" -> toForm Assets.wallBottomRightLeft

        "1111" -> toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_169.png")
        "1101" -> toForm Assets.wallBottom
        "1110" -> toForm Assets.wallRight
        "1100" -> toForm Assets.wallBottomRight

        "0011" -> toForm Assets.wallTopLeft
        "0001" -> toForm Assets.wallTopBottomLeft
        "0010" -> toForm Assets.wallTopRightLeft
        "0000" -> toForm (image 64 64 "/assets/topdown-shooter/PNG/Tiles/tile_171.png")

        "0111" -> toForm Assets.wallTop
        "0101" -> toForm Assets.wallTopBottom
        "0110" -> toForm Assets.wallTopRight
        "0100" -> toForm Assets.wallTopRightBottom

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
