import Array exposing (Array)
import Char
import List exposing (map)
import String
import Maybe exposing (withDefault, andThen)
import Debug exposing (log)
import Platform.Cmd exposing (..)

import Html exposing (Html)
import Html.App as Html
import Html.Events exposing (onClick)
import Matrix exposing (..)
import Keyboard.Extra exposing (..)

import Collage exposing (collage, move, toForm, rotate, scale, Form)
import Element exposing (image, Element)

import Time exposing (Time, millisecond)
import AnimationFrame
import Assets
import Model exposing (WorldCell, WallType, Item, FloorType, PackageType, Grid, Direction, Ornament, Model, removeItem, setItem, moveItem, wrap , init)
import Levels exposing (..)
import View exposing (view)

main =
    Html.program
    { init = init Levels.level3
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Level =
    { model: Model.Model
    , keyboardModel: Keyboard.Extra.Model
    }

init : (Location, Grid, List Ornament) -> ( Level, Cmd Msg )
init (position, grid, ornaments) =
    let
        ( keyboardModel, keyboardCmd ) = Keyboard.Extra.init
        model = Model.init (position, grid, ornaments)
    in
        ( Level model keyboardModel
        , Cmd.batch
            [ Cmd.map KeyboardExtraMsg keyboardCmd
            ]
        )

-- UPDATE

type Msg
    = KeyboardExtraMsg Keyboard.Extra.Msg
    | Tick Time

update : Msg -> Level -> (Level, Cmd Msg)
update msg level =
    case msg of
    KeyboardExtraMsg keyMsg ->
        let
            ( keyboardModel, keyboardCmd ) =
                Keyboard.Extra.update keyMsg level.keyboardModel
            direction = Keyboard.Extra.arrows keyboardModel
            playerDirection = if direction == Model.noDirection then level.model.direction else direction
            updatedModel = Model.updateLoc direction level.model
            newModel = { updatedModel | direction = playerDirection }
        in
            ( { level | keyboardModel = keyboardModel, model = newModel }
                , Cmd.map KeyboardExtraMsg keyboardCmd
                )

    Tick newTime ->
        let
            model = level.model
            newModel = { model | counter = (model.counter + 1) % 200 }
        in
        ({ level | model = newModel }, Cmd.none)

-- SUBSCRIPTIONS
subscriptions : Level -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardExtraMsg Keyboard.Extra.subscriptions
        , AnimationFrame.times Tick
        ]

-- VIEW
view : Level -> Html Msg
view level =
    View.view level.model
