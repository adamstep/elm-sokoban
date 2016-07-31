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
import Task

import Collage exposing (collage, move, toForm, rotate, scale, Form)
import Element exposing (image, Element)

import Time exposing (Time, millisecond)
import Assets
import Level.Model exposing (WorldCell, WallType, Item, FloorType, PackageType, Grid, Direction, Ornament, Model, removeItem, setItem, moveItem, wrap , init)
import Level.Levels as Levels
import Level.View exposing (view)
import Level.Update exposing (Msg, move, update, subscriptions)

main =
    Html.program
    { init = init Levels.level3
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
    { levelModel: Level.Model.Model
    , keyboardModel: Keyboard.Extra.Model
    , keyboardDisabled: Bool
    }

init : (Location, Grid, List Ornament) -> ( Model, Cmd Msg )
init (position, grid, ornaments) =
    let
        ( keyboardModel, keyboardCmd ) = Keyboard.Extra.init
        levelModel = Level.Model.init (position, grid, ornaments)
    in
        ( Model levelModel keyboardModel False
        , Cmd.batch
            [ Cmd.map KeyboardExtraMsg keyboardCmd
            ]
        )

-- UPDATE

type Msg
    = KeyboardExtraMsg Keyboard.Extra.Msg
    | LevelMsg Level.Update.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
    KeyboardExtraMsg keyMsg ->
        let
            ( keyboardModel, keyboardCmd ) =
                Keyboard.Extra.update keyMsg model.keyboardModel
            direction = Keyboard.Extra.arrows keyboardModel

            task = case model.keyboardDisabled of
                False -> Task.perform LevelMsg LevelMsg (Task.succeed (Level.Update.move direction))
                True -> Cmd.none
        in
            ( model, Cmd.batch
                [ task
                , Cmd.map KeyboardExtraMsg keyboardCmd
                ]
            )

    LevelMsg msg ->
        ( { model | levelModel = Level.Update.update msg model.levelModel }, Cmd.none )

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardExtraMsg Keyboard.Extra.subscriptions
        , Sub.map LevelMsg Level.Update.subscriptions
        ]

-- VIEW
view : Model -> Html Msg
view model =
    Level.View.view model.levelModel
