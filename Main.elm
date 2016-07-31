import Array exposing (Array)
import Char
import List exposing (map)
import String
import Maybe exposing (withDefault, andThen)
import Debug exposing (log)
import Platform.Cmd exposing (..)


import Html.App as Html
import Html.Events exposing (onClick)
import Matrix exposing (..)
import Keyboard.Extra exposing (..)

import Collage exposing (collage, move, toForm, rotate, scale, Form)
import Element exposing (image, Element)

import Time exposing (Time, millisecond)
import AnimationFrame
import Assets
import Model exposing (WorldCell, WallType, Item, FloorType, PackageType, Grid, Direction, Ornament, Model, removeItem, setItem, moveItem, wrap)
import Levels exposing (..)
import View exposing (view)
import Update exposing (..)

main =
    Html.program
    { init = init Levels.level3
    , view = view
    , update = Update.update
    , subscriptions = subscriptions
    }

-- MODEL

init : (Location, Grid, List Ornament) -> ( Model.Model, Cmd Update.Msg )
init (position, grid, ornaments) =
    let
        ( keyboardModel, keyboardCmd ) = Keyboard.Extra.init
        direction = Direction 0 1
    in
        ( Model.Model keyboardModel 0 grid position direction ornaments
        , Cmd.batch
            [ Cmd.map Update.KeyboardExtraMsg keyboardCmd
            ]
        )

-- SUBSCRIPTIONS
subscriptions : Model.Model -> Sub Update.Msg
subscriptions model =
    Sub.batch
        [ Sub.map Update.KeyboardExtraMsg Keyboard.Extra.subscriptions
        , AnimationFrame.times Tick
        ]
