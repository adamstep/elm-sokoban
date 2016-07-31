module Level.Update exposing (Msg, move, update, subscriptions)

import Platform.Cmd exposing (..)
import Time exposing (Time, millisecond)

import Level.Model exposing (Model, Direction, noDirection, updateLoc)
import Level.View as View exposing (view)

import AnimationFrame

type Msg
    = Tick Time
    | Move Direction

move dir =
    Move dir

update : Msg -> Model -> Model
update msg model =
    case msg of
        Move direction ->
            let
                playerDirection = if direction == noDirection then model.direction else direction
                updatedModel = updateLoc direction model
            in
                { updatedModel | direction = playerDirection }
            
        Tick newTime ->
            { model | counter = (model.counter + 1) % 200 }

subscriptions : Sub Msg
subscriptions =
    AnimationFrame.times Tick
