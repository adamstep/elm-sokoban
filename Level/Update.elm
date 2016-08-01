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
                playerDirection = if direction == noDirection then
                    model.direction
                else
                    direction

                oldPos = model.position
                updatedModel = updateLoc direction model
                newPos = updatedModel.position

                samePos = ((fst oldPos) == (fst newPos)) && ((snd oldPos) == (snd newPos))

                numMoves = if samePos then
                    model.numMoves
                else
                    model.numMoves + 1
            in
                { updatedModel | direction = playerDirection, numMoves = numMoves }
            
        Tick newTime ->
            { model | counter = (model.counter + 1) % 200 }

subscriptions : Sub Msg
subscriptions =
    AnimationFrame.times Tick
