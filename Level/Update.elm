module Level.Update exposing (Msg(..), move, update, subscriptions)

import Platform.Cmd exposing (..)

import Level.Model exposing (Model, Direction, noDirection, updateLoc, levelComplete)
import Level.View as View exposing (view)
import Task exposing (Task, andThen)
import Process
import Time

type Msg
    = Move Direction
    | LevelFinished

move dir =
    Move dir


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LevelFinished ->
            (model, Cmd.none)
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

                f = \_ -> LevelFinished
                t = (Process.sleep (0.5 * Time.second)) `andThen` Task.succeed
                cmd =
                    if (levelComplete model) then
                        Task.perform f f t
                    else
                        Cmd.none
            in
                ( { updatedModel | direction = playerDirection, numMoves = numMoves }
                , cmd
                )
            
subscriptions : Sub Msg
subscriptions =
    Sub.batch []
