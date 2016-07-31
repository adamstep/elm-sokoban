module Update exposing (..)
import Keyboard.Extra exposing (..)
import Model exposing (Model, noDirection, updateLoc)
import Time exposing (Time)

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
            newModel = Model.updateLoc direction model
            playerDirection = if direction == Model.noDirection then model.direction else direction
        in
            ( { newModel | keyboardModel = keyboardModel, direction = playerDirection }
                , Cmd.map KeyboardExtraMsg keyboardCmd
                )

    Tick newTime ->
        ({ model | counter = (model.counter + 1) % 200 }, Cmd.none)
