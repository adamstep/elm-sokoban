import Array exposing (Array)
import Char
import List exposing (map)
import String
import Maybe exposing (withDefault, andThen)
import Debug exposing (log)
import Platform.Cmd exposing (..)

import Html exposing (Html, Attribute, div, button, text)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events exposing (onClick, onMouseDown, onMouseUp)
import Matrix exposing (..)
import Keyboard.Extra exposing (..)
import Task

import Collage exposing (collage, move, toForm, rotate, scale, Form)
import Element exposing (image, Element)

import Time exposing (Time, millisecond)
import Assets
import Level.Model exposing (WorldCell, WallType, Item, FloorType, PackageType, Grid, Direction, Ornament, Model, removeItem, setItem, moveItem, wrap , init)
import Level.Levels as Levels
import Level.View exposing (view, Model, Msg, init, update, subscriptions)
import Level.Update exposing (Msg(..), move, update, subscriptions)
import LevelPicker exposing (Model, Msg(..), view, update, init)
import LevelComplete exposing (Model, Msg(..), view, update, init)

main =
    Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
    { levelModel: Level.Model.Model
    , viewModel: Level.View.Model
    , currentLevel: Levels.AvailableLevels
    , levelPickerModel: LevelPicker.Model
    , levelCompleteModel: LevelComplete.Model
    , keyboardModel: Keyboard.Extra.Model
    , keyboardDisabled: Bool
    , pickingLevel: Bool
    , menuPressed: Bool
    , resetPressed: Bool
    , levelComplete: Bool
    }

init : ( Model, Cmd Msg )
init =
    let
        ( keyboardModel, keyboardCmd ) = Keyboard.Extra.init
        currentLevel = Levels.Level1
        viewModel = Level.View.init
        levelModel = Level.Model.init (Levels.getLevel currentLevel)
        levelPickerModel = LevelPicker.init
        levelCompleteModel = LevelComplete.init
    in
        ( Model levelModel viewModel currentLevel levelPickerModel levelCompleteModel keyboardModel False False False False False
        , Cmd.batch
            [ Cmd.map KeyboardExtraMsg keyboardCmd
            ]
        )

-- UPDATE

type Msg
    = KeyboardExtraMsg Keyboard.Extra.Msg
    | LevelMsg Level.Update.Msg
    | LevelViewMsg Level.View.Msg
    | LevelPickerMsg LevelPicker.Msg
    | LevelCompleteMsg LevelComplete.Msg
    | MenuPressed Bool
    | ResetPressed Bool
    | PickLevel
    | ChangeLevel Levels.AvailableLevels

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
    KeyboardExtraMsg keyMsg ->
        let
            ( keyboardModel, keyboardCmd ) =
                Keyboard.Extra.update keyMsg model.keyboardModel
            direction = Keyboard.Extra.arrows keyboardModel

            cmd = case model.keyboardDisabled of
                False -> Task.perform LevelMsg LevelMsg (Task.succeed (Level.Update.move direction))
                True -> Cmd.none
        in
            ( model, Cmd.batch
                [ cmd
                , Cmd.map KeyboardExtraMsg keyboardCmd
                ]
            )

    LevelMsg msg ->
        let
            ( levelModel, levelCmd ) =
                Level.Update.update msg model.levelModel
            newModel = { model | levelModel = levelModel }
            newCmd = Cmd.map LevelMsg levelCmd

        in
            case msg of
                Level.Update.LevelFinished ->
                    ( { newModel | levelComplete = True, keyboardDisabled = True }, newCmd )
                _ ->
                    ( newModel, newCmd )


    LevelViewMsg msg ->
        ( { model | viewModel = Level.View.update msg model.viewModel }, Cmd.none )

    LevelCompleteMsg msg ->
        let
            newModel = { model | levelCompleteModel = LevelComplete.update msg model.levelCompleteModel }
        in
            case msg of
                LevelComplete.Replay ->
                    ( newModel
                    , Task.perform ChangeLevel ChangeLevel (Task.succeed model.currentLevel)
                    )

                LevelComplete.Next ->
                    ( newModel
                    , Task.perform ChangeLevel ChangeLevel (Task.succeed (Levels.nextLevel model.currentLevel))
                    )

                _ -> ( newModel, Cmd.none )

    LevelPickerMsg msg ->
        let
            newModel = { model | levelPickerModel = LevelPicker.update msg model.levelPickerModel }
        in
            case msg of
                LevelPicker.LevelClicked l ->
                    ( newModel, Task.perform ChangeLevel ChangeLevel (Task.succeed l) )

                LevelPicker.Cancel ->
                    ( { newModel | pickingLevel = False, keyboardDisabled = False }, Cmd.none )

                _ -> ( newModel, Cmd.none )

    PickLevel ->
        ( { model | pickingLevel = True, keyboardDisabled = True }, Cmd.none )

    MenuPressed pressed ->
        ( { model | menuPressed = pressed }, Cmd.none )

    ResetPressed pressed ->
        ( { model | resetPressed = pressed }, Cmd.none )

    ChangeLevel availableLevel ->
        let
          levelModel = Level.Model.init (Levels.getLevel availableLevel)
        in
        ( { model
            | pickingLevel = False
            , keyboardDisabled = False
            , levelModel = levelModel
            , currentLevel = availableLevel
            , levelComplete = False
          }
          , Cmd.none
        )

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardExtraMsg Keyboard.Extra.subscriptions
        , Sub.map LevelMsg Level.Update.subscriptions
        , Sub.map LevelViewMsg Level.View.subscriptions
        ]

-- VIEW
view : Model -> Html Msg
view model =
    let
        components =
            [ Html.map LevelMsg (Level.View.view model.viewModel model.levelModel)
            , controls model
            , levelPicker model
            , levelComplete model
            ]
    in
        div [ style
                [ ("position", "relative")
                , ("width", "100%")
                , ("height", "100%")
                , ("display", "flex")
                , ("justifyContent", "center")
                , ("alignItems", "center")
                , ("backgroundColor", "#999")
                , ("fontFamily", "sans-serif")
                ] 
            ]
            [ div [ style
                    [ ("position", "relative")
                    , ("width", "720px")
                    , ("height", "576px")
                    , ("display", "flex")
                    , ("justifyContent", "center")
                    , ("alignItems", "center")
                    , ("backgroundColor", "#222")
                    ]
                ]
                components
            ]

levelComplete model =
    if model.levelComplete then
        div [ style
                [ ("position", "absolute")
                , ("top", "0")
                , ("left", "0")
                , ("width", "100%")
                , ("height", "100%")
                , ("backgroundColor", "rgba(0,0,0,0.8)")
                ]
            ]
            [ Html.map LevelCompleteMsg (LevelComplete.view model.levelCompleteModel)
            ]
    else
        div [] []

levelPicker model =
    if model.pickingLevel then
        div [ style
                [ ("position", "absolute")
                , ("top", "0")
                , ("left", "0")
                , ("width", "100%")
                , ("height", "100%")
                , ("backgroundColor", "rgba(0,0,0,0.8)")
                ]
            ]
            [ Html.map LevelPickerMsg (LevelPicker.view model.levelPickerModel)
            ]
    else
        div [] []

stats model =
    div [ ]
        [ div [
                style
                  [ ("color", "#555")
                  , ("font-family", "sans-serif")
                  , ("width", "190px")
                  , ("height", "49px")
                  , ("background", "url('/assets/uipack_fixed/PNG/green_button13.png')")
                  , ("line-height", "45px")
                  , ("text-align", "center")
                  ]
              ]
              [ text ("Moves: " ++ (toString model.numMoves))
              ]
        ]

controls model =
    div [ style
            [ ("position", "absolute")
            , ("display", "flex")
            , ("flexDirection", "row")
            , ("flexWrap", "wrap")
            , ("justifyContent", "space-between")
            , ("alignItems", "center")
            , ("top", "0")
            , ("left", "0")
            , ("width", "700px")
            , ("padding", "10px")
            ]
        ]
        [ levelsButton model.menuPressed
        , resetButton model
        , stats model.levelModel
        ]

blueButtonStyles pressed =
    let
      styles = if pressed then
        [ ("background", "url('/assets/uipack_fixed/PNG/blue_button10.png')")
        , ("width", "49px")
        , ("height", "45px")
        , ("marginTop", "4px")
        , ("lineHeight", "45px")
        ]
      else
        [ ("background", "url('/assets/uipack_fixed/PNG/blue_button09.png')")
        , ("width", "49px")
        , ("height", "49px")
        , ("lineHeight", "49px")
        ]
    in
        styles ++
        [ ("font-family", "sans-serif")
        , ("color", "white")
        , ("textAlign", "center")
        , ("cursor", "pointer")
        ]

levelsButton pressed =
    div [ onClick PickLevel
        , onMouseDown (MenuPressed True)
        , onMouseUp (MenuPressed False)
        , style (blueButtonStyles pressed)
        ]
        [ div
            [ style
                [ ("background", "url('/assets/gameicons/PNG/White/1x/door.png')")
                , ("backgroundSize", "contain")
                , ("width", "40px")
                , ("height", "40px")
                , ("marginLeft", "5px")
                , ("marginTop", "1px")
                , ("opacity", "0.8")
                ]
            ] []
        ]

resetButton model =
    div [ onClick (ChangeLevel model.currentLevel)
        , onMouseDown (ResetPressed True)
        , onMouseUp (ResetPressed False)
        , style (blueButtonStyles model.resetPressed)
        ]
        [ div
            [ style
                [ ("background", "url('/assets/gameicons/PNG/White/1x/return.png')")
                , ("width", "40px")
                , ("height", "40px")
                , ("backgroundSize", "contain")
                , ("marginLeft", "5px")
                , ("marginTop", "1px")
                , ("opacity", "0.8")
                ]
            ] []
        ]
