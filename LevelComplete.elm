module LevelComplete exposing (Model, init, Msg(..), update, view)

import Html exposing (Html, Attribute, text, div, ul, li, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onMouseDown, onMouseUp)

import List

import Level.Levels as Levels exposing (AvailableLevels, allLevels)

-- Model

type alias Model =
    { nextPressed: Bool
    , replayPressed: Bool
    }

init =
    Model False False

type Msg
    = NextPressed Bool
    | ReplayPressed Bool
    | Next
    | Replay

update: Msg -> Model -> Model
update msg model =
    case msg of
      NextPressed pressed ->
        { model | nextPressed = pressed }
      ReplayPressed pressed ->
        { model | replayPressed = pressed }
      _ ->
        model


view : Model -> Html Msg
view model =
    div [ style
            [ ("position", "relative")
            , ("display", "flex")
            , ("flexDirection", "column")
            , ("flexWrap", "wrap")
            , ("justifyContent", "center")
            , ("alignItems", "center")
            , ("width", "100%")
            , ("height", "100%")
            ]
        ]
        [ replayButton model.replayPressed
        , nextButton model.nextPressed
        ]

replayButton pressed =
    let
        styles = if pressed then
            [ ("background", "url('/assets/uipack_fixed/PNG/red_button00.png')")
            , ("width", "190px")
            , ("marginTop", "24px")
            , ("height", "45px")
            , ("lineHeight", "45px")
            ]
        else
            [ ("background", "url('/assets/uipack_fixed/PNG/red_button13.png')")
            , ("width", "190px")
            , ("marginTop", "20px")
            , ("height", "49px")
            , ("lineHeight", "49px")
            ]
    in
         div [ onClick Replay
             , onMouseDown (ReplayPressed True)
             , onMouseUp (ReplayPressed False)
             , style
                 (styles ++
                 [ ("textAlign", "center")
                 , ("color", "white")
                 , ("cursor", "pointer")
                 ])
             ] [ text "Replay" ]


nextButton pressed =
    let
        styles = if pressed then
            [ ("background", "url('/assets/uipack_fixed/PNG/red_button00.png')")
            , ("width", "190px")
            , ("marginTop", "24px")
            , ("height", "45px")
            , ("lineHeight", "45px")
            ]
        else
            [ ("background", "url('/assets/uipack_fixed/PNG/red_button13.png')")
            , ("width", "190px")
            , ("marginTop", "20px")
            , ("height", "49px")
            , ("lineHeight", "49px")
            ]
    in
         div [ onClick Next
             , onMouseDown (NextPressed True)
             , onMouseUp (NextPressed False)
             , style
                 (styles ++
                 [ ("textAlign", "center")
                 , ("color", "white")
                 , ("cursor", "pointer")
                 ])
             ] [ text "Next" ] 
