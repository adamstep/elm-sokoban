module LevelPicker exposing (Model, init, Msg(..), update, view)

import Html exposing (Html, Attribute, text, div, ul, li, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onMouseDown, onMouseUp)

import List

import Level.Levels as Levels exposing (AvailableLevels, allLevels)

-- Model

type alias Model =
    { levels: List Levels.AvailableLevels
    , cancelPressed: Bool
    }

init =
    Model Levels.allLevels False

-- Update

type Msg
    = LevelClicked Levels.AvailableLevels
    | Cancel
    | CancelPressed Bool

update: Msg -> Model -> Model
update msg model =
    case msg of
      CancelPressed pressed ->
        { model | cancelPressed = pressed }
      _ ->
        model


-- View

view : Model -> Html Msg
view model =
    div [ style
            [ ("position", "relative")
            , ("display", "flex")
            , ("flexDirection", "row")
            , ("flexWrap", "wrap")
            , ("justifyContent", "center")
            , ("alignItems", "center")
            , ("width", "100%")
            , ("height", "100%")
            ]
        ]
        [ controls model ]

controls model =
    div [ style
            [ ("position", "relative")
            , ("width", "500px")
            , ("font-family", "sans-serif")
            ]
        ]
        [ div [
           style
             [ ("position", "relative")
             , ("display", "flex")
             , ("flexDirection", "row")
             , ("flexWrap", "wrap")
             , ("justifyContent", "space-around")
             , ("width", "100%")
             ] 
          ]
          (List.indexedMap renderLevel model.levels)
        , div [
           style
             [ ("position", "relative")
             , ("display", "flex")
             , ("flexDirection", "row")
             , ("flexWrap", "wrap")
             , ("justifyContent", "space-around")
             , ("width", "100%")
             ] 
          ]
          [ cancelButton model.cancelPressed ]
        ]

cancelButton pressed =
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
         div [ onClick (Cancel)
             , onMouseDown (CancelPressed True)
             , onMouseUp (CancelPressed False)
             , style
                 (styles ++
                 [ ("textAlign", "center")
                 , ("color", "white")
                 , ("cursor", "pointer")
                 ])
             ] [ text "Back" ]   

renderLevel : Int -> Levels.AvailableLevels -> Html Msg
renderLevel index level =
    div []
        [ div
            [ onClick (LevelClicked level)
            , style
                [ ("width", "100px")
                , ("height", "100px")
                , ("background", "url('/assets/uipack_fixed/PNG/blue_panel.png')")
                , ("lineHeight", "100px")
                , ("textAlign", "center")
                , ("color", "white")
                , ("cursor", "pointer")
                , ("fontWeight", "bold")
                , ("fontSize", "32px")
                , ("opacity", "0.9")
                , ("margin", "10px")
                ]
            ]
            [ text (toString (index + 1)) ]
        ]
