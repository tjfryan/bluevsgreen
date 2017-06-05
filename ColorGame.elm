module Hello exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Color exposing (Color, rgb, toRgb)
import Element exposing (..)

-- MODEL

type alias Model =
    { upper : Color
    , lower : Color
    }

-- UPDATE

type Msg
    = Reset
    | BoundUpper Color
    | BoundLower Color

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (
    case msg of
        Reset -> initialState
        BoundUpper upper -> { model | upper = upper }
        BoundLower lower -> { model | lower = lower }
    ) ! []

-- VIEW

square : Color -> Html Msg
square color_ =
    toHtml (
        size 200 200 (color color_ empty)
    )

view : Model -> Html Msg
view model =
    case closeEnough model.upper model.lower of
    False ->
      let middle = average model.upper model.lower
      in
          div []
              [ square middle
              , div []
                  [ button [onClick (BoundUpper middle)] [ text "Green"]
                  , button [onClick (BoundLower middle)] [ text "Blue"]
                  ]
              ]
    True ->
        div []
            [ text "You found the difference between blue and green!"
            , div []
                [ text "Blue: "
                , square model.lower
                , text "Green: "
                , square model.upper
                ]
            ]

-- LOGIC

average : Color -> Color -> Color
average a b =
    let
        a_ = toRgb a
        b_ = toRgb b
    in
    rgb
        ((a_.red + b_.red) // 2)
        ((a_.green + b_.green) // 2)
        ((a_.blue + b_.blue) // 2)

closeEnough : Color -> Color -> Bool
closeEnough a b =
    let
        a_ = toRgb a
        b_ = toRgb b
    in
        (abs (a_.red - b_.red)) +
        (abs (a_.green - b_.green)) +
        (abs (a_.blue - b_.blue)) == 1

initialState: Model
initialState =
    { upper = rgb 0 255 0
    , lower = rgb 0 0 255
    }

init : (Model, Cmd Msg)
init = initialState ! []

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }
