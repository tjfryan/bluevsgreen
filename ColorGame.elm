module Hello exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Color exposing (Color, rgb, toRgb)
import Element exposing (..)
import Random exposing (Generator, Seed, bool, step, initialSeed)
import Time exposing (now, millisecond)
import Task exposing (perform)


-- MODEL

type alias Model =
    { upper : Color
    , lower : Color
    , gameStarted : Bool
    , tries: Int
    , successes: Int
    , seed: Seed
    }

-- UPDATE

type Msg
    = Reset
    | BoundUpper Color
    | BoundLower Color
    | StartGame
    | Guess ColorOption ColorOption
    | SetSeed Seed

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (
    case msg of
        Reset -> initialState
        BoundUpper upper -> { model | upper = upper }
        BoundLower lower -> { model | lower = lower }
        StartGame -> { model | gameStarted = True }
        Guess real guess ->
            { model
            | tries = model.tries + 1
            , successes = model.successes + if real == guess then 1 else 0
            , seed = (\(_, x) -> x) (step colorChooser model.seed)
            }
        SetSeed seed -> { model | seed = seed }
    ) ! []

-- VIEW

square : Color -> Html Msg
square color_ =
    toHtml (size 200 200 (color color_ empty))

break : Html Msg
break = br [] []

phase1 : Model -> Html Msg
phase1 model =
    case closeEnough model.upper model.lower of
        False ->
            let middle = average model.upper model.lower
            in
                div []
                    [ text "What color is this?"
                    , square middle
                    , div []
                        [ button [ onClick (BoundUpper middle) ] [ text "Green"]
                        , button [ onClick (BoundLower middle) ] [ text "Blue"]
                        ]
                    ]
        True ->
            div []
                [ text "You found the difference between blue and green!"
                , div []
                    [ text "Blue: "
                    , break
                    , square model.lower
                    , break
                    , text "Green: "
                    , break
                    , square model.upper
                    ]
                , button [ onClick StartGame ] [ text "Start Game" ]
                ]

phase2 : Model -> Html Msg
phase2 model =
    let
        (colorOption, _) = step colorChooser model.seed
        color_ =
            case colorOption of
                Blue -> model.lower
                Green -> model.upper
    in
        div []
            [ text "What color is this?"
            , break
            , square color_
            , break
            , text ("Accuracy rate: " ++ (toString model.successes) ++ "/" ++ (toString model.tries))
            , break
            , button [ onClick (Guess colorOption Green) ] [ text "Green" ]
            , button [ onClick (Guess colorOption Blue) ] [ text "Blue" ]
            ]

view : Model -> Html Msg
view model =
    case model.gameStarted of
        False -> phase1 model
        True -> phase2 model

-- LOGIC

type ColorOption = Blue | Green

colorChooser : Generator ColorOption
colorChooser =
    Random.map (\b -> if b then Blue else Green) bool

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
    , gameStarted = False
    , tries = 0
    , successes = 0
    , seed = initialSeed 0
    }

init : (Model, Cmd Msg)
init = initialState !
       [ perform (\time -> SetSeed (initialSeed (round (time / millisecond)))) now
       ]

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }
