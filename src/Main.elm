port module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Random
import Types exposing (..)


type alias Model =
    { seed : Random.Seed, state : State }


type State
    = InProgress Game
    | Menu
    | Results


type alias Game =
    { answer : ( Vowel, Tone )
    , guess : Maybe ( Vowel, Tone )
    }


type Msg
    = Guess ( Vowel, Tone )
    | PlaySound ( Vowel, Tone )
    | Next


port playSoundFromString : String -> Cmd msg


play : ( Vowel, Tone ) -> Cmd msg
play =
    char >> playSoundFromString


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


view : Model -> Html Msg
view superModel =
    case superModel.state of
        Menu ->
            viewMenu

        InProgress model ->
            viewGame model

        Results ->
            Html.text "TODO!"


viewMenu : Html Msg
viewMenu =
    Html.button
        [ Html.Events.onClick Next ]
        [ Html.text "start!" ]


init : Int -> ( Model, Cmd msg )
init seed =
    ( { seed = Random.initialSeed seed
      , state = Menu
      }
    , Cmd.none
    )


mapGame : (Game -> Game) -> State -> State
mapGame f state =
    case state of
        InProgress game ->
            InProgress (f game)

        _ ->
            state


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Guess guess ->
            ( { model
                | state = mapGame (\game -> { game | guess = Just guess }) model.state
              }
            , play guess
            )

        PlaySound sound ->
            ( model
            , play sound
            )

        Next ->
            let
                ( answer, seed ) =
                    Random.step generator model.seed
            in
            ( { model
                | state = InProgress { answer = answer, guess = Nothing }
                , seed = seed
              }
            , play answer
            )


viewGame : Game -> Html Msg
viewGame model =
    Html.div []
        [ viewSound model.answer
        , viewOptions model
        , viewNext (Just model.answer /= model.guess)
        ]


viewNext : Bool -> Html Msg
viewNext disabled =
    Html.button
        [ Html.Attributes.class "next"
        , Html.Attributes.disabled disabled
        , Html.Events.onClick Next
        ]
        [ Html.text "next ⮕" ]


viewSound : ( Vowel, Tone ) -> Html Msg
viewSound answer =
    Html.button
        [ Html.Events.onClick (PlaySound answer) ]
        [ Html.text "listen" ]


viewOptions : Game -> Html Msg
viewOptions model =
    Html.div
        [ Html.Attributes.class "options" ]
    <|
        List.map (viewOption model) combinations


viewOption : Game -> ( Vowel, Tone ) -> Html Msg
viewOption model option =
    Html.button
        [ Html.Events.onClick (Guess option)
        , optionData model.answer model.guess option
        ]
        [ Html.text (char option) ]


optionData answer guess option =
    Html.Attributes.attribute "data-state" <|
        case guess of
            Just g ->
                (if g == answer then
                    "correct"

                 else
                    "incorrect"
                )
                    ++ (if option == g then
                            "-this"

                        else
                            "-other"
                       )

            Nothing ->
                "unset"
