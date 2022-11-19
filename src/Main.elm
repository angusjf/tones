port module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Random
import Types exposing (..)


type SuperModel
    = Game Model
    | Init Int
    | Results


type alias Model =
    { answer : ( Vowel, Tone )
    , guess : Maybe ( Vowel, Tone )
    , seed : Random.Seed
    }


type Msg
    = Guess ( Vowel, Tone )
    | PlaySound
    | Next
    | NewGame Int


port playSoundFromString : String -> Cmd msg


play =
    char >> playSoundFromString


main : Program Int SuperModel Msg
main =
    Browser.element
        { init = superInit
        , update = superUpdate
        , view = superView
        , subscriptions = \_ -> Sub.none
        }


superView : SuperModel -> Html Msg
superView superModel =
    case superModel of
        Init s ->
            viewInit s

        Game model ->
            view model

        Results ->
            Html.text "TODO!"


viewInit : Int -> Html Msg
viewInit seed =
    Html.button
        [ Html.Events.onClick (NewGame seed) ]
        [ Html.text "start!" ]


superInit : Int -> ( SuperModel, Cmd msg )
superInit seed =
    ( Init seed, Cmd.none )


init : Int -> ( Model, Cmd Msg )
init int =
    newQuestion <|
        { answer = ( A, High )
        , seed = Random.initialSeed int
        , guess = Nothing
        }


superUpdate : Msg -> SuperModel -> ( SuperModel, Cmd Msg )
superUpdate msg m =
    case msg of
        Guess guess ->
            case m of
                Game model ->
                    ( Game
                        { model
                            | guess = Just guess
                        }
                    , play guess
                    )

                _ ->
                    ( m, Cmd.none )

        PlaySound ->
            case m of
                Game model ->
                    ( Game model, play model.answer )

                _ ->
                    ( m, Cmd.none )

        Next ->
            case m of
                Game model ->
                    Tuple.mapFirst Game <| newQuestion model

                _ ->
                    ( m, Cmd.none )

        NewGame x ->
            Tuple.mapFirst Game <| init x


newQuestion model =
    let
        ( answer, seed ) =
            Random.step generator model.seed
    in
    ( { model
        | answer = answer
        , seed = seed
        , guess = Nothing
      }
    , play answer
    )


view : Model -> Html Msg
view model =
    Html.div []
        [ debug model
        , viewSound
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


debug model =
    Html.div []
        [ Html.text <| "answer: " ++ char model.answer
        , Html.text <| " / guess: " ++ (Maybe.withDefault "n/a" <| Maybe.map char model.guess)
        ]


viewSound =
    Html.button [ Html.Events.onClick PlaySound ] [ Html.text "listen" ]


viewOptions : Model -> Html Msg
viewOptions model =
    Html.div
        [ Html.Attributes.class "options" ]
    <|
        List.map (viewOption model) combinations


viewOption : Model -> ( Vowel, Tone ) -> Html Msg
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
