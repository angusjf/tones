port module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Random
import Types exposing (Tone, Vowel, allVowels, char, generator, withTones)


type alias Model =
    { seed : Random.Seed, state : State }


type State
    = InProgress Game
    | Menu
    | Over Results


type alias Game =
    { answer : ( Vowel, Tone )
    , guess : Maybe ( Vowel, Tone )
    , options : List Vowel
    , results : Results
    }


type alias Results =
    { rounds : Int
    , correct : Int
    , incorrect : Int
    }


type Msg
    = Guess ( Vowel, Tone )
    | PlaySound ( Vowel, Tone )
    | Next
    | NewGame Int
    | Back


port playSoundFromString : String -> Cmd msg


play : ( Vowel, Tone ) -> Cmd msg
play =
    char >> playSoundFromString


main : Program Int Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


view : Model -> Browser.Document Msg
view superModel =
    { title = "mandarin tones quiz!"
    , body =
        case superModel.state of
            Menu ->
                viewMenu

            InProgress model ->
                viewGame model

            Over results ->
                viewResults results
    }


viewResults : Results -> List (Html Msg)
viewResults results =
    let
        score =
            String.fromInt results.correct ++ "/" ++ String.fromInt results.rounds

        quip =
            distribute "bad luck!"
                [ "nice try!", "well done!", "outstanding!" ]
                (toFloat results.correct / toFloat results.rounds)
    in
    [ Html.h1 [] [ Html.text "game over" ]
    , Html.h1 [] [ Html.text <| "score: " ++ score ]
    , Html.h1 [] [ Html.text <| quip ]
    , Html.button
        [ Html.Events.onClick Back ]
        [ Html.text "play again" ]
    ]


distribute : a -> List a -> Float -> a
distribute x xs n =
    let
        len =
            List.length xs

        get i l =
            if i == 0 then
                Maybe.withDefault x <| List.head l

            else if i < 0 then
                x

            else
                get (i - 1) (Maybe.withDefault [] <| List.tail l)
    in
    get (round (n * toFloat len)) (x :: xs)


viewMenu : List (Html Msg)
viewMenu =
    [ Html.h1 [] [ Html.text "tones quiz" ]
    , Html.button
        [ Html.Events.onClick (NewGame 1) ]
        [ Html.text "easy" ]
    , Html.button
        [ Html.Events.onClick (NewGame 3) ]
        [ Html.text "medium" ]
    , Html.button
        [ Html.Events.onClick (NewGame (List.length allVowels)) ]
        [ Html.text "hard" ]
    ]


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
                | state =
                    mapGame
                        (\game ->
                            { game
                                | guess = Just guess
                                , results =
                                    if guess == game.answer then
                                        case game.guess of
                                            Nothing ->
                                                rightAnswer game.results

                                            Just _ ->
                                                wrongAnswer game.results

                                    else
                                        game.results
                            }
                        )
                        model.state
              }
            , play guess
            )

        PlaySound sound ->
            ( model
            , play sound
            )

        Next ->
            let
                gameOver =
                    case model.state of
                        InProgress { results } ->
                            results.correct + results.incorrect >= results.rounds

                        _ ->
                            False
            in
            if gameOver then
                case model.state of
                    InProgress { results } ->
                        ( { model | state = Over results }, Cmd.none )

                    _ ->
                        ( { model | state = Menu }, Cmd.none )

            else
                case model.state of
                    InProgress game ->
                        let
                            ( vowels, answer, seed ) =
                                generator (List.length game.options) model.seed
                        in
                        ( { model
                            | state = InProgress { game | answer = answer, guess = Nothing, options = vowels }
                            , seed = seed
                          }
                        , play answer
                        )

                    _ ->
                        ( { model | state = Menu }, Cmd.none )

        NewGame nVowels ->
            let
                ( vowels, answer, seed ) =
                    generator nVowels model.seed
            in
            ( { model
                | state =
                    InProgress
                        { answer = answer
                        , guess = Nothing
                        , results = { correct = 0, incorrect = 0, rounds = 5 }
                        , options = vowels
                        }
                , seed = seed
              }
            , Cmd.none
            )

        Back ->
            ( { model | state = Menu }, Cmd.none )


rightAnswer : Results -> Results
rightAnswer results =
    { results | correct = results.correct + 1 }


wrongAnswer : Results -> Results
wrongAnswer results =
    { results | incorrect = results.incorrect + 1 }


viewGame : Game -> List (Html Msg)
viewGame model =
    [ viewQuestion model.results
    , viewSound model.answer
    , viewOptions model
    , viewNext (Just model.answer /= model.guess)
    ]


viewQuestion { correct, incorrect, rounds } =
    let
        t =
            String.fromInt (correct + incorrect) ++ "/" ++ String.fromInt rounds
    in
    Html.h1
        []
        [ Html.text t ]


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
        List.map (viewOption model) (withTones model.options)


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
