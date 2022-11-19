module Types exposing (..)

import Random


type Vowel
    = A
    | O
    | E
    | I
    | U
    | Ue


allVowels : List Vowel
allVowels =
    [ A, O, E, I, U, Ue ]


type Tone
    = High
    | Rising
    | LowDipping
    | Falling


allTones : List Tone
allTones =
    [ High, Rising, LowDipping, Falling ]


combinations =
    List.concatMap (\v -> List.map (Tuple.pair v) allTones) allVowels


generator : Random.Generator ( Vowel, Tone )
generator =
    Random.pair (uniformWithDefault A allVowels) (uniformWithDefault High allTones)


uniformWithDefault : a -> List a -> Random.Generator a
uniformWithDefault x xs =
    case xs of
        head :: tail ->
            Random.uniform head tail

        [] ->
            Random.constant x


char ( v, t ) =
    case v of
        A ->
            case t of
                High ->
                    "ā"

                Rising ->
                    "á"

                LowDipping ->
                    "ǎ"

                Falling ->
                    "à"

        O ->
            case t of
                High ->
                    "ō"

                Rising ->
                    "ó"

                LowDipping ->
                    "ǒ"

                Falling ->
                    "ò"

        E ->
            case t of
                High ->
                    "ē"

                Rising ->
                    "é"

                LowDipping ->
                    "ě"

                Falling ->
                    "è"

        I ->
            case t of
                High ->
                    "ī"

                Rising ->
                    "í"

                LowDipping ->
                    "ǐ"

                Falling ->
                    "ì"

        U ->
            case t of
                High ->
                    "ū"

                Rising ->
                    "ú"

                LowDipping ->
                    "ǔ"

                Falling ->
                    "ù"

        Ue ->
            case t of
                High ->
                    "ǖ"

                Rising ->
                    "ǘ"

                LowDipping ->
                    "ǚ"

                Falling ->
                    "ǜ"
