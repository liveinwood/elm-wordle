module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Board =
    Array (Array String)


type Status
    = Green
    | Yellow
    | Gray
    | N


type Key
    = Key String Status
    | Bs String
    | Ent String


type alias Keyboard =
    List Key


initialKeyboard : Keyboard
initialKeyboard =
    [ Key "A" N
    , Key "B" N
    , Key "C" N
    , Key "D" N
    , Key "E" N
    , Key "F" N
    , Key "G" N
    , Key "H" N
    , Key "I" N
    , Key "J" N
    , Key "K" N
    , Key "L" N
    , Key "M" N
    , Key "N" N
    , Key "O" N
    , Key "P" N
    , Key "Q" N
    , Key "R" N
    , Key "S" N
    , Key "T" N
    , Key "U" N
    , Key "V" N
    , Key "W" N
    , Key "X" N
    , Key "Y" N
    , Key "Z" N
    , Bs "BS"
    , Ent "ENTER"
    ]


type alias Model =
    String


init : Model
init =
    ""



-- UPDATE


type Msg
    = Click Key


update : Msg -> Model -> Model
update msg model =
    case msg of
        Click key ->
            case key of
                Key char _ ->
                    char

                Bs _ ->
                    ""

                Ent _ ->
                    "ENT"



-- VIEW


viewKeyboard : Keyboard -> Html Msg
viewKeyboard keyboard =
    div []
        [ viewKeyboardRow (List.take 10 keyboard)
        , viewKeyboardRow (List.take 9 (List.drop 10 keyboard))
        , viewKeyboardRow (List.take 9 (List.drop 19 keyboard))
        ]


viewKeyboardRow : Keyboard -> Html Msg
viewKeyboardRow keyboard =
    div []
        (List.map viewKey keyboard)


viewKey : Key -> Html Msg
viewKey key =
    case key of
        Key char _ ->
            button [ onClick (Click key) ] [ text char ]

        Bs char ->
            button [ onClick (Click key) ] [ text char ]

        Ent char ->
            button [ onClick (Click key) ] [ text char ]


view : Model -> Html Msg
view model =
    div []
        [ Html.p [] [ text model ]
        , viewKeyboard initialKeyboard
        ]
