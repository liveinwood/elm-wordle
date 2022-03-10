module Wordle exposing (Cp(..), Iw(..), Ni(..), check, deleteChar, getAnswer, getCol, getCp, getIw, getNi, getRow, getTable, initializeWordle, pushChar)

import Html.Attributes exposing (rows)
import List.Extra exposing (getAt, setAt)
import Maybe exposing (withDefault)
import Result exposing (andThen)
import Set as S exposing (Set)


type WordelError
    = NotEnoughLetter
    | NotInWordList


type alias Dictionary =
    Set (List Char)


initializeDictionary : List (List Char) -> Dictionary
initializeDictionary list =
    S.fromList list


type alias Table =
    List (List Char)


initialTable : Table
initialTable =
    [ [ ' ', ' ', ' ', ' ', ' ' ]
    , [ ' ', ' ', ' ', ' ', ' ' ]
    , [ ' ', ' ', ' ', ' ', ' ' ]
    , [ ' ', ' ', ' ', ' ', ' ' ]
    , [ ' ', ' ', ' ', ' ', ' ' ]
    ]


type Wordle
    = Wordle
        { row : Int
        , col : Int
        , answer : List Char
        , table : Table
        , dictionary : Dictionary
        }


initializeWordle : List (List Char) -> List Char -> Wordle
initializeWordle list answer =
    Wordle
        { row = 0
        , col = -1
        , answer = answer
        , table = initialTable
        , dictionary = S.fromList list
        }


getRow : Wordle -> Int
getRow (Wordle w) =
    w.row


getCol : Wordle -> Int
getCol (Wordle w) =
    w.col


getAnswer : Wordle -> List Char
getAnswer (Wordle w) =
    w.answer


getTable : Wordle -> Table
getTable (Wordle w) =
    w.table


getDictionary : Wordle -> Dictionary
getDictionary (Wordle w) =
    w.dictionary


pushChar : Char -> Wordle -> Wordle
pushChar char (Wordle wordle) =
    if wordle.col == 4 then
        Wordle wordle

    else
        let
            row =
                wordle.row

            col =
                wordle.col

            newCol =
                col + 1
        in
        Wordle
            { row = row
            , col = newCol
            , answer = wordle.answer
            , table = putCharAt row newCol char wordle.table
            , dictionary = wordle.dictionary
            }


putCharAt : Int -> Int -> Char -> Table -> Table
putCharAt row col char table =
    let
        row1 =
            withDefault [] (getAt row table)

        newRow =
            setAt col char row1
    in
    setAt row newRow table


deleteChar : Wordle -> Wordle
deleteChar (Wordle wordle) =
    if wordle.col == -1 then
        Wordle wordle

    else
        let
            currentRow =
                wordle.row

            currentCol =
                wordle.col

            newCol =
                wordle.col - 1
        in
        Wordle
            { row = wordle.row
            , col = newCol
            , answer = wordle.answer
            , table = putCharAt currentRow currentCol ' ' wordle.table
            , dictionary = wordle.dictionary
            }



-- 正しい位置にある文字の集合


type Cp
    = Cp (Set Char)



-- 回答文字列に含まれている文字の集合


type Iw
    = Iw (Set Char)



-- 回答文字列に含まれない文字の集合


type Ni
    = Ni (Set Char)



-- 正解に含まれていない文字集合を返す


getNi : List Char -> List Char -> Ni
getNi answer guess =
    Ni (S.diff (S.fromList guess) (S.fromList answer))



-- 正解に含まれている文字集合を返す


getIw : List Char -> List Char -> Iw
getIw answer guess =
    Iw (S.intersect (S.fromList answer) (S.fromList guess))



-- 正解と同じ位置にある文字集合を返す


getCp : List Char -> List Char -> Cp
getCp answer guess =
    Cp (S.fromList (getCpHelper answer guess))


getCpHelper : List Char -> List Char -> List Char
getCpHelper answer guess =
    case ( answer, guess ) of
        ( [], _ ) ->
            []

        ( _, [] ) ->
            []

        ( a :: aa, g :: gg ) ->
            if a == g then
                a :: getCpHelper aa gg

            else
                getCpHelper aa gg



-- 文字数をチェックする


checkWordLength : Wordle -> Result WordelError Wordle
checkWordLength wordle =
    if getCol wordle < 4 then
        Err NotEnoughLetter

    else
        Ok wordle



-- リストにある単語なのかチェックする


checkWordExist : Wordle -> Result WordelError Wordle
checkWordExist (Wordle wordle) =
    let
        dictionary =
            wordle.dictionary

        row =
            wordle.row

        word =
            withDefault [] (getAt row wordle.table)
    in
    if S.member word dictionary then
        Ok (Wordle wordle)

    else
        Err NotInWordList


checkWord : Wordle -> Result WordelError Wordle
checkWord wordle =
    checkWordLength wordle |> andThen checkWordExist


check : List Char -> List Char -> Result ( Cp, Iw, Ni ) Bool
check answer guess =
    if answer == guess then
        Ok True

    else
        Err ( getCp answer guess, getIw answer guess, getNi answer guess )
