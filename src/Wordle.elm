module Wordle exposing (deleteChar, getAnswer, getCol, getGrayChar, getGreenChar, getRow, getTable, getYellowChar, initializeWordle, pushChar)

import List.Extra exposing (getAt, setAt, zip)
import Maybe exposing (withDefault)
import Result exposing (andThen)
import Set as S exposing (Set)


type GameState
    = Win
    | Lose
    | Continue


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
        , gameState : GameState
        , greenChars : Set Char
        , yellowChars : Set Char
        , grayChars : Set Char
        }


initializeWordle : List (List Char) -> List Char -> Wordle
initializeWordle list answer =
    Wordle
        { row = 0
        , col = -1
        , answer = answer
        , table = initialTable
        , dictionary = S.fromList list
        , gameState = Continue
        , greenChars = S.empty
        , yellowChars = S.empty
        , grayChars = S.empty
        }


pushChar : Char -> Wordle -> Wordle
pushChar char (Wordle wordle) =
    if wordle.col == 4 then
        Wordle wordle

    else
        let
            row =
                wordle.row

            newCol =
                wordle.col + 1
        in
        Wordle
            { wordle
                | col = newCol
                , table = putCharAt row newCol char wordle.table
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
            { wordle
                | col = newCol
                , table = putCharAt currentRow currentCol ' ' wordle.table
            }



-- 文字数をチェックする


checkWordLength : Wordle -> Result WordelError Wordle
checkWordLength (Wordle wordle) =
    if wordle.col < 4 then
        Err NotEnoughLetter

    else
        Ok (Wordle wordle)



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



-- 文字数が5文字で辞書にある単語であることをチェックする


checkWord : Wordle -> Result WordelError Wordle
checkWord wordle =
    checkWordLength wordle |> andThen checkWordExist


pushEnter : Wordle -> Wordle
pushEnter (Wordle w) =
    let
        guess =
            withDefault [] (getAt w.row w.table)

        answer =
            w.answer

        newGreenChars =
            S.union w.grayChars (getGreenChar answer guess)

        newYellowChars =
            S.union w.yellowChars (getYellowChar answer guess)

        newGrayChars =
            S.union w.grayChars (getGrayChar answer guess)
    in
    if guess == answer then
        Wordle
            { w
                | gameState = Win
                , greenChars = newGreenChars
                , yellowChars = newYellowChars
                , grayChars = newGrayChars
            }

    else
        Wordle
            { w
                | row = w.row + 1
                , col = -1
                , gameState =
                    if w.row + 1 == 4 then
                        Lose

                    else
                        Continue
                , greenChars = newGreenChars
                , yellowChars = newYellowChars
                , grayChars = newGrayChars
            }


getRow : Wordle -> Int
getRow (Wordle w) =
    w.row


getCol : Wordle -> Int
getCol (Wordle w) =
    w.col


getTable : Wordle -> Table
getTable (Wordle w) =
    w.table


getAnswer : Wordle -> List Char
getAnswer (Wordle w) =
    w.answer


getGreenChar : List Char -> List Char -> Set Char
getGreenChar answer guess =
    S.fromList
        (List.filterMap
            (\( a, g ) ->
                if a == g then
                    Just a

                else
                    Nothing
            )
            (zip answer guess)
        )


getYellowChar : List Char -> List Char -> Set Char
getYellowChar answer guess =
    S.intersect (S.fromList answer) (S.fromList guess)


getGrayChar : List Char -> List Char -> Set Char
getGrayChar answer guess =
    S.diff (S.fromList guess) (S.fromList answer)
