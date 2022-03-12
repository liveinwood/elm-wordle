module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Set exposing (..)
import Test exposing (..)
import Wordle exposing (..)


suite : Test
suite =
    describe "The Wordle module"
        [ describe "initializeWordle"
            [ test "initalize row 0" <|
                \_ ->
                    let
                        wordle =
                            initializeWordle [] [ 'A', 'B', 'C', 'D', 'E' ]
                    in
                    Expect.equal 0 (getRow wordle)
            , test "initalize col -1" <|
                \_ ->
                    let
                        wordle =
                            initializeWordle [] [ 'A', 'B', 'C', 'D', 'E' ]
                    in
                    Expect.equal -1 (getCol wordle)
            , test "initalize answer" <|
                \_ ->
                    let
                        wordle =
                            initializeWordle [] [ 'A', 'B', 'C', 'D', 'E' ]
                    in
                    Expect.equal [ 'A', 'B', 'C', 'D', 'E' ] (getAnswer wordle)
            , test "initalize table" <|
                \_ ->
                    let
                        wordle =
                            initializeWordle [] [ 'A', 'B', 'C', 'D', 'E' ]
                    in
                    Expect.equal
                        [ [ ' ', ' ', ' ', ' ', ' ' ]
                        , [ ' ', ' ', ' ', ' ', ' ' ]
                        , [ ' ', ' ', ' ', ' ', ' ' ]
                        , [ ' ', ' ', ' ', ' ', ' ' ]
                        , [ ' ', ' ', ' ', ' ', ' ' ]
                        ]
                        (getTable wordle)
            ]
        , describe "pushChar"
            [ test "pushCharはrowを変更しない" <|
                \_ ->
                    let
                        wordle =
                            initializeWordle [] [ 'A', 'B', 'C', 'D', 'E' ]
                    in
                    Expect.equal 0 (getRow (pushChar 'A' wordle))
            , test "pushCharを連続して実行してもrowを変更しない" <|
                \_ ->
                    let
                        wordle =
                            initializeWordle [] [ 'A', 'B', 'C', 'D', 'E' ]

                        newWordle =
                            pushChar 'A' wordle |> pushChar 'B'
                    in
                    Expect.equal 0 (getRow newWordle)
            , test "pushCharはcolをインクリメントする" <|
                \_ ->
                    let
                        wordle =
                            initializeWordle [] [ 'A', 'B', 'C', 'D', 'E' ]
                    in
                    Expect.equal 0 (getCol (pushChar 'A' wordle))
            , test "pushCharを連続実行するとその分colがインクリメントされる" <|
                \_ ->
                    let
                        wordle =
                            initializeWordle [] [ 'A', 'B', 'C', 'D', 'E' ]

                        newWordle =
                            pushChar 'A' wordle |> pushChar 'B'
                    in
                    Expect.equal 1 (getCol newWordle)
            , test "pushCharを6回以上実行してもcolはインクリメントされない" <|
                \_ ->
                    let
                        wordle =
                            initializeWordle [] [ 'A', 'B', 'C', 'D', 'E' ]

                        newWordle =
                            pushChar 'A' wordle
                                |> pushChar 'B'
                                |> pushChar 'C'
                                |> pushChar 'D'
                                |> pushChar 'E'
                                |> pushChar 'F'
                                |> pushChar 'G'
                    in
                    Expect.equal 4 (getCol newWordle)
            , test "pushCharで追加した文字がtableに保存されている" <|
                \_ ->
                    let
                        wordle =
                            initializeWordle [] [ 'A', 'B', 'C', 'D', 'E' ]

                        newWordle =
                            pushChar 'V' wordle
                                |> pushChar 'W'
                                |> pushChar 'X'
                                |> pushChar 'Y'
                                |> pushChar 'Z'

                        expect =
                            [ [ 'V', 'W', 'X', 'Y', 'Z' ]
                            , [ ' ', ' ', ' ', ' ', ' ' ]
                            , [ ' ', ' ', ' ', ' ', ' ' ]
                            , [ ' ', ' ', ' ', ' ', ' ' ]
                            , [ ' ', ' ', ' ', ' ', ' ' ]
                            ]
                    in
                    Expect.equal expect (getTable newWordle)
            , test "pushCharを6回以上実行してもtableには5文字だけ保存されている" <|
                \_ ->
                    let
                        wordle =
                            initializeWordle [] [ 'A', 'B', 'C', 'D', 'E' ]

                        newWordle =
                            pushChar 'V' wordle
                                |> pushChar 'W'
                                |> pushChar 'X'
                                |> pushChar 'Y'
                                |> pushChar 'Z'
                                |> pushChar 'A'
                                |> pushChar 'B'

                        expect =
                            [ [ 'V', 'W', 'X', 'Y', 'Z' ]
                            , [ ' ', ' ', ' ', ' ', ' ' ]
                            , [ ' ', ' ', ' ', ' ', ' ' ]
                            , [ ' ', ' ', ' ', ' ', ' ' ]
                            , [ ' ', ' ', ' ', ' ', ' ' ]
                            ]
                    in
                    Expect.equal expect (getTable newWordle)
            , test "deleteCharを実行してもrowは変わらない" <|
                \_ ->
                    let
                        wordle =
                            initializeWordle [] [ 'A', 'B', 'C', 'D', 'E' ]

                        newWordle =
                            deleteChar wordle
                    in
                    Expect.equal 0 (getRow newWordle)
            , test "deleteCharを実行してもcolが-1より小さくなることはない" <|
                \_ ->
                    let
                        wordle =
                            initializeWordle [] [ 'A', 'B', 'C', 'D', 'E' ]

                        newWordle =
                            deleteChar wordle |> deleteChar
                    in
                    Expect.equal -1 (getCol newWordle)
            , test "pushCharを3回以上実行してdeleteCharを3回実行するとtableは空" <|
                \_ ->
                    let
                        wordle =
                            initializeWordle [] [ 'A', 'B', 'C', 'D', 'E' ]

                        newWordle =
                            pushChar 'V' wordle
                                |> pushChar 'W'
                                |> pushChar 'X'
                                |> deleteChar
                                |> deleteChar
                                |> deleteChar

                        expect =
                            [ [ ' ', ' ', ' ', ' ', ' ' ]
                            , [ ' ', ' ', ' ', ' ', ' ' ]
                            , [ ' ', ' ', ' ', ' ', ' ' ]
                            , [ ' ', ' ', ' ', ' ', ' ' ]
                            , [ ' ', ' ', ' ', ' ', ' ' ]
                            ]
                    in
                    Expect.equal expect (getTable newWordle)
            , test "pushCharとdeleteCharを交互に実行してみる" <|
                \_ ->
                    let
                        wordle =
                            initializeWordle [] [ 'A', 'B', 'C', 'D', 'E' ]

                        newWordle =
                            pushChar 'V' wordle
                                |> pushChar 'W'
                                |> deleteChar
                                |> pushChar 'X'
                                |> deleteChar
                                |> pushChar 'Y'
                                |> pushChar 'Z'
                                |> deleteChar

                        expect =
                            [ [ 'V', 'Y', ' ', ' ', ' ' ]
                            , [ ' ', ' ', ' ', ' ', ' ' ]
                            , [ ' ', ' ', ' ', ' ', ' ' ]
                            , [ ' ', ' ', ' ', ' ', ' ' ]
                            , [ ' ', ' ', ' ', ' ', ' ' ]
                            ]
                    in
                    Expect.equal expect (getTable newWordle)
            ]
        , describe "getGreenChar"
            [ test "getGreenChar test1" <|
                \_ ->
                    let
                        answer =
                            [ 'A', 'B', 'C', 'D', 'E' ]

                        guess =
                            [ 'A', 'B', 'C', 'D', 'E' ]

                        expect =
                            Set.fromList [ 'A', 'B', 'C', 'D', 'E' ]
                    in
                    Expect.equal expect (getGreenChar answer guess)
            , test "getGreenChar test2" <|
                \_ ->
                    let
                        answer =
                            [ 'A', 'B', 'C', 'D', 'E' ]

                        guess =
                            [ 'B', 'C', 'D', 'E', 'A' ]

                        expect =
                            Set.empty
                    in
                    Expect.equal expect (getGreenChar answer guess)
            , test "getGreenChar test3" <|
                \_ ->
                    let
                        answer =
                            [ 'A', 'B', 'C', 'D', 'E' ]

                        guess =
                            [ 'A', 'X', 'C', 'Y', 'E' ]

                        expect =
                            Set.fromList [ 'C', 'E', 'A' ]
                    in
                    Expect.equal expect (getGreenChar answer guess)
            ]
        , describe "getYellowChar"
            [ test "getYellowChar test1" <|
                \_ ->
                    let
                        answer =
                            [ 'A', 'B', 'C', 'D', 'E' ]

                        guess =
                            [ 'A', 'B', 'C', 'D', 'E' ]

                        expect =
                            Set.fromList [ 'A', 'B', 'C', 'D', 'E' ]
                    in
                    Expect.equal expect (getYellowChar answer guess)
            , test "getYellowChar test2" <|
                \_ ->
                    let
                        answer =
                            [ 'A', 'B', 'C', 'D', 'E' ]

                        guess =
                            [ 'B', 'X', 'D', 'Y', 'A' ]

                        expect =
                            Set.fromList [ 'A', 'B', 'D' ]
                    in
                    Expect.equal expect (getYellowChar answer guess)
            , test "getYellowChar test3" <|
                \_ ->
                    let
                        answer =
                            [ 'A', 'B', 'C', 'D', 'E' ]

                        guess =
                            [ 'V', 'W', 'X', 'Y', 'Z' ]

                        expect =
                            Set.empty
                    in
                    Expect.equal expect (getYellowChar answer guess)
            ]
        , describe "getGrayChar"
            [ test "getGrayChar test1" <|
                \_ ->
                    let
                        answer =
                            [ 'A', 'B', 'C', 'D', 'E' ]

                        guess =
                            [ 'A', 'B', 'C', 'D', 'E' ]

                        expect =
                            Set.empty
                    in
                    Expect.equal expect (getGrayChar answer guess)
            , test "getGrayChar test2" <|
                \_ ->
                    let
                        answer =
                            [ 'A', 'B', 'C', 'D', 'E' ]

                        guess =
                            [ 'B', 'X', 'D', 'Y', 'A' ]

                        expect =
                            Set.fromList [ 'X', 'Y' ]
                    in
                    Expect.equal expect (getGrayChar answer guess)
            , test "getGrayChar test3" <|
                \_ ->
                    let
                        answer =
                            [ 'A', 'B', 'C', 'D', 'E' ]

                        guess =
                            [ 'V', 'W', 'X', 'Y', 'Z' ]

                        expect =
                            Set.fromList [ 'V', 'W', 'X', 'Y', 'Z' ]
                    in
                    Expect.equal expect (getGrayChar answer guess)
            ]
        ]
