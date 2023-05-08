module My2048_Test exposing (suite)

import Expect
import Grid
import Move
import Parser exposing ((|.), (|=))
import Test exposing (Test, describe, test)
import TestGrid



-- À chaque tour, une tuile apparait, soit un 2 soit un 4
-- une grille plus un mouvement = une nouvelle grille
-- nouvelle grille + une nouvelle tuile
-- une grille vide n'existe pas
-- une grille fait 4x4


suite : Test
suite =
    describe "2048"
        [ describe "Déplacement vers la gauche"
            [ lineParamTest "Aligne les tuiles sur le bord droit"
                [ """
                  0002 
                  + L
                  ----
                  2000
                  """
                , """
                  0004 
                  + L
                  ----
                  4000
                  """
                , """
                  2000 
                  + L
                  ----
                  2000
                  """
                , """
                  0020 
                  + L
                  ----
                  2000
                  """
                , """
                  0024 
                  + L
                  ----
                  2400
                  """
                ]
            , lineParamTest "les tuiles égales s'ajoutent"
                [ """
                  0022 
                  + L
                  ----
                  4000
                  """
                , """
                  4022 
                  + L
                  ----
                  4400
                  """
                ]
            , lineParamTest "Des tuiles déjà fusionnées ne peuvent pas fusioner sur le même tour"
                [ """
                  2222 
                  + L
                  ----
                  4400
                  """
                , """
                  4422 
                  + L
                  ----
                  8400
                  """
                , """
                  2244 
                  + L
                  ----
                  4800
                  """
                ]
            ]
        , describe "Déplacement vers la droite"
            [ lineParamTest "Aligne les tuiles sur le bord gauche"
                [ """
                  2000 
                  + R
                  ----
                  0002
                  """
                , """
                  4000 
                  + R
                  ----
                  0004
                  """
                , """
                  0002 
                  + R
                  ----
                  0002
                  """
                , """
                  0200 
                  + R
                  ----
                  0002
                  """
                , """
                  4200 
                  + R
                  ----
                  0042
                  """
                ]
            , lineParamTest "les tuiles égales s'ajoutent"
                [ """
                  2200 
                  + R
                  ----
                  0004
                  """
                , """
                  2204 
                  + R
                  ----
                  0044
                  """
                ]
            , lineParamTest "Des tuiles déjà fusionnées ne peuvent pas fusioner sur le même tour"
                [ """
                  2222 
                  + R
                  ----
                  0044
                  """
                , """
                  2244 
                  + R
                  ----
                  0048
                  """
                , """
                  4422 
                  + R
                  ----
                  0084
                  """
                ]
            ]
        , describe "Déplacement vers le bas"
            [ lineParamTest "Aligne les tuiles sur le bord bas"
                [ """
                2222
                0000
                0000
                0000
                + D
                ----
                0000
                0000
                0000
                2222
                """
                ]
            ]
        , describe "Déplacement vers le haut"
            [ lineParamTest "Aligne les tuiles sur le bord haut"
                [ """
                0000
                0000
                0000
                2222
                + U
                ----
                2222
                0000
                0000
                0000
                """
                ]
            ]
        ]


lineParamTest : String -> List String -> Test
lineParamTest name params =
    describe name
        (params
            |> List.map
                (\testName ->
                    test testName <|
                        \_ ->
                            let
                                result =
                                    parseTestName testName
                            in
                            case result of
                                Ok { input, move, expected } ->
                                    input
                                        |> Grid.handle move
                                        |> TestGrid.expectEqual expected

                                Err message ->
                                    Expect.fail message
                )
        )


parseTestName : String -> Result String { input : Grid.Grid, move : Move.Move, expected : Grid.Grid }
parseTestName raw =
    Parser.run
        (Parser.succeed
            (\input move expected ->
                { input = TestGrid.parse input
                , move = Move.parse move
                , expected = TestGrid.parse expected
                }
            )
            |= gridParser
            |. Parser.token "+"
            |= moveParser
            |. Parser.spaces
            |. Parser.token "----"
            |. Parser.spaces
            |= gridParser
        )
        raw
        |> Result.mapError Debug.toString


gridParser : Parser.Parser String
gridParser =
    Parser.succeed identity
        |. Parser.spaces
        |= (Parser.chompUntilEndOr "+" |> Parser.getChompedString)
        |. Parser.spaces


moveParser : Parser.Parser String
moveParser =
    Parser.succeed identity
        |. Parser.spaces
        |= (Parser.chompUntil "\n" |> Parser.getChompedString)
        |. Parser.spaces
