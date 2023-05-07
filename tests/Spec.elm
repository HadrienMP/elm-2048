module Spec exposing (suite)

import Expect
import Grid exposing (Move(..))
import Test exposing (Test, describe, test)



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
                [ "0002 + L = 2000"
                , "0004 + L = 4000"
                , "2000 + L = 2000"
                , "0020 + L = 2000"
                , "0024 + L = 2400"
                ]
            , lineParamTest "les tuiles égales s'ajoutent"
                [ "0022 + L = 4000"
                , "4022 + L = 4400"
                ]
            , lineParamTest "Des tuiles déjà fusionnées ne peuvent pas fusioner sur le même tour"
                [ "2222 + L = 4400"
                , "4422 + L = 8400"
                , "2244 + L = 4800"
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
                                ( input, expected ) =
                                    case testName |> String.split " + L = " of
                                        [ a, b ] ->
                                            ( a, b )

                                        _ ->
                                            ( "oops", "oops" )
                            in
                            [ input ]
                                |> Grid.parseGrid
                                |> Grid.handle Left
                                |> Grid.printGrid
                                |> Expect.equal [ expected ]
                )
        )
