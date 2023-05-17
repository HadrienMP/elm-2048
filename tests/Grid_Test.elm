module Grid_Test exposing (..)

import Expect
import Game.Grid as Grid
import GridUtils
import Test exposing (..)


suite : Test
suite =
    describe "Grid"
        [ describe "Turn clockwise"
            [ GridUtils.test "single line"
                { given = "1234"
                , when = Grid.turnClockwise
                , then_ =
                    """
                    1
                    2
                    3
                    4
                    """
                }
            , GridUtils.test "small square"
                { given = """
                    12
                    34
                    """
                , when = Grid.turnClockwise
                , then_ = """
                    31
                    42
                    """
                }
            , GridUtils.test "acceptance"
                { given = """
                    1234
                    5678
                    9ABC
                    DEFG
                    """
                , when = Grid.turnClockwise
                , then_ =
                    """
                    D951
                    EA62
                    FB73
                    GC84
                    """
                }
            ]
        , describe "Turn counter clockwise"
            [ GridUtils.test "single line"
                { given = """1234"""
                , when = Grid.turnCounterClockwise
                , then_ = """
                    4
                    3
                    2
                    1
                    """
                }
            , GridUtils.test "small square"
                { given = """
                    12
                    34
                    """
                , when = Grid.turnCounterClockwise
                , then_ = """
                    24
                    13
                    """
                }
            ]
        , describe "available coordinates"
            [ test "single square" <|
                \_ ->
                    "0"
                        |> GridUtils.parse
                        |> Grid.listAvailableSquares
                        |> Expect.equal [ { x = 0, y = 0 } ]
            , test "one line" <|
                \_ ->
                    "00"
                        |> GridUtils.parse
                        |> Grid.listAvailableSquares
                        |> Expect.equal [ { x = 0, y = 0 }, { x = 1, y = 0 } ]
            , test "small square" <|
                \_ ->
                    """
                    00
                    00
                    """
                        |> GridUtils.parse
                        |> Grid.listAvailableSquares
                        |> Expect.equal
                            [ { x = 0, y = 0 }
                            , { x = 1, y = 0 }
                            , { x = 0, y = 1 }
                            , { x = 1, y = 1 }
                            ]
            , test "small square with unavailable square" <|
                \_ ->
                    """
                    01
                    00
                    """
                        |> GridUtils.parse
                        |> Grid.listAvailableSquares
                        |> Expect.equal
                            [ { x = 0, y = 0 }
                            , { x = 0, y = 1 }
                            , { x = 1, y = 1 }
                            ]
            ]
        ]
