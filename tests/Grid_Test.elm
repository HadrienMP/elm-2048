module Grid_Test exposing (..)

import Expect
import Grid
import Test exposing (..)
import TestGrid


suite : Test
suite =
    describe "Grid"
        [ describe "Turn clockwise"
            [ TestGrid.test "single line"
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
            , TestGrid.test "small square"
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
            , TestGrid.test "acceptance"
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
            [ TestGrid.test "single line"
                { given = """1234"""
                , when = Grid.turnCounterClockwise
                , then_ = """
                    4
                    3
                    2
                    1
                    """
                }
            , TestGrid.test "small square"
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
        ]
