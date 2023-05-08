module GridUtils exposing (..)

import Expect
import Grid
import Row
import Test



-- Test


test : String -> { given : String, when : Grid.Grid -> Grid.Grid, then_ : String } -> Test.Test
test name { given, when, then_ } =
    Test.test name <|
        \_ ->
            given
                |> parse
                |> when
                |> expectEqualString then_


expectEqual : Grid.Grid -> Grid.Grid -> Expect.Expectation
expectEqual a b =
    Expect.equal (print a) (print b)


expectEqualString : String -> Grid.Grid -> Expect.Expectation
expectEqualString a b =
    Expect.equal (print <| parse a) (print b)



-- Parse


parse : String -> Grid.Grid
parse raw =
    raw
        |> String.split "\n"
        |> List.filter (String.trim >> String.isEmpty >> not)
        |> List.map parseRow


parseRow : String -> Row.Row
parseRow =
    String.trim >> String.split "" >> List.map (String.toInt >> Maybe.withDefault -1)



-- Print


print : Grid.Grid -> List String
print grid =
    grid |> List.map printRow


printRow : Row.Row -> String
printRow tiles =
    tiles
        |> List.map ((+) 48 >> Char.fromCode)
        |> List.foldr String.cons ""
