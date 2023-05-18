module Extra.Random exposing (chooseN, sequence, traverse)

import Random
import Random.List


traverse : (a -> Random.Generator b) -> Random.Generator (List a) -> Random.Generator (List b)
traverse f =
    Random.andThen (List.map f >> sequence)



------------------------------------------


sequence : List (Random.Generator a) -> Random.Generator (List a)
sequence =
    List.foldr
        (\el acc ->
            acc
                |> Random.andThen
                    (\randomTiles ->
                        el
                            |> Random.map (\randomTile -> randomTile :: randomTiles)
                    )
        )
        (Random.constant [])



------------------------------------------


chooseN : Int -> List a -> Random.Generator (List a)
chooseN toChoose list =
    recChooseN
        toChoose
        (Random.constant { chozen = [], choosable = list })
        |> Random.map .chozen


type alias RecursiveChoice a =
    { chozen : List a
    , choosable : List a
    }


recChooseN :
    Int
    -> Random.Generator (RecursiveChoice a)
    -> Random.Generator (RecursiveChoice a)
recChooseN leftToChoose generator =
    if leftToChoose == 0 then
        generator

    else
        recChooseN
            (leftToChoose - 1)
            (generator
                |> Random.andThen
                    (\previous ->
                        Random.List.choose previous.choosable
                            |> Random.map (addTo previous)
                    )
            )


addTo : RecursiveChoice a -> ( Maybe a, List a ) -> RecursiveChoice a
addTo previous ( pick, nextAvailable ) =
    { chozen =
        case pick of
            Just a ->
                a :: previous.chozen

            Nothing ->
                previous.chozen
    , choosable = nextAvailable
    }
