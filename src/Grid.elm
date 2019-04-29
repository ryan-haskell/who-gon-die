module Grid exposing
    ( Grid
    , get
    , init
    , update
    )

import Array exposing (Array)


type Grid a
    = Grid a Size (Array (Array a))


type alias Size =
    ( Int, Int )


type alias Position =
    ( Int, Int )


init : Size -> a -> Grid a
init ( width, height ) value =
    Grid
        value
        ( width, height )
        (Array.fromList <|
            List.map
                (\_ ->
                    Array.fromList <|
                        List.map (always value) (List.range 1 width)
                )
                (List.range 1 height)
        )


get : Position -> Grid a -> a
get ( x, y ) (Grid default _ rows) =
    Array.get y rows
        |> Maybe.withDefault Array.empty
        |> Array.get x
        |> Maybe.withDefault default


update : Position -> a -> Grid a -> Grid a
update ( x, y ) value (Grid default size rows) =
    Grid
        default
        size
        (Array.set y
            (Array.get y rows
                |> Maybe.withDefault Array.empty
                |> Array.set x value
            )
            rows
        )
