module Main exposing (main)

import Array exposing (Array)
import Browser
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Grid exposing (Grid)
import Html exposing (Html)
import Random
import Random.List


characters : List String
characters =
    [ "Tyrion Lannister"
    , "Jaime Lannister"
    , "Cersei Lannister"
    , "Daenerys"
    , "Jon Snow"
    , "Sansa Stark"
    , "Arya Stark"
    , "Davos Seaworth"
    , "Missandei"
    , "Theon Greyjoy"
    , "Samwell Tarly"
    , "Bran Stark"
    , "Brienne of Tarth"
    , "Varys"
    , "The Hound"
    , "Bronn"
    , "Tormund"
    , "Gendry"
    , "Grey Worm"
    , "Jorah Mormont"
    , "Gilly"
    , "Melisandre"
    , "Beric Dondarrion"
    , "Eddison Tollett"
    , "Podrick Payne"
    , "Yohn Royce"
    , "Lyanna Mormont"
    , "The Night King"
    ]


type alias Flags =
    { seed : Maybe String
    }


type alias Model =
    { seed : String
    , squares : Grid Status
    }


type Status
    = Marked
    | Unmarked


type Msg
    = Toggle ( Int, Int )
    | UpdateSeed String


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


size =
    { width = 5
    , height = 5
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model
        (flags.seed |> Maybe.withDefault "elm")
        (Grid.init ( size.width, size.height ) Unmarked)
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSeed seed ->
            ( { model | seed = seed }
            , Cmd.none
            )

        Toggle location ->
            let
                square =
                    Grid.get location model.squares
            in
            ( { model
                | squares =
                    Grid.update
                        location
                        (case square of
                            Marked ->
                                Unmarked

                            Unmarked ->
                                Marked
                        )
                        model.squares
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Element.layout [] (page model)


page : Model -> Element Msg
page model =
    column [ padding 16 ]
        [ Input.text []
            { label = Input.labelAbove [] (text "Seed")
            , onChange = UpdateSeed
            , placeholder = Nothing
            , text = model.seed
            }
        , grid model
        ]


grid : Model -> Element Msg
grid model =
    column
        [ Font.center ]
        (List.map (viewSquareRow model) (List.range 0 (size.height - 1)))


viewSquareRow : Model -> Int -> Element Msg
viewSquareRow model y =
    row []
        (List.map (viewSquare model y) (List.range 0 (size.width - 1)))


viewSquare : Model -> Int -> Int -> Element Msg
viewSquare model y x =
    let
        square =
            Grid.get ( x, y ) model.squares

        index =
            y * size.width + x

        ( shuffledList, _ ) =
            Random.step (Random.List.shuffle characters) (stringToSeed model.seed)
                |> Tuple.mapFirst Array.fromList
    in
    Input.button
        [ Font.color <|
            case square of
                Marked ->
                    rgb 1 0 0

                Unmarked ->
                    rgb 0 0 0
        , Border.solid
        , Border.width 1
        , Font.size 16
        ]
        { label =
            el
                [ paddingXY 8 32
                , width (px 150)
                , clipX
                ]
                (text <|
                    case ( x, y ) of
                        ( 2, 2 ) ->
                            "Wight"

                        _ ->
                            Array.get index shuffledList |> Maybe.withDefault "oops"
                )
        , onPress = Just (Toggle ( x, y ))
        }


indexGenerator : Random.Generator Int
indexGenerator =
    Random.int 0 (size.width * size.height - 1)


stringToSeed : String -> Random.Seed
stringToSeed =
    String.toList
        >> List.map Char.toCode
        >> List.map String.fromInt
        >> String.join ""
        >> String.toInt
        >> Maybe.withDefault 0
        >> Random.initialSeed
