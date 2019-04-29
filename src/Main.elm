module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Grid exposing (Grid)
import Html exposing (Html)


type alias Flags =
    { seed : Maybe String
    }


type alias Model =
    { seed : String
    , squares : Grid Square
    }


type alias Square =
    { status : Status
    , label : String
    }


type Status
    = Marked
    | Unmarked


type Msg
    = Toggle ( Int, Int )


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
        (Grid.init ( size.width, size.height ) (Square Unmarked "Example"))
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle location ->
            let
                square =
                    Grid.get location model.squares
            in
            ( { model
                | squares =
                    Grid.update
                        location
                        { square
                            | status =
                                case square.status of
                                    Marked ->
                                        Unmarked

                                    Unmarked ->
                                        Marked
                        }
                        model.squares
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Element.layout [] (page model)


page : Model -> Element Msg
page model =
    column
        []
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
    in
    Input.button
        [ Font.color <|
            case square.status of
                Marked ->
                    rgb 1 0 0

                Unmarked ->
                    rgb 0 0 0
        ]
        { label = text square.label
        , onPress = Just (Toggle ( x, y ))
        }
