module Main exposing (Msg(..), main)

import Browser
import Browser.Events exposing (onResize)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Line exposing (lineWidth)
import Color
import Html exposing (Html)
import Simplex exposing (PermutationTable)



-- NOISE
--Create a permutation table, using 42 as the seed


permTable : PermutationTable
permTable =
    Simplex.permutationTableFromInt 42



-- Create a function for 2D fractal noise


noise : Float -> Float -> Float
noise =
    Simplex.fractal2d { scale = 10.0, steps = 5, stepSize = 3.0, persistence = 4.0 } permTable



-- Create a 100x100 matrix of fractal noise.


signal : Float -> List ( Float, Float )
signal range =
    List.range 0 (ceiling range)
        |> List.map (\x -> ( toFloat x, noise (toFloat x) (toFloat 0) ))


type alias Model =
    { width : Float
    , height : Float
    }


init : ( Float, Float ) -> ( Model, Cmd Msg )
init ( width, height ) =
    ( Model width height, Cmd.none )


view : Model -> Html msg
view { width, height } =
    let
        s =
            signal width

        head =
            List.head s |> Maybe.withDefault ( 0, height / 2 )

        rest =
            List.tail s |> Maybe.withDefault []

        adjusted =
            Tuple.mapSecond (\y -> (y * height / 2) + height / 2)
    in
    Canvas.toHtml ( round width, round height )
        []
        [ shapes [ fill Color.black ] [ rect ( 0, 0 ) width height ]
        , shapes [ stroke Color.white, lineWidth 2.0 ]
            [ path (adjusted head) (List.map (lineTo << adjusted) rest) ]
        ]


type Msg
    = BrowserResized Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BrowserResized w h ->
            let
                width =
                    toFloat w

                height =
                    toFloat h
            in
            ( { model
                | width = width
                , height = height
              }
            , Cmd.none
            )


main : Program ( Float, Float ) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions =
            \_ ->
                Sub.batch
                    [ onResize BrowserResized
                    ]
        }
