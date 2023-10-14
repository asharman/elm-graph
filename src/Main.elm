module Main exposing (Msg(..), main)

import Browser
import Browser.Events exposing (onResize)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Html exposing (Html)
import Signal
import Time


type alias Model =
    { width : Float
    , height : Float
    }


init : ( Float, Float ) -> ( Model, Cmd Msg )
init ( width, height ) =
    ( Model width height, Cmd.none )


view : Model -> Html msg
view model =
    let
        fakeTelemetry =
            Signal.noise ( Time.millisToPosix 0, Time.millisToPosix (ceiling model.width) )
                |> Signal.map ((*) 1000 >> (+) (model.height / 2))
    in
    Canvas.toHtml ( round model.width, round model.height )
        []
        [ blankCanvas model
        , Signal.render fakeTelemetry
        ]


blankCanvas : Model -> Renderable
blankCanvas { width, height } =
    shapes [ fill Color.black ] [ rect ( 0, 0 ) width height ]


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
