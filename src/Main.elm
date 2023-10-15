module Main exposing (Msg(..), main)

import Browser
import Browser.Events exposing (onResize)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Lazy
import Signal exposing (Signal)
import Time exposing (Posix)


type alias Model =
    { width : Float
    , height : Float
    , renderRange : ( Posix, Posix )
    , signal : Signal Float
    }


init : ( Float, Float ) -> ( Model, Cmd Msg )
init ( width, height ) =
    let
        renderRange =
            ( Time.millisToPosix -1000, Time.millisToPosix (ceiling width + 1000) )

        timeRange =
            ( Time.millisToPosix 0, Time.millisToPosix (ceiling width) )

        fakeTelemetry =
            Signal.noise timeRange
                |> Signal.map ((*) 800 >> (+) (height / 2))
    in
    ( Model width height renderRange fakeTelemetry, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div [ Attributes.class "relative" ]
        [ Html.div [ Attributes.class "absolute top-3 right-3 flex space-x-4" ]
            [ Html.button
                [ Attributes.class "rounded w-16 h-10 bg-gray-700 text-white text-xl border-gray-400 border hover:bg-gray-500 font-bold"
                , Events.onClick ZoomOut
                ]
                [ Html.text "-" ]
            , Html.button
                [ Attributes.class "rounded w-16 h-10 bg-gray-700 text-white text-xl border-gray-400 border hover:bg-gray-500 font-bold"
                , Events.onClick ZoomIn
                ]
                [ Html.text "+" ]
            ]
        , Html.Lazy.lazy viewCanvas model
        ]


viewCanvas : Model -> Html msg
viewCanvas model =
    Canvas.toHtml ( round model.width, round model.height )
        []
        [ blankCanvas model
        , Signal.render model.renderRange model.signal { width = model.width }
        ]


blankCanvas : Model -> Renderable
blankCanvas { width, height } =
    shapes [ fill Color.black ] [ rect ( 0, 0 ) width height ]


type Msg
    = BrowserResized Int Int
    | ZoomIn
    | ZoomOut


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

        ZoomIn ->
            let
                newRange =
                    model.renderRange
                        |> Tuple.mapBoth
                            (Time.posixToMillis >> (+) 250 >> Time.millisToPosix)
                            (Time.posixToMillis >> (+) -250 >> Time.millisToPosix)
            in
            ( { model | renderRange = newRange }, Cmd.none )

        ZoomOut ->
            let
                newRange =
                    model.renderRange
                        |> Tuple.mapBoth
                            (Time.posixToMillis >> (+) -250 >> Time.millisToPosix)
                            (Time.posixToMillis >> (+) 250 >> Time.millisToPosix)
            in
            ( { model | renderRange = newRange }, Cmd.none )


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
