module Signal exposing (Signal, constant, map, noise, render)

import Canvas exposing (..)
import Canvas.Settings exposing (stroke)
import Canvas.Settings.Line exposing (lineWidth)
import Color
import Html.Attributes exposing (width)
import List.Extra
import List.Nonempty as NE exposing (ListNonempty)
import List.Nonempty.Extra as NE
import Simplex
import Time exposing (Posix)


type alias Range =
    ( Posix, Posix )


type Signal a
    = Points (ListNonempty ( Posix, a ))


epoch : Posix
epoch =
    Time.millisToPosix 0


constant : a -> Signal a
constant a =
    Points <| NE.fromPair ( epoch, a ) []


map : (a -> b) -> Signal a -> Signal b
map f (Points ps) =
    Points <| NE.map (Tuple.mapSecond f) ps


noise : Range -> Signal Float
noise ( min, max ) =
    let
        fractalNoise =
            Simplex.fractal2d { scale = 10.0, steps = 5, stepSize = 3.0, persistence = 4.0 } (Simplex.permutationTableFromInt 42) 0.0

        f =
            fractalNoise << toFloat
    in
    NE.range (Time.posixToMillis min) (Time.posixToMillis max)
        |> NE.map (\ms -> ( Time.millisToPosix ms, f ms ))
        |> Points


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first

            else
                find predicate rest


sample : Posix -> Signal a -> Maybe a
sample t (Points ps) =
    let
        timeInMillis =
            Time.posixToMillis t

        search pairOfPoints =
            let
                t1 =
                    (Tuple.first << NE.head) pairOfPoints

                t2 =
                    (Tuple.first << NE.last) pairOfPoints
            in
            timeInMillis >= t1 && timeInMillis < t2
    in
    ps
        |> NE.map (Tuple.mapFirst Time.posixToMillis)
        |> NE.groupsOfWithStep 2 1
        |> NE.toList
        |> find search
        |> Maybe.map (NE.head >> Tuple.second)


isJust : Maybe a -> Bool
isJust a =
    case a of
        Just _ ->
            True

        Nothing ->
            False


render : Range -> Signal Float -> { width : Float } -> Renderable
render range signal { width } =
    let
        ( min_, max_ ) =
            Tuple.mapBoth Time.posixToMillis Time.posixToMillis range

        deltaT =
            toFloat (max_ - min_) / width

        drawPoints =
            List.range 1 (floor width)
                |> List.map
                    (\i ->
                        let
                            msToSample =
                                round (toFloat min_ + (toFloat i * deltaT))
                        in
                        sample (Time.millisToPosix msToSample) signal
                            |> Maybe.map (\v -> ( toFloat i, v ))
                    )
                |> List.Extra.groupWhile (\a b -> a == b || (isJust a && isJust b))
                |> List.concatMap (NE.fromTuple >> NE.filterMap identity >> path_)

        path_ : List Canvas.Point -> List Shape
        path_ ps =
            Maybe.map2 Tuple.pair (List.head ps) (List.tail ps)
                |> Maybe.map (\( head, rest ) -> [ path head (List.map lineTo rest) ])
                |> Maybe.withDefault []
    in
    shapes [ stroke Color.white, lineWidth 2.0 ]
        drawPoints
