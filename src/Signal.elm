module Signal exposing (Signal, constant, map, noise, render)

import Canvas exposing (..)
import Canvas.Settings exposing (stroke)
import Canvas.Settings.Line exposing (lineWidth)
import Color
import List.Nonempty as NE exposing (ListNonempty)
import List.Nonempty.Extra as NE
import Simplex
import Time exposing (Posix, posixToMillis)


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


render : Signal Float -> Renderable
render (Points ps) =
    let
        f =
            Tuple.mapFirst (toFloat << posixToMillis)
    in
    shapes [ stroke Color.white, lineWidth 2.0 ]
        [ path ((f << NE.head) ps) (List.map (lineTo << f) (NE.rest ps)) ]
