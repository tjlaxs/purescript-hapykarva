module Svg.Path where

import Prelude
import Data.Array (head, tail, foldr)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Tuple (Tuple(..), fst, snd)

type Point = Tuple Int Number

interpolatePoints :: Int -> Int -> Array Point -> Array Point
interpolatePoints w h ps = map (\(Tuple x y) -> Tuple x $ interpolate h ymax ymin y) ps
    where
        ys = map snd ps
        ymin = foldr min 0.0 ys
        ymax = foldr max 0.0 ys
        interpolate height ymax ymin x = k * x + c
            where
                fk height ymax ymin = (negate <<< toNumber) height / (ymax - ymin)
                fc ymax k = negate (k * ymax)
                k = fk height ymax ymin
                c = fc ymax k

path :: Int -> Int -> Array Point -> String
path w h ps = createPath $ interpolatePoints w h ps

createPath :: Array Point -> String
createPath ps = moveTo (head ps) <> drawLines (tail ps)

showPoint :: Point -> String
showPoint p = show (fst p) <> "," <> show (snd p)

moveTo :: Maybe Point -> String
moveTo (Nothing) = ""
moveTo (Just p) = "M" <> showPoint p

drawLines :: Maybe (Array Point) -> String
drawLines Nothing = ""
drawLines (Just ps) = joinWith "" $ map (\p -> "L" <> showPoint p) ps
