module Svg.Hapykarva where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Data.Maybe (Maybe(..))
import Data.Array (head, tail, foldr, length, range, zip)
import Data.Int (toNumber)
import Data.Tuple (Tuple, fst, snd)
import Data.String (joinWith)

type State = 
    { data :: Array Number
    }

data Query a
    = Toggle a

data Message
    = UpdatedData (Array Number)

hapykarva :: forall m. H.Component HH.HTML Query Unit Message m
hapykarva =
    H.component
        { initialState: const initialState
        , render
        , eval
        , receiver: const Nothing
        }
    where
 
    initialState :: State
    initialState = { data: [10.0, 22.0, -11.0, 30.0, -33.0] }

    render :: State -> H.ComponentHTML Query
    render state =
        let
            width = 100
            height = 100

            elem = HH.elementNS (H.Namespace "http://www.w3.org/2000/svg") <<< H.ElemName
            attr = HH.attr <<< H.AttrName

            spc = width / (length state.data - 1)
            yMin = foldr min 0.0 state.data
            yMax = foldr max 0.0 state.data
            k = (negate <<< toNumber) height / (yMax - yMin)
            c = negate (k * yMax)
            interpolate x = k * x + c

            xRange = map (\x -> spc * x) $ range 0 (length state.data - 1)
            points = zip xRange state.data
            path = moveTo (head points) <> drawLines (tail points)
                where
                showPoint :: Tuple Int Number -> String
                showPoint p = show (fst p) <> "," <> show (interpolate (snd p))

                moveTo :: Maybe (Tuple Int Number) -> String
                moveTo (Nothing) = ""
                moveTo (Just p) = "M" <> showPoint p

                drawLines :: Maybe (Array (Tuple Int Number)) -> String
                drawLines Nothing = ""
                drawLines (Just ps) = joinWith "" $ map (\p -> "L" <> showPoint p) ps
        in
            elem "svg"
                [ attr "viewBox" $ "0 0 " <> show width <> " " <> show height
                , attr "class" "Icon Icon-foo"
                ]
                [ elem "path"
                    [ attr "d" $ path
                    , attr "fill" "none"
                    , attr "stroke" "grey"
                    , attr "stroke-linecap" "round"
                    , attr "stroke-linejoin" "round"
                    ]
                    []
                ]
    
    eval :: Query ~> H.ComponentDSL State Query Message m
    eval (Toggle next) = do
        state <- H.get
        H.put state
        H.raise $ UpdatedData state.data
        pure next
