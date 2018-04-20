module Svg.Hapykarva where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Data.Maybe (Maybe(..))
import Data.Array (length, foldr, head)
import Data.Int (toNumber)

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
    initialState = { data: [10.0, 22.0, -11.0, 30.0] }

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
        in
            elem "svg"
                [ attr "viewBox" $ "0 0 " <> show width <> " " <> show height
                , attr "class" "Icon Icon-foo"
                ]
                [ elem "path"
                    [ attr "d" $
                        "M" <> show (0 * spc) <> "," <> show (interpolate 10.0) <>
                        "L" <> show (1 * spc) <> "," <> show (interpolate 22.0) <>
                        "L" <> show (2 * spc) <> "," <> show (interpolate (-11.0)) <>
                        "L" <> show (3 * spc) <> "," <> show (interpolate 30.0)
                    , attr "fill" "none"
                    , attr "stroke" "grey"
                    ]
                    []
                ]
    
    eval :: Query ~> H.ComponentDSL State Query Message m
    eval (Toggle next) = do
        state <- H.get
        H.put state
        H.raise $ UpdatedData state.data
        pure next
