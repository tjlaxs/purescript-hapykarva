module Svg.Hapykarva where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Data.Maybe (Maybe(..))
import Data.Array (length, range, zip)
import Data.Int (toNumber)
import Svg.Path (path)

type State = 
    { data :: Array Number
    }

data Query a
    = UpdateData (Array Number) a

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
    initialState = { data: [13.0, 33.0, 1.0, -14.0] }

    render :: State -> H.ComponentHTML Query
    render state =
        let
            elem = HH.elementNS (H.Namespace "http://www.w3.org/2000/svg") <<< H.ElemName
            attr = HH.attr <<< H.AttrName

            width = 100
            height = 100
            xpad = toNumber width * 0.01
            ypad = toNumber height * 0.01
            vbx = negate xpad
            vby = negate ypad
            vbw = toNumber width + 2.0 * xpad
            vbh = toNumber height + 2.0 * ypad

            spc = width / (length state.data - 1)
            xRange = map (\x -> spc * x) $ range 0 (length state.data - 1)
            ps = zip xRange state.data
            svgPath = path width height ps
        in
            elem "svg"
                [ attr "viewBox" $ show vbx <> " " <> show vby <> " "
                                     <> show vbw <> " " <> show vbh
                , attr "class" "Icon Icon-foo"
                ]
                [ elem "path"
                    [ attr "d" $ svgPath
                    , attr "fill" "none"
                    , attr "stroke" "grey"
                    , attr "stroke-linecap" "round"
                    , attr "stroke-linejoin" "round"
                    ]
                    []
                ]
    
    eval :: Query ~> H.ComponentDSL State Query Message m
    eval (UpdateData nd next) = do
        H.modify (_ { data = nd })
        H.raise $ UpdatedData nd
        pure next
