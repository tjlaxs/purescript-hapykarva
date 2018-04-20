module Svg.Hapykarva where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Data.Maybe (Maybe(..))
import Data.Array (length)

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
    initialState = { data: [0.0, 2.0, -1.0, 0.0] }

    render :: State -> H.ComponentHTML Query
    render state =
        let
            width = 100
            height = 100
            svgElem = HH.elementNS $ H.Namespace "http://www.w3.org/2000/svg"
            svag name = svgElem (H.ElemName name)
            xSpacing = width / (length state.data - 1)
        in
            svag "svg"
                [ HH.attr (H.AttrName "viewBox") ("0 0 " <> show width <> " " <> show height)
                , HH.attr (H.AttrName "class") "Icon Icon-foo"
                ]
                [ svag "path"
                    [HH.attr (H.AttrName "d") "M10 10 H 90 V 90 H 10 L 10 10" ]
                    []
                ]
    
    eval :: Query ~> H.ComponentDSL State Query Message m
    eval (Toggle next) = do
        state <- H.get
        H.put state
        H.raise $ UpdatedData state.data
        pure next
