module Container where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Svg.Hapykarva as HK

type State =
    { sparklinedata :: Array Number
    }

data Query a
    = UpdateData HK.Message a
    | RefreshData a

data Slot = HapykarvaSlot
derive instance eqHapykarvaSlot :: Eq Slot
derive instance ordHapykarvaSlot :: Ord Slot

component :: forall m. H.Component HH.HTML Query Unit Void m
component =
    H.parentComponent
        { initialState: const initialState
        , render
        , eval
        , receiver: const Nothing
        }
    where
    
    initialState :: State
    initialState =
        { sparklinedata: [1.0, 2.5, 3.0, -1.9, 0.0]
        }

    render :: State -> H.ParentHTML Query HK.Query Slot m
    render state =
        HH.div_
            [ HH.slot HapykarvaSlot HK.component state.sparklinedata (HE.input UpdateData)
            , HH.p_
                [ HH.text $ "Update data by clicking the button"
                , HH.button
                    [ HE.onClick (HE.input_ RefreshData) ]
                    [ HH.text "Update data" ]
                ]
            ]
    
    eval :: Query ~> H.ParentDSL State Query HK.Query Slot Void m
    eval = case _ of
        UpdateData (HK.UpdatedData _) next -> do
            pure next
        RefreshData next -> do
            pure next
