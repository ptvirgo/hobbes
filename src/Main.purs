module Main where

import Prelude
import Data.Int
import Data.Maybe (Maybe (..))
import Data.NonEmpty as NE

import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Console (log)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

import Halogen.Svg.Elements as Svg
import Halogen.Svg.Attributes as SvgAttr
import Halogen.Svg.Attributes.Transform (Transform(..))

import Hobbes

{- For demonstration / experimentation -}

ratio :: Number
ratio = 0.5

demosize :: Size
demosize = Size { width : 500, height : 500 }
  
myPanel :: Panel
myPanel = NE.NonEmpty (Drawing { url : "/panel.svg#panel", translate : Nothing }) []

type State =
  { size :: Maybe Size
  , panel :: Panel
  }

initialState :: forall input. input -> State
initialState _ =
  { size : Nothing
  , panel : myPanel
  }

data Action = Init | UpdateSize

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction action = case action of
  Init -> do
          whenWindowResizes UpdateSize
          updateSize
  UpdateSize -> updateSize

updateSize :: forall output m. MonadEffect m => H.HalogenM State Action () output m Unit
updateSize = do
  (Size ws) <- H.liftEffect windowSize
  let newHeight = floor $ (toNumber ws.height) * ratio
      newWidth = floor $ (toNumber ws.width) * ratio
      newSize = Size { height : newHeight, width : newWidth }
  H.modify_ $ \state -> state { size = Just newSize }

render :: forall m. State -> H.ComponentHTML Action () m
render state = case state.size of
  Nothing -> HH.p_ [ HH.text "Loading" ]
  Just s -> Svg.svg ( sizeToHxW s)
            [ Svg.g ( sizeToTransform s demosize )
                    [( renderPanel state.panel )
                    , renderBubble ( Bubble { height : 200.0, width : 100.0, rootX : 200.0, rootY : 250.0, rootLength : 25.0, facingRight : true } )
                    ]
            ]

component :: forall query input output m. MonadEffect m => H.Component query input output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction
                                 , initialize = Just Init
                                 }
  }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
