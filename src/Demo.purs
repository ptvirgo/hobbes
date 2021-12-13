module Demo where

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
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

import Halogen.Svg.Elements as Svg
import Halogen.Svg.Attributes as SvgAttr
import Halogen.Svg.Attributes.Transform (Transform(..))

import Hobbes

{- For demonstration / experimentation -}

ratio :: Number
ratio = 0.9

demosize :: Size
demosize = Size { width : 500, height : 500 }
  
myPanel :: Panel
myPanel = NE.NonEmpty (Drawing { url : "/panel.svg#panel", translate : Nothing }) []

type State =
  { size :: Maybe Size
  , panel :: Panel
  , textOn :: Boolean
  }

initialState :: forall input. input -> State
initialState _ =
  { size : Nothing
  , panel : myPanel
  , textOn : false
  }

data Action = Init | UpdateSize | ToggleText

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction action = case action of
  Init -> do
          whenWindowResizes UpdateSize
          updateSize
  UpdateSize -> updateSize
  ToggleText -> toggleText

updateSize :: forall output m. MonadEffect m => H.HalogenM State Action () output m Unit
updateSize = do
  (Size ws) <- H.liftEffect windowSize
  let newHeight = floor $ (toNumber ws.height) * ratio
      newWidth = floor $ (toNumber ws.width) * ratio
      newSize = Size { height : newHeight, width : newWidth }
  H.modify_ $ \state -> state { size = Just newSize }

toggleText :: forall output m. MonadEffect m => H.HalogenM State Action () output m Unit
toggleText = do _ <- H.liftEffect $ log "toggleText"
                H.modify_ $ \state -> state { textOn = not state.textOn }

dummyBubble :: Bubble
dummyBubble = Bubble { height : 185.0, width : 200.0, rootX : 200.0, rootY : 250.0, rootLength : 25.0, facingRight : true }

dummyText :: forall w i. HH.HTML w i
dummyText = HH.p [ HP.style "margin: 1ex;" ]  [ HH.text "This is the song that does not end. It goes on and on my friend.  Some people started singing it not knowing what it was. Now they'll continue singing it forover just because..." ]


render :: forall m. State -> H.ComponentHTML Action () m
render state = case state.size of
  Nothing -> HH.p_ [ HH.text "Loading" ]
  Just s -> Svg.svg ( sizeToHxW s)
            [ Svg.g ( [ HE.onClick (\_ -> ToggleText ) ] <> ( sizeToTransform s demosize ) )
                    [ renderPanel state.panel
                    , renderText state.textOn
                    ]
            ]

renderText :: forall w i. Boolean -> HH.HTML w i
renderText b = Svg.g [ ] elems where
  elems = if b
             then [ renderBubble dummyBubble, textBubbleWindow dummyBubble [ dummyText ] ]
             else [ ]

component :: forall query input output m. MonadEffect m => H.Component query input output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction
                                 , initialize = Just Init
                                 }
  }

demoMain :: Effect Unit
demoMain = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
