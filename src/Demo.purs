module Demo where

import Prelude
import Data.Array ((:))
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

type Url = String
data Drawing = Drawing
  { url :: Url
  , translate :: Maybe { x :: Number, y :: Number }
  }

renderDrawing :: forall w i. Drawing -> HH.HTML w i
renderDrawing (Drawing d) = Svg.use ([ SvgAttr.href d.url ] <> atts) where
  atts = case d.translate of
    Nothing -> []
    Just tl -> [ SvgAttr.transform [ Translate tl.x tl.y ] ]

type Panel = NE.NonEmpty Array Drawing

renderPanel :: forall w i. Panel -> HH.HTML w i
renderPanel p = Svg.g [] $ NE.fromNonEmpty (\x xs -> (renderDrawing x) : (map renderDrawing xs)) p


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

data Bubble = Bubble
  { height :: Number
  , width :: Number
  , rootX :: Number
  , rootY :: Number
  , rootLength :: Number
  , facingRight :: Boolean
  }

textBubbleWindow :: forall w i. Bubble -> Array ( HH.HTML w i ) -> HH.HTML w i
textBubbleWindow (Bubble b) elements =
  Svg.foreignObject [ SvgAttr.height b.height, SvgAttr.width b.width, SvgAttr.x b.rootX, SvgAttr.y (b.rootY - b.rootLength - b.height ) ]
                    elements

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

renderBubbleBubble :: Bubble -> Array SvgAttr.PathCommand
renderBubbleBubble (Bubble b) =
  let backStep = if b.facingRight
                     then (\x -> - x)
                     else identity
      forwardStep = if b.facingRight
                        then identity
                        else (\x -> - x)
  in
  [ SvgAttr.l SvgAttr.Rel (forwardStep b.width * 0.3) (- b.rootLength)
  , SvgAttr.h SvgAttr.Rel (backStep b.width * 0.2)
  , SvgAttr.q SvgAttr.Rel (backStep b.width * 0.1) 0.0 (backStep b.width * 0.1) (- b.height * 0.1)
  , SvgAttr.v SvgAttr.Rel (- b.height * 0.8)
  , SvgAttr.q SvgAttr.Rel 0.0 (- b.height * 0.1) (forwardStep b.width * 0.1) (- b.height * 0.1)
  , SvgAttr.h SvgAttr.Rel (forwardStep b.width * 0.8)
  , SvgAttr.q SvgAttr.Rel (forwardStep b.width * 0.1) 0.0 (forwardStep b.width * 0.1) (b.height * 0.1)
  , SvgAttr.v SvgAttr.Rel (b.height * 0.8)
  , SvgAttr.q SvgAttr.Rel 0.0 (b.height * 0.1) (backStep b.width * 0.1) (b.height * 0.1)
  , SvgAttr.h SvgAttr.Rel (backStep b.width * 0.5)
  , SvgAttr.z
  ]

renderBubbleRoot :: Bubble -> Array SvgAttr.PathCommand
renderBubbleRoot (Bubble b) = [ SvgAttr.m SvgAttr.Abs b.rootX b.rootY ]

renderBubble :: forall w i. Bubble -> HH.HTML w i
renderBubble b =
  Svg.path
    [ SvgAttr.d ( renderBubbleRoot b <> renderBubbleBubble b )
    , SvgAttr.stroke $ SvgAttr.Named "black"
    , SvgAttr.strokeWidth 5.0
    , SvgAttr.fill $ SvgAttr.Named "white"
    ]

renderBubbleBox :: forall w i. Bubble -> HH.HTML w i
renderBubbleBox (Bubble b) = Svg.rect
  [ SvgAttr.height b.height
  , SvgAttr.width b.width
  , SvgAttr.x b.rootX
  , SvgAttr.y (b.rootY -  b.rootLength - b.height)
  , SvgAttr.fill $ SvgAttr.Named "purple"
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
