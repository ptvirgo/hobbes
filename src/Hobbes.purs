module Hobbes where

import Prelude
import Data.Int
import Data.Maybe (Maybe(..))
import Data.NonEmpty as NE
import Data.Array ((:))

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Web.HTML.Window as Window
import Web.HTML (window)

import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event, EventType(..))
import Web.Internal.FFI (unsafeReadProtoTagged)
import Halogen.Query.Event (eventListener)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties (IProp)
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Attributes.Transform (Transform(..))

import Halogen.Svg.Elements as Svg
import Halogen.Svg.Attributes as SvgAttr
import Halogen.Svg.Attributes.Transform (Transform(..))

{- Size -}

data Size = Size { height :: Int , width :: Int }

instance Show Size where
  show (Size s) = "Height: " <> (show s.height) <> " Width: " <> (show s.width)

foreign import data ResizeEvent :: Type

resize :: EventType
resize = EventType "resize"

fromEvent :: Event -> Maybe ResizeEvent
fromEvent = unsafeReadProtoTagged "ResizeEvent"

toEvent :: ResizeEvent -> Event
toEvent = unsafeCoerce

windowSize :: Effect Size
windowSize = do
  w <- window
  height <- H.liftEffect $ Window.innerHeight w
  width <- H.liftEffect $ Window.innerWidth w
  pure $ Size { height : height, width : width }

whenWindowResizes :: forall state action slots output m. MonadEffect m => action -> H.HalogenM state action slots output m Unit
whenWindowResizes action = do
  target <- H.liftEffect $ Window.toEventTarget <$> window
  let listener = eventListener resize target (\_ -> Just action)
  _ <- H.subscribe listener
  pure unit

{- Scale -}

fitSizeToScale :: Size -> Size -> Transform
fitSizeToScale (Size target) (Size subject) =
  Scale n n where
    n = min (fitSizeToScale' target.width subject.width) (fitSizeToScale' target.height subject.height)

fitSizeToScale' :: Int -> Int -> Number
fitSizeToScale' target subject = (toNumber target) / (toNumber subject)

sizeToHxW :: forall r i. Size -> Array ( IProp ( height :: Number, width :: Number | r ) i )
sizeToHxW (Size s) = [ SvgAttr.height (toNumber s.height), SvgAttr.width (toNumber s.width) ]

sizeToTransform target subject = [ SvgAttr.transform [ (fitSizeToScale target subject) ]]

{- SVG Panels -}

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

data Bubble = Bubble
  { height :: Number
  , width :: Number
  , rootX :: Number
  , rootY :: Number
  , rootLength :: Number
  , facingRight :: Boolean
  }

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
