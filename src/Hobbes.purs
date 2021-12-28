module Hobbes where

import Prelude
import Data.Int
import Data.Maybe (Maybe(..))
import Data.NonEmpty as NE

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
