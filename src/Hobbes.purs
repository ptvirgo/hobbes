module Hobbes where

import Prelude
import Data.Int
import Data.Maybe (Maybe(..))

import Effect (Effect)
import Effect.Class (class MonadEffect)

import Web.HTML.Window as Window
import Web.HTML (window)

import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event, EventType(..))
import Web.Internal.FFI (unsafeReadProtoTagged)
import Halogen.Query.Event (eventListener)

import Halogen as H
import Halogen.Svg.Attributes as SA
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

scaleToFit :: Size -> Size -> Transform
scaleToFit (Size target) (Size subject) =
  Scale n n where
    n = min (scaleToFit' target.width subject.width) (scaleToFit' target.height subject.height)

scaleToFit' :: Int -> Int -> Number
scaleToFit' target subject = (toNumber target) / (toNumber subject)
