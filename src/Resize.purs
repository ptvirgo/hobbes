module Resize where

import Prelude
import Data.Maybe

import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event, EventType (..))
import Web.UIEvent.UIEvent (UIEvent)
import Web.Internal.FFI (unsafeReadProtoTagged)

resize :: EventType
resize = EventType "resize"

foreign import data ResizeEvent :: Type

fromUIEvent :: UIEvent -> Maybe ResizeEvent
fromUIEvent = unsafeReadProtoTagged "ResizeEvent"

fromEvent :: Event -> Maybe ResizeEvent
fromEvent = unsafeReadProtoTagged "ResizeEvent"

toUIEvent :: ResizeEvent -> UIEvent
toUIEvent = unsafeCoerce

toEvent :: ResizeEvent -> Event
toEvent = unsafeCoerce
