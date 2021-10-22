module Main where

import Prelude
import Data.Maybe (Maybe (..))

import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Console (log)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

import Web.HTML (window)
import Web.HTML.Window as Window

import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event, EventType(..))
import Web.Internal.FFI (unsafeReadProtoTagged)
import Halogen.Query.Event (eventListener)

{- Custom Resize Event -}

foreign import data ResizeEvent :: Type

resize :: EventType
resize = EventType "resize"

fromEvent :: Event -> Maybe ResizeEvent
fromEvent = unsafeReadProtoTagged "ResizeEvent"

toEvent :: ResizeEvent -> Event
toEvent = unsafeCoerce

whenWindowResizes :: forall state action slots output m. MonadEffect m => (Event -> Maybe action) -> H.HalogenM state action slots output m Unit
whenWindowResizes fn = do
  target <- H.liftEffect $ Window.toEventTarget <$> window
  let listener = eventListener resize target fn
  _ <- H.subscribe listener
  pure unit

{- Standard Halogen Stuff -}

data Size = Size { height :: Int , width :: Int }

instance Show Size where
  show (Size s) = "Height: " <> (show s.height) <> " Width: " <> (show s.width)

type State = Maybe Size

initialState :: forall input. input -> State
initialState _ = Nothing

data Action = Init | UpdateSize

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction action = case action of
  Init -> whenWindowResizes \_ -> Just UpdateSize
  UpdateSize -> H.liftEffect $ log "UpdateSize"

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.body [ HE.onResize (\_ -> UpdateSize) ]
  [ HH.p_ [ HH.text "Ytho?" ]
  , HH.p_
    [ HH.text $ case state of
                     Nothing -> "Size not loaded."
                     Just (Size s) -> show s
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
