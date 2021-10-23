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

import Hobbes


type State = Maybe Size

initialState :: forall input. input -> State
initialState _ = Nothing

data Action = Init | UpdateSize

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction action = case action of
  Init -> do
          whenWindowResizes UpdateSize
          updateSize
  UpdateSize -> updateSize

updateSize :: forall output m. MonadEffect m => H.HalogenM State Action () output m Unit
updateSize = do
  size <- H.liftEffect windowSize
  H.modify_ $ \_ -> Just size

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
