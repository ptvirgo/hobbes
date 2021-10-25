module Main where

import Prelude
import Data.Int
import Data.Maybe (Maybe (..))

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

import Hobbes

{- For demonstration / experimentation -}

newtype Icon = Icon String
derive instance eqIcon :: Eq Icon

instance Show Icon where
  show (Icon url) = url

type State =
  { icon :: Icon
  , size :: Maybe Size
  }

star :: Icon
star = Icon "/star.svg#star-icon"

spiral :: Icon
spiral = Icon "/spiral.svg#spiral-icon"

iconSize :: Size
iconSize = Size { height : 100, width : 100 }

ratio :: Number
ratio = 0.75

initialState :: forall input. input -> State
initialState _ =
  { icon : star
  , size : Nothing
  }

data Action = Init | UpdateSize | UseIcon Icon

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction action = case action of
  Init -> do
          whenWindowResizes UpdateSize
          updateSize
  UpdateSize -> updateSize
  UseIcon i -> H.modify_ $ \state -> state { icon = i }

updateSize :: forall output m. MonadEffect m => H.HalogenM State Action () output m Unit
updateSize = do
  (Size ws) <- H.liftEffect windowSize
  let newHeight = floor $ (toNumber ws.height) * ratio
      newWidth = floor $ (toNumber ws.width) * ratio
      newSize = Size { height : newHeight, width : newWidth }
  H.modify_ $ \state -> state { size = Just newSize }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.body [ HE.onResize (\_ -> UpdateSize) ]
  [ HH.button [ HE.onClick (\_ -> UseIcon star) ] [ HH.text "Star" ]
  , HH.button [ HE.onClick (\_ -> UseIcon spiral) ] [ HH.text "Spiral" ]
  , renderIcon state
  ]

renderIcon :: forall m. State -> H.ComponentHTML Action () m
renderIcon state = case state.size of
  Nothing -> HH.p_ [ HH.text "Size not loaded." ]
  Just (Size s) ->
    Svg.svg [ SvgAttr.height (toNumber s.height), SvgAttr.width (toNumber s.width) ]
            [ Svg.use [ SvgAttr.href (show state.icon), SvgAttr.transform [ scaleToFit (Size s) iconSize ]]]

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
