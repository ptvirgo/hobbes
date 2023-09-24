module Hobbes where

import Prelude
import Data.Int
import Data.Maybe (Maybe (..))

import Effect.Class (class MonadEffect)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties (IProp)
import Halogen.Subscription (Emitter)
import Halogen.Query.Event (eventListener)
import Halogen.Svg.Elements as Svg
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Attributes.Transform (Transform(..))

import Web.HTML (window)
import Web.HTML.Window as Window

import Resize (resize)

type Size =
  { height :: Number
  , width :: Number
  }

whenWindowResizes :: forall action m. MonadEffect m => action -> m ( Emitter action )
whenWindowResizes action = do
  targetWindow <- H.liftEffect $ Window.toEventTarget <$> window
  pure $ eventListener resize targetWindow (\_ -> Just action)

getWindowSize :: forall m. MonadEffect m => m Size
getWindowSize = do
  w <- H.liftEffect window
  initHeight <- H.liftEffect $ Window.innerHeight w
  initWidth <- H.liftEffect $ Window.innerWidth w
  pure { height: toNumber initHeight, width: toNumber initWidth }


{- Scale -}

fitSizeScale :: Size -> Size -> Number
fitSizeScale target subject = min (fsc target.width subject.width) (fsc target.height subject.height) where
    fsc t s = t / s

sizeToHxW :: forall r i. Size -> Array ( IProp ( height :: Number, width :: Number | r ) i )
sizeToHxW s = [ SA.height s.height, SA.width s.width ]

rescale :: Number -> Size -> Size
rescale ratio size =
  { height : toNumber <<< floor $ size.height * ratio
  , width : toNumber <<< floor $ size.width * ratio
  }

fitTransform target subject = [ SA.transform [ Scale n n ]] where
    n = (fitSizeScale target subject)

fitSvgSize target subject = sizeToHxW <<< rescale ( fitSizeScale target subject ) $ subject

fittedSvg :: forall w i. Size -> Size -> HH.HTML w i -> HH.HTML w i
fittedSvg target subject image = 
    Svg.svg
        (fitSvgSize target subject)
        [ Svg.g (fitTransform target subject) [image]]
