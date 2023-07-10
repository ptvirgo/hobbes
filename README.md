# Fit SVG to window in Purescript


## Usage excerpt

    handleAction :: Action -> H.HalogenM State Action Slot output m Unit
    handleAction Initialize = do
        listener <- whenWindowResizes NewSize
        _ <- H.subscribe listener
        updateSize

    handleAction NewSize = updateSize

    updateSize :: H.HalogenM State Action Slot output m Unit
    updateSize = do
       newSize <- rescale ratio <$> getWindowSize
       H.modify_ $ \state -> state { size = newSize }

    ...

    render :: State -> H.ComponentHTML Action Slot m
    render state = ...
        fittedSvg state.size (svg default size) (svg render)


## Import

packages.dhall


    let upstream = ...
    
    in  upstream
        with hobbes = 
            { dependencies =
               [ "console"
                , "effect"
                , "halogen"
                , "halogen-subscriptions"
                , "halogen-svg-elems"
                , "integers"
                , "maybe"
                , "prelude"
                , "test-unit"
                , "unsafe-coerce"
                , "web-events"
                , "web-html"
                , "web-uievents"
                ] 
            , repo = "https://github.com/ptvirgo/hobbes.git"
            , version = "master"
            }
