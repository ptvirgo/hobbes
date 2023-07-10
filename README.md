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

    ...

    render :: State -> H.ComponentHTML Action Slot m
    render state = ...
        fittedSvg targetSize defaultSize ...svg...


## Import

packages.dhall


let upstream = ...

in  upstream
    with hobbes = 
        { dependencies =
            [ "arrays"
            , "console"
            , "effect"
            , "halogen"
            , "halogen-svg-elems"
            , "integers"
            , "maybe"
            , "nonempty"
            , "prelude"
            , "psci-support"
            , "test-unit"
            , "unsafe-coerce"
            , "web-events"
            , "web-html"
            ]
        , repo = "https://github.com/ptvirgo/hobbes.git"
        , version = "master"
        }
