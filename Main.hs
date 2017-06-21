{-# LANGUAGE RecordWildCards #-}
module Main where

import Helm
import           Helm
import qualified Helm.Cmd as Cmd
import           Helm.Color
import           Helm.Engine.SDL (SDLEngine)
import qualified Helm.Engine.SDL as SDL
import           Helm.Graphics2D
import qualified Helm.Graphics2D.Text as Text
import qualified Helm.Keyboard as Keyboard
import qualified Helm.Mouse as Mouse
import qualified Helm.Sub as Sub
import qualified Helm.Time as Time
import Helm.Time (Time)
import Linear.V2 (V2(V2))

windowDims :: V2 Int
windowDims = V2 800 600

data Model = Model

data Action = DoNothing

initial :: (Model, Cmd SDLEngine a)
initial = (Model, Cmd.none)

update :: t -> t1 -> (Model, Cmd SDLEngine a)
update model action = initial

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch []

view :: Model -> Graphics SDLEngine
view model = Graphics2D $  collage []

main :: IO ()
main = do
    engine <- SDL.startupWith $ SDL.defaultConfig {
        SDL.windowIsResizable = False
        , SDL.windowDimensions = windowDims
    }

    run engine GameConfig {
        initialFn       = initial
        , updateFn        = update
        , subscriptionsFn = subscriptions
        , viewFn = view
    }
