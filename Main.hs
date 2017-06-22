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
import Data.List.NonEmpty
import qualified System.Random as Rand

windowDims :: V2 Int
windowDims = V2 800 600

data Direction = North | South | West | East
data Model = Model { snake :: NonEmpty (V2 Int), direction :: Direction , apple :: V2 Int, speed :: Double }

data Action = Move | ChangeDirection Direction | Die | NewApple Rand.StdGen | Eat | Wait

initial :: (Model, Cmd SDLEngine Action)
initial = (Model { snake = V2 0 0 :| [], apple = V2 0 0, direction = East, speed = 0.5 }, Cmd.execute Rand.newStdGen NewApple)

update :: t -> t1 -> (Model, Cmd SDLEngine Action)
update model action = initial

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch [
    Keyboard.presses $ \key -> ChangeDirection (case key of
        Keyboard.UpKey -> North
        Keyboard.DownKey -> South
        Keyboard.RightKey -> East
        Keyboard.LeftKey -> West
        _ -> West
    )]

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
