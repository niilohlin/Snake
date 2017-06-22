{-# LANGUAGE RecordWildCards #-}
module Main where

import Helm
import           Helm
import qualified Helm.Cmd as Cmd
import           Helm.Color
import           Helm.Engine.SDL (SDLEngine)
import qualified Helm.Engine.SDL as SDL
import           Helm.Graphics2D as Graphics
import qualified Helm.Graphics2D.Text as Text
import qualified Helm.Keyboard as Keyboard
import qualified Helm.Mouse as Mouse
import qualified Helm.Sub as Sub
import qualified Helm.Time as Time
import Helm.Time (Time)
import Linear.V2 (V2(V2))
import Linear.V2 (_x)
import Data.List.NonEmpty as NonEmpty
import qualified Data.List.NonEmpty as NonEmpty
import qualified System.Random as Rand
import Debug.Trace
import Control.Lens ((+=), (&))

windowDims :: V2 Int
windowDims = V2 800 600

data Direction = North | South | West | East
type Snake = NonEmpty (V2 Int)

data Model = Model { snake :: Snake, direction :: Direction , apple :: V2 Int, speed :: Double }

data Action = Move Double | ChangeDirection Direction | Die | NewApple Rand.StdGen | Eat | Wait


initial :: (Model, Cmd SDLEngine Action)
initial = (Model { snake = V2 0 0 :| [], apple = V2 0 0, direction = East, speed = 0.5 }, Cmd.none)--Cmd.execute Rand.newStdGen NewApple)

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model (ChangeDirection e) = (changeDirection model e, Cmd.none)
    where
        changeDirection model@(Model{ direction = East }) West = model
        changeDirection model@(Model{ direction = West }) East = model
        changeDirection model@(Model{ direction = North }) South = model
        changeDirection model@(Model{ direction = South }) North = model
        changeDirection model direction = model { direction = direction }
update model Wait = (model, Cmd.none)
update model (Move _) = (move model, Cmd.none)
    where
        move m@Model{direction = East, snake = s} = m { snake = ((NonEmpty.head s) + V2 1 0) :| NonEmpty.init s}
        move m@Model{direction = West, snake = s} = m { snake = ((NonEmpty.head s) - V2 1 0) :| NonEmpty.init s}
        move m@Model{direction = South, snake = s} = m { snake = ((NonEmpty.head s) + V2 0 1) :| NonEmpty.init s}
        move m@Model{direction = North, snake = s} = m { snake = ((NonEmpty.head s) - V2 0 1) :| NonEmpty.init s}

update model (NewApple stdGen) = undefined
    --where
        --generateNewApple model stgGen =
        --randomPoint stgGen = let (x, newStdGen) = random stgGen in
                             --let (y, newStdGen') = random newStdGen in
                             --let vector = V2 (x `mod` 10) (y `mod` 10) in

update _ _ = undefined
    --where
        --move model =

--update model Die = (model { speed = 0.5, snake = }

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch [
    Keyboard.presses $ \key -> (case key of
        Keyboard.UpKey -> ChangeDirection North
        Keyboard.DownKey -> ChangeDirection South
        Keyboard.RightKey -> ChangeDirection East
        Keyboard.LeftKey -> ChangeDirection West
        _ -> Wait
    ),
    Time.fps 1 Move]

squareSize :: Double
squareSize = 30

drawSquare :: V2 Int -> Form e
drawSquare position = move (position' * squareSize' + windowDims' / pure 2.0) $ filled (rgb 0.5 0.5 0.5) $ square squareSize
    where
        windowDims' = fmap fromIntegral windowDims
        position' = fmap fromIntegral position
        squareSize' = pure squareSize

drawSnake :: Snake -> Form e
drawSnake snake = Graphics.group . toList $ fmap drawSquare snake

drawApple :: V2 Int -> Form e
drawApple = drawSquare

view :: Model -> Graphics SDLEngine
view model = Graphics2D $ collage [drawSnake (snake model), drawApple (apple model)]

main :: IO ()
main = do
    engine <- SDL.startupWith $ SDL.defaultConfig {
        SDL.windowIsResizable = False,
        SDL.windowDimensions = windowDims
    }

    run engine GameConfig {
        initialFn = initial,
        updateFn = update,
        subscriptionsFn = subscriptions,
        viewFn = view
    }
