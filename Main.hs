{-# LANGUAGE RecordWildCards #-}
module Main where

import Helm
import qualified Helm.Cmd as Cmd
import           Helm.Color
import           Helm.Engine.SDL (SDLEngine)
import qualified Helm.Engine.SDL as SDL
import           Helm.Graphics2D as Graphics
import qualified Helm.Graphics2D.Text as Text
import qualified Helm.Keyboard as Keyboard
import qualified Helm.Sub as Sub
import qualified Helm.Time as Time
import Helm.Time (Time)
import Linear.V2 (V2(V2))
import Linear.V2 (_x)
import Data.List.NonEmpty as NonEmpty
import qualified Data.List.NonEmpty as NonEmpty
import qualified System.Random as Rand
import Debug.Trace
import qualified FRP.Elerea.Simple.Pure as Pure
import qualified Control.Monad as Monad

windowDims :: V2 Int
windowDims = V2 800 600

gameDims :: V2 Int
gameDims = fmap floor $ fmap fromIntegral windowDims / pure squareSize

modGameDim :: V2 Int -> V2 Int
modGameDim vector = fmap (flip mod) gameDims <*> vector


data Direction = North | South | West | East
type Snake = NonEmpty (V2 Int)

data Model = Model { snake :: Snake, direction :: Direction , apple :: V2 Int, speed :: Double }

data Action = Move Double | ChangeDirection Direction | NewApple Rand.StdGen | NoAction

initial :: (Model, Cmd SDLEngine Action)
initial = (Model { snake = V2 3 0 :| [V2 2 0, V2 1 0, V2 0 0], apple = V2 0 0, direction = East, speed = 0.5 }, Cmd.execute Rand.newStdGen NewApple)

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model (ChangeDirection newDirection) = changeDirection
    where
        changeDirection = case (direction model, newDirection) of
            (East,  West) -> (model, Cmd.none)
            (West,  East) -> (model, Cmd.none)
            (North, South) -> (model, Cmd.none)
            (South, North) -> (model, Cmd.none)
            (_, _) -> update (model { direction = newDirection }) (Move 0)

update model (Move _) = if eaten
                             then (newModel, (Cmd.execute Rand.newStdGen NewApple))
                             else (newModel, Cmd.none)
    where
        eaten = newHead == apple model
        dead = any (==snakeHead) snakeTail

        snakeTail = NonEmpty.tail $ snake model
        snakeHead = NonEmpty.head $ snake model

        newHead = modGameDim $ snakeHead + case direction model of
            East -> V2 1 0
            West -> -V2 1 0
            South -> V2 0 1
            North -> -V2 0 1

        newTail = case (eaten, dead) of
            (_, True) -> NonEmpty.take 3 $ snake model
            (True, _) -> NonEmpty.toList $ snake model
            (_, _) -> NonEmpty.init  $ snake model
        newModel = model { snake = newHead :| newTail }

update model (NewApple s) = (generateNewApple , Cmd.none)
    where
        generateNewApple = model {apple = randomPoint s}
        randomPoint stdGen = let (x, newStdGen) = Rand.random stdGen in
                      let (y, newStdGen') = Rand.random newStdGen in
                      let vector = V2 x y in
                      let newApplePos = modGameDim vector in
                      if any (==newApplePos) (snake model)
                          then randomPoint newStdGen'
                          else newApplePos

update model NoAction = (model, Cmd.none)

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch [
    Keyboard.presses $ \key -> (case key of
        Keyboard.UpKey -> ChangeDirection North
        Keyboard.DownKey -> ChangeDirection South
        Keyboard.RightKey -> ChangeDirection East
        Keyboard.LeftKey -> ChangeDirection West
        _ -> NoAction
    ),
    Time.fps 10 Move]

squareSize :: Double
squareSize = 30

drawSquare :: V2 Int -> Form e
drawSquare position = move (position' * squareSize') $ filled (rgb 0.5 0.5 0.5) $ square squareSize
    where
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
