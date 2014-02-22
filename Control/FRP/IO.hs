{-# LANGUAGE Arrows #-}

module Control.FRP.IO(Output(Output), Input, getInput,
                      addInput, addOutput,
                      exec) where

import Control.Arrow

newtype Output a = Output a
newtype Input a  = Input { getInput :: a }

addInput  :: (Arrow a) => a (Input (i, b)) c -> a () i -> a (Input b) c
addInput b i = proc (Input x) -> do newInput <- i -< ()
                                    b -< Input (newInput, x)

addOutput :: (Arrow a) => a b (Output (o, c)) -> a o () -> a b (Output c)
addOutput b o = proc x -> do Output (newOutput, y) <- b -< x
                             () <- o -< newOutput
                             returnA -< Output y

exec :: (Arrow a) => a (Input ()) (Output ()) -> a () ()
exec a = const (Input ()) ^>> a >>^ const ()

