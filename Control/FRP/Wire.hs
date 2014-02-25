{-# LANGUAGE GADTs #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.FRP.Wire(Wire, ArrowWire, accumulate,
                        runWire, stepWire) where

import Prelude hiding (id, (.))

import Control.Category
import Control.Arrow

import Control.Arrow.Transformer
import Control.Arrow.Operations

import Data.Monoid
import Data.Monoid.Idempotent

data Wire a b c where
  WLift :: a b c -> Wire a b c
  WState :: a (b, s) (c, s) -> s -> Wire a b c

runWire :: (Arrow a) => Wire a () () -> a () ()
runWire (WLift a) = ioLoop
  where ioLoop = a >>> ioLoop
runWire (WState f s) = const ((), s) ^>> ioLoop >>^ const ()
  where ioLoop = f >>> ioLoop

stepWire :: (Arrow a) => Wire a b c -> a b (c, Wire a b c)
stepWire w@(WLift a) = a >>^ (\x -> (x, w))
stepWire (WState f s) = (\x -> (x, s)) ^>> f >>^ (\(x, s') -> (x, WState f s'))

instance (Arrow a) => Category (Wire a) where
  id = WLift id
  WLift f . WLift g = WLift (f . g)
  WLift f . WState g isg = WState (first f . g) isg
  WState f isf . WLift g = WState (f . first g) isf
  WState f isf . WState g isg = WState h (isf, isg)
    where h = proc ~(x, (sf, sg)) -> do ~(y, sg') <- g -< (x, sg)
                                        ~(z, sf') <- f -< (y, sf)
                                        id -< (z, (sf', sg'))

instance (Arrow a) => Arrow (Wire a) where
  arr = WLift . arr
  first (WLift f) = WLift (first f)
  first (WState f s) = WState (exchange ^>> first f >>^ exchange) s
    where exchange ~((x, y), z) = ((x, z), y)

instance (ArrowChoice a) => ArrowChoice (Wire a) where
  left (WLift f) = WLift (left f)
  left (WState f s) = WState (exchange ^>> left f >>^ unexchange) s
    where exchange (Left x, y) = Left (x, y)
          exchange (Right x, y) = Right (x, y)
          unexchange (Left (x, y)) = (Left x, y)
          unexchange (Right (x, y)) = (Right x, y)

instance (ArrowLoop a) => ArrowLoop (Wire a) where
  loop (WLift f) = WLift (loop f)
  loop (WState f s) = WState (loop $ exchange ^>> f >>^ exchange) s
    where exchange ~((a, b), c) = ((a, c), b)

instance (Arrow a) => ArrowTransformer Wire a where
  lift = WLift

class (Arrow a) => ArrowWire a where
  accumulate :: (Idempotent b) => a b b

instance (Arrow a) => ArrowWire (Wire a) where
  accumulate = WState (arr process) mempty
    where process ~(x, s) = let s' = s `mappend` x in (s', s')

instance (ArrowZero a) => ArrowZero (Wire a) where
  zeroArrow = WLift zeroArrow

instance (ArrowReader r a) => ArrowReader r (Wire a) where
  readState = WLift readState
  newReader (WLift x) = WLift (newReader x)
  newReader (WState f s) = WState f' s
    where f' = exchange ^>> newReader f
          exchange ~((x, y), z) = ((x, z), y)

instance (ArrowWriter w a) => ArrowWriter w (Wire a) where
  write = WLift write
  newWriter (WLift x) = WLift (newWriter x)
  newWriter (WState f s) = WState (newWriter f >>^ exchange) s
    where exchange ~((x, y), z) = ((x, z), y)

instance (ArrowState s a) => ArrowState s (Wire a) where
  fetch = WLift fetch
  store = WLift store

instance (ArrowChoice a, ArrowError ex a) => ArrowError ex (Wire a) where
  raise = WLift raise
  newError (WLift a) = WLift (newError a)
  newError (WState f a) = WState f' a
    where f' = proc ~(x, s) -> do y <- newError f -< (x, s)
                                  case y of
                                    Left ex  -> id -< (Left ex, s)
                                    Right (z, s') -> id -< (Right z, s')
  tryInUnless = tryInUnlessDefault

