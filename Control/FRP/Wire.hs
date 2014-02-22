{-# LANGUAGE GADTs #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.FRP.Wire(Wire) where

import Prelude hiding (id, (.))

import Control.Category
import Control.Arrow

import Control.Arrow.Transformer
import Control.Arrow.Operations(ArrowCircuit, delay)

data Wire a b c where
  WLift :: a b c -> Wire a b c
  WState :: a (b, s) (c, s) -> s -> Wire a b c

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

instance (ArrowLoop a) => ArrowCircuit (Wire a) where
  delay = WState (arr swp)
    where swp ~(x, y) = (y, x)

