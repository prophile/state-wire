{-# LANGUAGE GADTs #-}
{-# LANGUAGE Arrows #-}

module Control.FRP.Wire(Wire) where

import Prelude hiding (id, (.))

import Control.Category
import Control.Arrow

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

