-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization.Backprop.ToVector
-- Copyright   :  (c) Masahiro Sakai 2023
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-- Conversion between a type and 'VS.Vector' 'Double'.
--
-----------------------------------------------------------------------------
module Numeric.Optimization.Backprop.ToVector {-# DEPRECATED "Use Numeric.Optimization.Utils.ToVector instead" #-}
  ( module Numeric.Optimization.Utils.ToVector
  ) where

import Numeric.Optimization.Utils.ToVector
