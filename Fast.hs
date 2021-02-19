{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Fast where

import Control.Applicative
import Control.Monad
-- import CirParser

import Control.Monad.State
import Control.Monad.Trans.Writer.Strict
import Data.Char
import Data.Function
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import GateStruct
import QCParser
import Quipper
import Quipper.Libraries.QuipperASCIIParser
import QuipperParser
import System.Environment
import TfcParser

--import qc2_inHSformat
i45eff_inc = sortBy (compare `on` t_count) i45eff

i45eff_dec = reverse i45eff_inc

i45eff = [[G 1 [0], G 1 [1], G 1 [2], G 1 [3], G 7 [0, 1], G 7 [0, 2], G 7 [0, 3], G 7 [1, 2], G 7 [1, 3], G 7 [2, 3], G 1 [0, 1, 2], G 1 [0, 1, 3], G 1 [0, 2, 3], G 1 [1, 2, 3], G 7 [0, 1, 2, 3]], [G 1 [0], G 1 [1], G 1 [2], G 1 [4], G 7 [0, 1], G 7 [0, 2], G 7 [0, 4], G 7 [1, 2], G 7 [1, 4], G 7 [2, 4], G 1 [0, 1, 2], G 1 [0, 1, 4], G 1 [0, 2, 4], G 1 [1, 2, 4], G 7 [0, 1, 2, 4]], [G 1 [0], G 1 [1], G 1 [3], G 1 [4], G 7 [0, 1], G 7 [0, 3], G 7 [0, 4], G 7 [1, 3], G 7 [1, 4], G 7 [3, 4], G 1 [0, 1, 3], G 1 [0, 1, 4], G 1 [0, 3, 4], G 1 [1, 3, 4], G 7 [0, 1, 3, 4]], [G 1 [0], G 1 [2], G 1 [3], G 1 [4], G 7 [0, 2], G 7 [0, 3], G 7 [0, 4], G 7 [2, 3], G 7 [2, 4], G 7 [3, 4], G 1 [0, 2, 3], G 1 [0, 2, 4], G 1 [0, 3, 4], G 1 [2, 3, 4], G 7 [0, 2, 3, 4]], [G 1 [1], G 1 [2], G 1 [3], G 1 [4], G 7 [1, 2], G 7 [1, 3], G 7 [1, 4], G 7 [2, 3], G 7 [2, 4], G 7 [3, 4], G 1 [1, 2, 3], G 1 [1, 2, 4], G 1 [1, 3, 4], G 1 [2, 3, 4], G 7 [1, 2, 3, 4]], [G 2 [0], G 2 [1], G 2 [2], G 1 [3], G 1 [4], G 6 [01], G 6 [0, 2], G 7 [0, 3], G 7 [0, 4], G 6 [1, 2], G 7 [1, 3], G 7 [1, 4], G 7 [2, 3], G 7 [2, 4], G 2 [0, 1, 2], G 1 [0, 1, 3], G 1 [0, 1, 4], G 1 [0, 2, 3], G 1 [0, 2, 4], G 1 [1, 2, 3], G 1 [1, 2, 4], G 7 [0, 1, 2, 3], G 7 [0, 1, 2, 4]], [G 2 [0], G 2 [1], G 1 [2], G 2 [3], G 1 [4], G 6 [0, 1], G 7 [0, 2], G 6 [0, 3], G 7 [0, 4], G 7 [1, 2], G 6 [1, 3], G 7 [1, 4], G 7 [2, 3], G 7 [3, 4], G 1 [0, 1, 2], G 2 [0, 1, 3], G 1 [0, 1, 4], G 1 [0, 2, 3], G 1 [0, 3, 4], G 1 [1, 2, 3], G 1 [1, 3, 4], G 7 [0, 1, 2, 3], G 7 [0, 1, 3, 4]], [G 2 [0], G 1 [1], G 2 [2], G 2 [3], G 1 [4], G 7 [0, 1], G 6 [0, 2], G 6 [0, 3], G 7 [0, 4], G 7 [1, 2], G 7 [1, 3], G 6 [2, 3], G 7 [2, 4], G 7 [3, 4], G 1 [0, 1, 2], G 1 [0, 1, 3], G 2 [0, 2, 3], G 1 [0, 2, 4], G 1 [0, 3, 4], G 1 [1, 2, 3], G 1 [2, 3, 4], G 7 [0, 1, 2, 3], G 7 [0, 2, 3, 4]], [G 1 [0], G 2 [1], G 2 [2], G 2 [3], G 1 [4], G 7 [0, 1], G 7 [0, 2], G 7 [0, 3], G 6 [1, 2], G 6 [1, 3], G 7 [1, 4], G 6 [2, 3], G 7 [2, 4], G 7 [3, 4], G 1 [0, 1, 2], G 1 [0, 1, 3], G 1 [0, 2, 3], G 2 [1, 2, 3], G 1 [1, 2, 4], G 1 [1, 3, 4], G 1 [2, 3, 4], G 7 [0, 1, 2, 3], G 7 [1, 2, 3, 4]], [G 2 [0], G 2 [1], G 1 [2], G 1 [3], G 2 [4], G 6 [0, 1], G 7 [0, 2], G 7 [0, 3], G 6 [0, 4], G 7 [1, 2], G 7 [1, 3], G 6 [1, 4], G 7 [2, 4], G 7 [3, 4], G 1 [0, 1, 2], G 1 [0, 1, 3], G 2 [0, 1, 4], G 1 [0, 2, 4], G 1 [0, 3, 4], G 1 [1, 2, 4], G 1 [1, 3, 4], G 7 [0, 1, 2, 4], G 7 [0, 1, 3, 4]], [G 2 [0], G 1 [1], G 2 [2], G 1 [3], G 2 [4], G 7 [0, 1], G 6 [0, 2], G 7 [0, 3], G 6 [0, 4], G 7 [1, 2], G 7 [1, 4], G 7 [2, 3], G 6 [2, 4], G 7 [3, 4], G 1 [0, 1, 2], G 1 [0, 1, 4], G 1 [0, 2, 3], G 2 [0, 2, 4], G 1 [0, 3, 4], G 1 [1, 2, 4], G 1 [2, 3, 4], G 7 [0, 1, 2, 4], G 7 [0, 2, 3, 4]], [G 1 [0], G 2 [1], G 2 [2], G 1 [3], G 2 [4], G 7 [0, 1], G 7 [0, 2], G 7 [0, 4], G 6 [1, 2], G 7 [1, 3], G 6 [1, 4], G 7 [2, 3], G 6 [2, 4], G 7 [3, 4], G 1 [0, 1, 2], G 1 [0, 1, 4], G 1 [0, 2, 4], G 1 [1, 2, 3], G 2 [1, 2, 4], G 1 [1, 3, 4], G 1 [2, 3, 4], G 7 [0, 1, 2, 4], G 7 [1, 2, 3, 4]], [G 2 [0], G 1 [1], G 1 [2], G 2 [3], G 2 [4], G 7 [0, 1], G 7 [0, 2], G 6 [0, 3], G 6 [0, 4], G 7 [1, 3], G 7 [1, 4], G 7 [2, 3], G 7 [2, 4], G 6 [3, 4], G 1 [0, 1, 3], G 1 [0, 1, 4], G 1 [0, 2, 3], G 1 [0, 2, 4], G 2 [0, 3, 4], G 1 [1, 3, 4], G 1 [2, 3, 4], G 7 [0, 1, 3, 4], G 7 [0, 2, 3, 4]], [G 1 [0], G 2 [1], G 1 [2], G 2 [3], G 2 [4], G 7 [0, 1], G 7 [0, 3], G 7 [0, 4], G 7 [1, 2], G 6 [1, 3], G 6 [1, 4], G 7 [2, 3], G 7 [2, 4], G 6 [3, 4], G 1 [0, 1, 3], G 1 [0, 1, 4], G 1 [0, 3, 4], G 1 [1, 2, 3], G 1 [1, 2, 4], G 2 [1, 3, 4], G 1 [2, 3, 4], G 7 [0, 1, 3, 4], G 7 [1, 2, 3, 4]], [G 1 [0], G 1 [1], G 2 [2], G 2 [3], G 2 [4], G 7 [0, 2], G 7 [0, 3], G 7 [0, 4], G 7 [1, 2], G 7 [1, 3], G 7 [1, 4], G 6 [2, 3], G 6 [2, 4], G 6 [3, 4], G 1 [0, 2, 3], G 1 [0, 2, 4], G 1 [0, 3, 4], G 1 [1, 2, 3], G 1 [1, 2, 4], G 1 [1, 3, 4], G 2 [2, 3, 4], G 7 [0, 2, 3, 4], G 7 [1, 2, 3, 4]], [G 3 [0], G 3 [1], G 2 [2], G 2 [3], G 2 [4], G 5 [0, 1], G 6 [0, 2], G 6 [0, 3], G 6 [0, 4], G 6 [1, 2], G 6 [1, 3], G 6 [1, 4], G 7 [2, 3], G 7 [2, 4], G 7 [3, 4], G 2 [0, 1, 2], G 2 [0, 1, 3], G 2 [0, 1, 4], G 1 [0, 2, 3], G 1 [0, 2, 4], G 1 [0, 3, 4], G 1 [1, 2, 3], G 1 [1, 2, 4], G 1 [1, 3, 4], G 7 [0, 1, 2, 3], G 7 [0, 1, 2, 4], G 7 [0, 1, 3, 4]], [G 3 [0], G 2 [1], G 3 [2], G 2 [3], G 2 [4], G 6 [0, 1], G 5 [0, 2], G 6 [0, 3], G 6 [0, 4], G 6 [1, 2], G 7 [1, 3], G 7 [1, 4], G 6 [2, 3], G 6 [2, 4], G 7 [3, 4], G 2 [0, 1, 2], G 1 [0, 1, 3], G 1 [0, 1, 4], G 2 [0, 2, 3], G 2 [0, 2, 4], G 1 [0, 3, 4], G 1 [1, 2, 3], G 1 [1, 2, 4], G 1 [2, 3, 4], G 7 [0, 1, 2, 3], G 7 [0, 1, 2, 4], G 7 [0, 2, 3, 4]], [G 2 [0], G 3 [1], G 3 [2], G 2 [3], G 2 [4], G 6 [0, 1], G 6 [0, 2], G 7 [0, 3], G 7 [0, 4], G 5 [1, 2], G 6 [1, 3], G 6 [1, 4], G 6 [2, 3], G 6 [2, 4], G 7 [3, 4], G 2 [0, 1, 2], G 1 [0, 1, 3], G 1 [0, 1, 4], G 1 [0, 2, 3], G 1 [0, 2, 4], G 2 [1, 2, 3], G 2 [1, 2, 4], G 1 [1, 3, 4], G 1 [2, 3, 4], G 7 [0, 1, 2, 3], G 7 [0, 1, 2, 4], G 7 [1, 2, 3, 4]], [G 3 [0], G 2 [1], G 2 [2], G 3 [3], G 2 [4], G 6 [0, 1], G 6 [0, 2], G 5 [0, 3], G 6 [0, 4], G 7 [1, 2], G 6 [1, 3], G 7 [1, 4], G 6 [2, 3], G 7 [2, 4], G 6 [3, 4], G 1 [0, 1, 2], G 2 [0, 1, 3], G 1 [0, 1, 4], G 2 [0, 2, 3], G 1 [0, 2, 4], G 2 [0, 3, 4], G 1 [1, 2, 3], G 1 [1, 3, 4], G 1 [2, 3, 4], G 7 [0, 1, 2, 3], G 7 [0, 1, 3, 4], G 7 [0, 2, 3, 4]], [G 2 [0], G 3 [1], G 2 [2], G 3 [3], G 2 [4], G 6 [0, 1], G 7 [0, 2], G 6 [0, 3], G 7 [0, 4], G 6 [1, 2], G 5 [1, 3], G 6 [1, 4], G 6 [2, 3], G 7 [2, 4], G 6 [3, 4], G 1 [0, 1, 2], G 2 [0, 1, 3], G 1 [0, 1, 4], G 1 [0, 2, 3], G 1 [0, 3, 4], G 2 [1, 2, 3], G 1 [1, 2, 4], G 2 [1, 3, 4], G 1 [2, 3, 4], G 7 [0, 1, 2, 3], G 7 [0, 1, 3, 4], G 7 [1, 2, 3, 4]], [G 2 [0], G 2 [1], G 3 [2], G 3 [3], G 2 [4], G 7 [0, 1], G 6 [0, 2], G 6 [0, 3], G 7 [0, 4], G 6 [1, 2], G 6 [1, 3], G 7 [1, 4], G 5 [2, 3], G 6 [2, 4], G 6 [3, 4], G 1 [0, 1, 2], G 1 [0, 1, 3], G 2 [0, 2, 3], G 1 [0, 2, 4], G 1 [0, 3, 4], G 2 [1, 2, 3], G 1 [1, 2, 4], G 1 [1, 3, 4], G 2 [2, 3, 4], G 7 [0, 1, 2, 3], G 7 [0, 2, 3, 4], G 7 [1, 2, 3, 4]], [G 3 [0], G 2 [1], G 2 [2], G 2 [3], G 3 [4], G 6 [0, 1], G 6 [0, 2], G 6 [0, 3], G 5 [0, 4], G 7 [1, 2], G 7 [1, 3], G 6 [1, 4], G 7 [2, 3], G 6 [2, 4], G 6 [3, 4], G 1 [0, 1, 2], G 1 [0, 1, 3], G 2 [0, 1, 4], G 1 [0, 2, 3], G 2 [0, 2, 4], G 2 [0, 3, 4], G 1 [1, 2, 4], G 1 [1, 3, 4], G 1 [2, 3, 4], G 7 [0, 1, 2, 4], G 7 [0, 1, 3, 4], G 7 [0, 2, 3, 4]], [G 2 [0], G 3 [1], G 2 [2], G 2 [3], G 3 [4], G 6 [0, 1], G 7 [0, 2], G 7 [0, 3], G 6 [0, 4], G 6 [1, 2], G 6 [1, 3], G 5 [1, 4], G 7 [2, 3], G 6 [2, 4], G 6 [3, 4], G 1 [0, 1, 2], G 1 [0, 1, 3], G 2 [0, 1, 4], G 1 [0, 2, 4], G 1 [0, 3, 4], G 1 [1, 2, 3], G 2 [1, 2, 4], G 2 [1, 3, 4], G 1 [2, 3, 4], G 7 [0, 1, 2, 4], G 7 [0, 1, 3, 4], G 7 [1, 2, 3, 4]], [G 2 [0], G 2 [1], G 3 [2], G 2 [3], G 3 [4], G 7 [0, 1], G 6 [0, 2], G 7 [0, 3], G 6 [0, 4], G 6 [1, 2], G 7 [1, 3], G 6 [1, 4], G 6 [2, 3], G 5 [2, 4], G 6 [3, 4], G 1 [0, 1, 2], G 1 [0, 1, 4], G 1 [0, 2, 3], G 2 [0, 2, 4], G 1 [0, 3, 4], G 1 [1, 2, 3], G 2 [1, 2, 4], G 1 [1, 3, 4], G 2 [2, 3, 4], G 7 [0, 1, 2, 4], G 7 [0, 2, 3, 4], G 7 [1, 2, 3, 4]], [G 2 [0], G 2 [1], G 2 [2], G 3 [3], G 3 [4], G 7 [0, 1], G 7 [0, 2], G 6 [0, 3], G 6 [0, 4], G 7 [1, 2], G 6 [1, 3], G 6 [1, 4], G 6 [2, 3], G 6 [2, 4], G 5 [3, 4], G 1 [0, 1, 3], G 1 [0, 1, 4], G 1 [0, 2, 3], G 1 [0, 2, 4], G 2 [0, 3, 4], G 1 [1, 2, 3], G 1 [1, 2, 4], G 2 [1, 3, 4], G 2 [2, 3, 4], G 7 [0, 1, 3, 4], G 7 [0, 2, 3, 4], G 7 [1, 2, 3, 4]], [G 4 [0], G 3 [1], G 3 [2], G 3 [3], G 3 [4], G 5 [0, 1], G 5 [0, 2], G 5 [0, 3], G 5 [0, 4], G 6 [1, 2], G 6 [1, 3], G 6 [1, 4], G 6 [2, 3], G 6 [2, 4], G 6 [3, 4], G 2 [0, 1, 2], G 2 [0, 1, 3], G 2 [0, 1, 4], G 2 [0, 2, 3], G 2 [0, 2, 4], G 2 [0, 3, 4], G 1 [1, 2, 3], G 1 [1, 2, 4], G 1 [1, 3, 4], G 1 [2, 3, 4], G 7 [0, 1, 2, 3], G 7 [0, 1, 2, 4], G 7 [0, 1, 3, 4], G 7 [0, 2, 3, 4]], [G 3 [0], G 4 [1], G 3 [2], G 3 [3], G 3 [4], G 5 [0, 1], G 6 [0, 2], G 6 [0, 3], G 6 [0, 4], G 5 [1, 2], G 5 [1, 3], G 5 [1, 4], G 6 [2, 3], G 6 [2, 4], G 6 [3, 4], G 2 [0, 1, 2], G 2 [0, 1, 3], G 2 [0, 1, 4], G 1 [0, 2, 3], G 1 [0, 2, 4], G 1 [0, 3, 4], G 2 [1, 2, 3], G 2 [1, 2, 4], G 2 [1, 3, 4], G 1 [2, 3, 4], G 7 [0, 1, 2, 3], G 7 [0, 1, 2, 4], G 7 [0, 1, 3, 4], G 7 [1, 2, 3, 4]], [G 3 [0], G 3 [1], G 4 [2], G 3 [3], G 3 [4], G 6 [0, 1], G 5 [0, 2], G 6 [0, 3], G 6 [0, 4], G 5 [1, 2], G 6 [1, 3], G 6 [1, 4], G 5 [2, 3], G 5 [2, 4], G 6 [3, 4], G 2 [0, 1, 2], G 1 [0, 1, 3], G 1 [0, 1, 4], G 2 [0, 2, 3], G 2 [0, 2, 4], G 1 [0, 3, 4], G 2 [1, 2, 3], G 2 [1, 2, 4], G 1 [1, 3, 4], G 2 [2, 3, 4], G 7 [0, 1, 2, 3], G 7 [0, 1, 2, 4], G 7 [0, 2, 3, 4], G 7 [1, 2, 3, 4]], [G 3 [0], G 3 [1], G 3 [2], G 4 [3], G 3 [4], G 6 [0, 1], G 6 [0, 2], G 5 [0, 3], G 6 [0, 4], G 6 [1, 2], G 5 [1, 3], G 6 [1, 4], G 5 [2, 3], G 6 [2, 4], G 5 [3, 4], G 1 [0, 1, 2], G 2 [0, 1, 3], G 1 [0, 1, 4], G 2 [0, 2, 3], G 1 [0, 2, 4], G 2 [0, 3, 4], G 2 [1, 2, 3], G 1 [1, 2, 4], G 2 [1, 3, 4], G 2 [2, 3, 4], G 7 [0, 1, 2, 3], G 7 [0, 1, 3, 4], G 7 [0, 2, 3, 4], G 7 [1, 2, 3, 4]], [G 3 [0], G 3 [1], G 3 [2], G 3 [3], G 4 [4], G 6 [0, 1], G 6 [0, 2], G 6 [0, 3], G 5 [0, 4], G 6 [1, 2], G 6 [1, 3], G 5 [1, 4], G 6 [2, 3], G 5 [2, 4], G 5 [3, 4], G 1 [0, 1, 2], G 1 [0, 1, 3], G 2 [0, 1, 4], G 1 [0, 2, 3], G 2 [0, 2, 4], G 2 [0, 3, 4], G 1 [1, 2, 3], G 2 [1, 2, 4], G 2 [1, 3, 4], G 2 [2, 3, 4], G 7 [0, 1, 2, 4], G 7 [0, 1, 3, 4], G 7 [0, 2, 3, 4], G 7 [1, 2, 3, 4]], [G 3 [0], G 3 [1], G 3 [2], G 3 [3], G 3 [4], G 6 [0, 1], G 6 [0, 2], G 6 [0, 3], G 6 [0, 4], G 6 [1, 2], G 6 [1, 3], G 6 [1, 4], G 6 [2, 3], G 6 [2, 4], G 6 [3, 4], G 1 [0, 1, 2], G 1 [0, 1, 3], G 1 [0, 1, 4], G 1 [0, 2, 3], G 1 [0, 2, 4], G 1 [0, 3, 4], G 1 [1, 2, 3], G 1 [1, 2, 4], G 1 [1, 3, 4], G 1 [2, 3, 4], G 7 [0, 1, 2, 3, 4]], [G 4 [0], G 4 [1], G 4 [2], G 4 [3], G 3 [4], G 5 [0, 1], G 5 [0, 2], G 5 [0, 3], G 6 [0, 4], G 5 [1, 2], G 5 [1, 3], G 6 [1, 4], G 5 [2, 3], G 6 [2, 4], G 6 [3, 4], G 2 [0, 1, 2], G 2 [0, 1, 3], G 1 [0, 1, 4], G 2 [0, 2, 3], G 1 [0, 2, 4], G 1 [0, 3, 4], G 2 [1, 2, 3], G 1 [1, 2, 4], G 1 [1, 3, 4], G 1 [2, 3, 4], G 7 [0, 1, 2, 3], G 7 [0, 1, 2, 3, 4]], [G 4 [0], G 4 [1], G 4 [2], G 3 [3], G 4 [4], G 5 [0, 1], G 5 [0, 2], G 6 [0, 3], G 5 [0, 4], G 5 [1, 2], G 6 [1, 3], G 5 [1, 4], G 6 [2, 3], G 5 [2, 4], G 6 [3, 4], G 2 [0, 1, 2], G 1 [0, 1, 3], G 2 [0, 1, 4], G 1 [0, 2, 3], G 2 [0, 2, 4], G 1 [0, 3, 4], G 1 [1, 2, 3], G 2 [1, 2, 4], G 1 [1, 3, 4], G 1 [2, 3, 4], G 7 [0, 1, 2, 4], G 7 [0, 1, 2, 3, 4]], [G 4 [0], G 4 [1], G 3 [2], G 4 [3], G 4 [4], G 5 [0, 1], G 6 [0, 2], G 5 [0, 3], G 5 [0, 4], G 6 [1, 2], G 5 [1, 3], G 5 [1, 4], G 6 [2, 3], G 6 [2, 4], G 5 [3, 4], G 1 [0, 1, 2], G 2 [0, 1, 3], G 2 [0, 1, 4], G 1 [0, 2, 3], G 1 [0, 2, 4], G 2 [0, 3, 4], G 1 [1, 2, 3], G 1 [1, 2, 4], G 2 [1, 3, 4], G 1 [2, 3, 4], G 7 [0, 1, 3, 4], G 7 [0, 1, 2, 3, 4]], [G 4 [0], G 3 [1], G 4 [2], G 4 [3], G 4 [4], G 6 [0, 1], G 5 [0, 2], G 5 [0, 3], G 5 [0, 4], G 6 [1, 2], G 6 [1, 3], G 6 [1, 4], G 5 [2, 3], G 5 [2, 4], G 5 [3, 4], G 1 [0, 1, 2], G 1 [0, 1, 3], G 1 [0, 1, 4], G 2 [0, 2, 3], G 2 [0, 2, 4], G 2 [0, 3, 4], G 1 [1, 2, 3], G 1 [1, 2, 4], G 1 [1, 3, 4], G 2 [2, 3, 4], G 7 [0, 2, 3, 4], G 7 [0, 1, 2, 3, 4]], [G 3 [0], G 4 [1], G 4 [2], G 4 [3], G 4 [4], G 6 [0, 1], G 6 [0, 2], G 6 [0, 3], G 6 [0, 4], G 5 [1, 2], G 5 [1, 3], G 5 [1, 4], G 5 [2, 3], G 5 [2, 4], G 5 [3, 4], G 1 [0, 1, 2], G 1 [0, 1, 3], G 1 [0, 1, 4], G 1 [0, 2, 3], G 1 [0, 2, 4], G 1 [0, 3, 4], G 2 [1, 2, 3], G 2 [1, 2, 4], G 2 [1, 3, 4], G 2 [2, 3, 4], G 7 [1, 2, 3, 4], G 7 [0, 1, 2, 3, 4]], [G 5 [0], G 5 [1], G 5 [2], G 4 [3], G 4 [4], G 4 [0, 1], G 4 [0, 2], G 5 [0, 3], G 5 [0, 4], G 4 [1, 2], G 5 [1, 3], G 5 [1, 4], G 5 [2, 3], G 5 [2, 4], G 6 [3, 4], G 3 [0, 1, 2], G 2 [0, 1, 3], G 2 [0, 1, 4], G 2 [0, 2, 3], G 2 [0, 2, 4], G 1 [0, 3, 4], G 2 [1, 2, 3], G 2 [1, 2, 4], G 1 [1, 3, 4], G 1 [2, 3, 4], G 7 [0, 1, 2, 3], G 7 [0, 1, 2, 4], G 7 [0, 1, 2, 3, 4]], [G 5 [0], G 5 [1], G 4 [2], G 5 [3], G 4 [4], G 4 [0, 1], G 5 [0, 2], G 4 [0, 3], G 5 [0, 4], G 5 [1, 2], G 4 [1, 3], G 5 [1, 4], G 5 [2, 3], G 6 [2, 4], G 5 [3, 4], G 2 [0, 1, 2], G 3 [0, 1, 3], G 2 [0, 1, 4], G 2 [0, 2, 3], G 1 [0, 2, 4], G 2 [0, 3, 4], G 2 [1, 2, 3], G 1 [1, 2, 4], G 2 [1, 3, 4], G 1 [2, 3, 4], G 7 [0, 1, 2, 3], G 7 [0, 1, 3, 4], G 7 [0, 1, 2, 3, 4]], [G 5 [0], G 4 [1], G 5 [2], G 5 [3], G 4 [4], G 5 [0, 1], G 4 [0, 2], G 4 [0, 3], G 5 [0, 4], G 5 [1, 2], G 5 [1, 3], G 6 [1, 4], G 4 [2, 3], G 5 [2, 4], G 5 [3, 4], G 2 [0, 1, 2], G 2 [0, 1, 3], G 1 [0, 1, 4], G 3 [0, 2, 3], G 2 [0, 2, 4], G 2 [0, 3, 4], G 2 [1, 2, 3], G 1 [1, 2, 4], G 1 [1, 3, 4], G 2 [2, 3, 4], G 7 [0, 1, 2, 3], G 7 [0, 2, 3, 4], G 7 [0, 1, 2, 3, 4]], [G 4 [0], G 5 [1], G 5 [2], G 5 [3], G 4 [4], G 5 [0, 1], G 5 [0, 2], G 5 [0, 3], G 6 [0, 4], G 4 [1, 2], G 4 [1, 3], G 5 [1, 4], G 4 [2, 3], G 5 [2, 4], G 5 [3, 4], G 2 [0, 1, 2], G 2 [0, 1, 3], G 1 [0, 1, 4], G 2 [0, 2, 3], G 1 [0, 2, 4], G 1 [0, 3, 4], G 3 [1, 2, 3], G 2 [1, 2, 4], G 2 [1, 3, 4], G 2 [2, 3, 4], G 7 [0, 1, 2, 3], G 7 [1, 2, 3, 4], G 7 [0, 1, 2, 3, 4]], [G 5 [0], G 5 [1], G 4 [2], G 4 [3], G 5 [4], G 4 [0, 1], G 5 [0, 2], G 5 [0, 3], G 4 [0, 4], G 5 [1, 2], G 5 [1, 3], G 4 [1, 4], G 6 [2, 3], G 5 [2, 4], G 5 [3, 4], G 2 [0, 1, 2], G 2 [0, 1, 3], G 3 [0, 1, 4], G 1 [0, 2, 3], G 2 [0, 2, 4], G 2 [0, 3, 4], G 1 [1, 2, 3], G 2 [1, 2, 4], G 2 [1, 3, 4], G 1 [2, 3, 4], G 7 [0, 1, 2, 4], G 7 [0, 1, 3, 4], G 7 [0, 1, 2, 3, 4]], [G 5 [0], G 4 [1], G 5 [2], G 4 [3], G 5 [4], G 5 [0, 1], G 4 [0, 2], G 5 [0, 3], G 4 [0, 4], G 5 [1, 2], G 6 [1, 3], G 5 [1, 4], G 5 [2, 3], G 4 [2, 4], G 5 [3, 4], G 2 [0, 1, 2], G 1 [0, 1, 3], G 2 [0, 1, 4], G 2 [0, 2, 3], G 3 [0, 2, 4], G 2 [0, 3, 4], G 1 [1, 2, 3], G 2 [1, 2, 4], G 1 [1, 3, 4], G 2 [2, 3, 4], G 7 [0, 1, 2, 4], G 7 [0, 2, 3, 4], G 7 [0, 1, 2, 3, 4]], [G 4 [0], G 5 [1], G 5 [2], G 4 [3], G 5 [4], G 5 [0, 1], G 5 [0, 2], G 6 [0, 3], G 5 [0, 4], G 4 [1, 2], G 5 [1, 3], G 4 [1, 4], G 5 [2, 3], G 4 [2, 4], G 5 [3, 4], G 2 [0, 1, 2], G 1 [0, 1, 3], G 2 [0, 1, 4], G 1 [0, 2, 3], G 2 [0, 2, 4], G 1 [0, 3, 4], G 2 [1, 2, 3], G 3 [1, 2, 4], G 2 [1, 3, 4], G 2 [2, 3, 4], G 7 [0, 1, 2, 4], G 7 [1, 2, 3, 4], G 7 [0, 1, 2, 3, 4]], [G 5 [0], G 4 [1], G 4 [2], G 5 [3], G 5 [4], G 5 [0, 1], G 5 [0, 2], G 4 [0, 3], G 4 [0, 4], G 6 [1, 2], G 5 [1, 3], G 5 [1, 4], G 5 [2, 3], G 5 [2, 4], G 4 [3, 4], G 1 [0, 1, 2], G 2 [0, 1, 3], G 2 [0, 1, 4], G 2 [0, 2, 3], G 2 [0, 2, 4], G 3 [0, 3, 4], G 1 [1, 2, 3], G 1 [1, 2, 4], G 2 [1, 3, 4], G 2 [2, 3, 4], G 7 [0, 1, 3, 4], G 7 [0, 2, 3, 4], G 7 [0, 1, 2, 3, 4]], [G 4 [0], G 5 [1], G 4 [2], G 5 [3], G 5 [4], G 5 [0, 1], G 6 [0, 2], G 5 [0, 3], G 5 [0, 4], G 5 [1, 2], G 4 [1, 3], G 4 [1, 4], G 5 [2, 3], G 5 [2, 4], G 4 [3, 4], G 1 [0, 1, 2], G 2 [0, 1, 3], G 2 [0, 1, 4], G 1 [0, 2, 3], G 1 [0, 2, 4], G 2 [0, 3, 4], G 2 [1, 2, 3], G 2 [1, 2, 4], G 3 [1, 3, 4], G 2 [2, 3, 4], G 7 [0, 1, 3, 4], G 7 [1, 2, 3, 4], G 7 [0, 1, 2, 3, 4]], [G 4 [0], G 4 [1], G 5 [2], G 5 [3], G 5 [4], G 6 [0, 1], G 5 [0, 2], G 5 [0, 3], G 5 [0, 4], G 5 [1, 2], G 5 [1, 3], G 5 [1, 4], G 4 [2, 3], G 4 [2, 4], G 4 [3, 4], G 1 [0, 1, 2], G 1 [0, 1, 3], G 1 [0, 1, 4], G 2 [0, 2, 3], G 2 [0, 2, 4], G 2 [0, 3, 4], G 2 [1, 2, 3], G 2 [1, 2, 4], G 2 [1, 3, 4], G 3 [2, 3, 4], G 7 [0, 2, 3, 4], G 7 [1, 2, 3, 4], G 7 [0, 1, 2, 3, 4]], [G 6 [0], G 6 [1], G 5 [2], G 5 [3], G 5 [4], G 3 [0, 1], G 4 [0, 2], G 4 [0, 3], G 4 [0, 4], G 4 [1, 2], G 4 [1, 3], G 4 [1, 4], G 5 [2, 3], G 5 [2, 4], G 5 [3, 4], G 3 [0, 1, 2], G 3 [0, 1, 3], G 3 [0, 1, 4], G 2 [0, 2, 3], G 2 [0, 2, 4], G 2 [0, 3, 4], G 2 [1, 2, 3], G 2 [1, 2, 4], G 2 [1, 3, 4], G 1 [2, 3, 4], G 7 [0, 1, 2, 3], G 7 [0, 1, 2, 4], G 7 [0, 1, 3, 4], G 7 [0, 1, 2, 3, 4]], [G 6 [0], G 5 [1], G 6 [2], G 5 [3], G 5 [4], G 4 [0, 1], G 3 [0, 2], G 4 [0, 3], G 4 [0, 4], G 4 [1, 2], G 5 [1, 3], G 5 [1, 4], G 4 [2, 3], G 4 [2, 4], G 5 [3, 4], G 3 [0, 1, 2], G 2 [0, 1, 3], G 2 [0, 1, 4], G 3 [0, 2, 3], G 3 [0, 2, 4], G 2 [0, 3, 4], G 2 [1, 2, 3], G 2 [1, 2, 4], G 1 [1, 3, 4], G 2 [2, 3, 4], G 7 [0, 1, 2, 3], G 7 [0, 1, 2, 4], G 7 [0, 2, 3, 4], G 7 [0, 1, 2, 3, 4]], [G 5 [0], G 6 [1], G 6 [2], G 5 [3], G 5 [4], G 4 [0, 1], G 4 [0, 2], G 5 [0, 3], G 5 [0, 4], G 3 [1, 2], G 4 [1, 3], G 4 [1, 4], G 4 [2, 3], G 4 [2, 4], G 5 [3, 4], G 3 [0, 1, 2], G 2 [0, 1, 3], G 2 [0, 1, 4], G 2 [0, 2, 3], G 2 [0, 2, 4], G 1 [0, 3, 4], G 3 [1, 2, 3], G 3 [1, 2, 4], G 2 [1, 3, 4], G 2 [2, 3, 4], G 7 [0, 1, 2, 3], G 7 [0, 1, 2, 4], G 7 [1, 2, 3, 4], G 7 [0, 1, 2, 3, 4]], [G 6 [0], G 5 [1], G 5 [2], G 6 [3], G 5 [4], G 4 [0, 1], G 4 [0, 2], G 3 [0, 3], G 4 [0, 4], G 5 [1, 2], G 4 [1, 3], G 5 [1, 4], G 4 [2, 3], G 5 [2, 4], G 4 [3, 4], G 2 [0, 1, 2], G 3 [0, 1, 3], G 2 [0, 1, 4], G 3 [0, 2, 3], G 2 [0, 2, 4], G 3 [0, 3, 4], G 2 [1, 2, 3], G 1 [1, 2, 4], G 2 [1, 3, 4], G 2 [2, 3, 4], G 7 [0, 1, 2, 3], G 7 [0, 1, 3, 4], G 7 [0, 2, 3, 4], G 7 [0, 1, 2, 3, 4]], [G 5 [0], G 6 [1], G 5 [2], G 6 [3], G 5 [4], G 4 [0, 1], G 5 [0, 2], G 4 [0, 3], G 5 [0, 4], G 4 [1, 2], G 3 [1, 3], G 4 [1, 4], G 4 [2, 3], G 5 [2, 4], G 4 [3, 4], G 2 [0, 1, 2], G 3 [0, 1, 3], G 2 [0, 1, 4], G 2 [0, 2, 3], G 1 [0, 2, 4], G 2 [0, 3, 4], G 3 [1, 2, 3], G 2 [1, 2, 4], G 3 [1, 3, 4], G 2 [2, 3, 4], G 7 [0, 1, 2, 3], G 7 [0, 1, 3, 4], G 7 [1, 2, 3, 4], G 7 [0, 1, 2, 3, 4]], [G 5 [0], G 5 [1], G 6 [2], G 6 [3], G 5 [4], G 5 [0, 1], G 4 [0, 2], G 4 [0, 3], G 5 [0, 4], G 4 [1, 2], G 4 [1, 3], G 5 [1, 4], G 3 [2, 3], G 4 [2, 4], G 4 [3, 4], G 2 [0, 1, 2], G 2 [0, 1, 3], G 1 [0, 1, 4], G 3 [0, 2, 3], G 2 [0, 2, 4], G 2 [0, 3, 4], G 3 [1, 2, 3], G 2 [1, 2, 4], G 2 [1, 3, 4], G 3 [2, 3, 4], G 7 [0, 1, 2, 3], G 7 [0, 2, 3, 4], G 7 [1, 2, 3, 4], G 7 [0, 1, 2, 3, 4]], [G 6 [0], G 5 [1], G 5 [2], G 5 [3], G 6 [4], G 4 [0, 1], G 4 [0, 2], G 4 [0, 3], G 3 [0, 4], G 5 [1, 2], G 5 [1, 3], G 4 [1, 4], G 5 [2, 3], G 4 [2, 4], G 4 [3, 4], G 2 [0, 1, 2], G 2 [0, 1, 3], G 3 [0, 1, 4], G 2 [0, 2, 3], G 3 [0, 2, 4], G 3 [0, 3, 4], G 1 [1, 2, 3], G 2 [1, 2, 4], G 2 [1, 3, 4], G 2 [2, 3, 4], G 7 [0, 1, 2, 4], G 7 [0, 1, 3, 4], G 7 [0, 2, 3, 4], G 7 [0, 1, 2, 3, 4]], [G 5 [0], G 6 [1], G 5 [2], G 5 [3], G 6 [4], G 4 [0, 1], G 5 [0, 2], G 5 [0, 3], G 4 [0, 4], G 4 [1, 2], G 4 [1, 3], G 3 [1, 4], G 5 [2, 3], G 4 [2, 4], G 4 [3, 4], G 2 [0, 1, 2], G 2 [0, 1, 3], G 3 [0, 1, 4], G 1 [0, 2, 3], G 2 [0, 2, 4], G 2 [0, 3, 4], G 2 [1, 2, 3], G 3 [1, 2, 4], G 3 [1, 3, 4], G 2 [2, 3, 4], G 7 [0, 1, 2, 4], G 7 [0, 1, 3, 4], G 7 [1, 2, 3, 4], G 7 [0, 1, 2, 3, 4]], [G 5 [0], G 5 [1], G 6 [2], G 5 [3], G 6 [4], G 5 [0, 1], G 4 [0, 2], G 5 [0, 3], G 4 [0, 4], G 4 [1, 2], G 5 [1, 3], G 4 [1, 4], G 4 [2, 3], G 3 [2, 4], G 4 [3, 4], G 2 [0, 1, 2], G 1 [0, 1, 3], G 2 [0, 1, 4], G 2 [0, 2, 3], G 3 [0, 2, 4], G 2 [0, 3, 4], G 2 [1, 2, 3], G 3 [1, 2, 4], G 2 [1, 3, 4], G 3 [2, 3, 4], G 7 [0, 1, 2, 4], G 7 [0, 2, 3, 4], G 7 [1, 2, 3, 4], G 7 [0, 1, 2, 3, 4]], [G 5 [0], G 5 [1], G 5 [2], G 6 [3], G 6 [4], G 5 [0, 1], G 5 [0, 2], G 4 [0, 3], G 4 [0, 4], G 5 [1, 2], G 4 [1, 3], G 4 [1, 4], G 4 [2, 3], G 4 [2, 4], G 3 [3, 4], G 1 [0, 1, 2], G 2 [0, 1, 3], G 2 [0, 1, 4], G 2 [0, 2, 3], G 2 [0, 2, 4], G 3 [0, 3, 4], G 2 [1, 2, 3], G 2 [1, 2, 4], G 3 [1, 3, 4], G 3 [2, 3, 4], G 7 [0, 1, 3, 4], G 7 [0, 2, 3, 4], G 7 [1, 2, 3, 4], G 7 [0, 1, 2, 3, 4]], [G 7 [0], G 6 [1], G 6 [2], G 6 [3], G 6 [4], G 3 [0, 1], G 3 [0, 2], G 3 [0, 3], G 3 [0, 4], G 4 [1, 2], G 4 [1, 3], G 4 [1, 4], G 4 [2, 3], G 4 [2, 4], G 4 [3, 4], G 3 [0, 1, 2], G 3 [0, 1, 3], G 3 [0, 1, 4], G 3 [0, 2, 3], G 3 [0, 2, 4], G 3 [0, 3, 4], G 2 [1, 2, 3], G 2 [1, 2, 4], G 2 [1, 3, 4], G 2 [2, 3, 4], G 7 [0, 1, 2, 3], G 7 [0, 1, 2, 4], G 7 [0, 1, 3, 4], G 7 [0, 2, 3, 4], G 7 [0, 1, 2, 3, 4]], [G 6 [0], G 7 [1], G 6 [2], G 6 [3], G 6 [4], G 3 [0, 1], G 4 [0, 2], G 4 [0, 3], G 4 [0, 4], G 3 [1, 2], G 3 [1, 3], G 3 [1, 4], G 4 [2, 3], G 4 [2, 4], G 4 [3, 4], G 3 [0, 1, 2], G 3 [0, 1, 3], G 3 [0, 1, 4], G 2 [0, 2, 3], G 2 [0, 2, 4], G 2 [0, 3, 4], G 3 [1, 2, 3], G 3 [1, 2, 4], G 3 [1, 3, 4], G 2 [2, 3, 4], G 7 [0, 1, 2, 3], G 7 [0, 1, 2, 4], G 7 [0, 1, 3, 4], G 7 [1, 2, 3, 4], G 7 [0, 1, 2, 3, 4]], [G 6 [0], G 6 [1], G 7 [2], G 6 [3], G 6 [4], G 4 [0, 1], G 3 [0, 2], G 4 [0, 3], G 4 [0, 4], G 3 [1, 2], G 4 [1, 3], G 4 [1, 4], G 3 [2, 3], G 3 [2, 4], G 4 [3, 4], G 3 [0, 1, 2], G 2 [0, 1, 3], G 2 [0, 1, 4], G 3 [0, 2, 3], G 3 [0, 2, 4], G 2 [0, 3, 4], G 3 [1, 2, 3], G 3 [1, 2, 4], G 2 [1, 3, 4], G 3 [2, 3, 4], G 7 [0, 1, 2, 3], G 7 [0, 1, 2, 4], G 7 [0, 2, 3, 4], G 7 [1, 2, 3, 4], G 7 [0, 1, 2, 3, 4]], [G 6 [0], G 6 [1], G 6 [2], G 7 [3], G 6 [4], G 4 [0, 1], G 4 [0, 2], G 3 [0, 3], G 4 [0, 4], G 4 [1, 2], G 3 [1, 3], G 4 [1, 4], G 3 [2, 3], G 4 [2, 4], G 3 [3, 4], G 2 [0, 1, 2], G 3 [0, 1, 3], G 2 [0, 1, 4], G 3 [0, 2, 3], G 2 [0, 2, 4], G 3 [0, 3, 4], G 3 [1, 2, 3], G 2 [1, 2, 4], G 3 [1, 3, 4], G 3 [2, 3, 4], G 7 [0, 1, 2, 3], G 7 [0, 1, 3, 4], G 7 [0, 2, 3, 4], G 7 [1, 2, 3, 4], G 7 [0, 1, 2, 3, 4]], [G 6 [0], G 6 [1], G 6 [2], G 6 [3], G 7 [4], G 4 [0, 1], G 4 [0, 2], G 4 [0, 3], G 3 [0, 4], G 4 [1, 2], G 4 [1, 3], G 3 [1, 4], G 4 [2, 3], G 3 [2, 4], G 3 [3, 4], G 2 [0, 1, 2], G 2 [0, 1, 3], G 3 [0, 1, 4], G 2 [0, 2, 3], G 3 [0, 2, 4], G 3 [0, 3, 4], G 2 [1, 2, 3], G 3 [1, 2, 4], G 3 [1, 3, 4], G 3 [2, 3, 4], G 7 [0, 1, 2, 4], G 7 [0, 1, 3, 4], G 7 [0, 2, 3, 4], G 7 [1, 2, 3, 4], G 7 [0, 1, 2, 3, 4]], [G 4 [0], G 4 [1], G 4 [2], G 4 [3], G 4 [4], G 5 [0, 1], G 5 [0, 2], G 5 [0, 3], G 5 [0, 4], G 5 [1, 2], G 5 [1, 3], G 5 [1, 4], G 5 [2, 3], G 5 [2, 4], G 5 [3, 4], G 2 [0, 1, 2], G 2 [0, 1, 3], G 2 [0, 1, 4], G 2 [0, 2, 3], G 2 [0, 2, 4], G 2 [0, 3, 4], G 2 [1, 2, 3], G 2 [1, 2, 4], G 2 [1, 3, 4], G 2 [2, 3, 4], G 7 [0, 1, 2, 3], G 7 [0, 1, 2, 4], G 7 [0, 1, 3, 4], G 7 [0, 2, 3, 4], G 7 [1, 2, 3, 4]], [G 7 [0], G 7 [1], G 7 [2], G 7 [3], G 7 [4], G 3 [0, 1], G 3 [0, 2], G 3 [0, 3], G 3 [0, 4], G 3 [1, 2], G 3 [1, 3], G 3 [1, 4], G 3 [2, 3], G 3 [2, 4], G 3 [3, 4], G 3 [0, 1, 2], G 3 [0, 1, 3], G 3 [0, 1, 4], G 3 [0, 2, 3], G 3 [0, 2, 4], G 3 [0, 3, 4], G 3 [1, 2, 3], G 3 [1, 2, 4], G 3 [1, 3, 4], G 3 [2, 3, 4], G 7 [0, 1, 2, 3], G 7 [0, 1, 2, 4], G 7 [0, 1, 3, 4], G 7 [0, 2, 3, 4], G 7 [1, 2, 3, 4], G 7 [0, 1, 2, 3, 4]]]

--------------------------------------------------------------------------

-- | For QPL 2019 paper
-- | A heuristic to reduce pi/t-parity-phase circuits

-- | Data structure for phase Gadget. The first parameter encodes the
-- phase k * Pi/4, where k in Z_8 = Z/(8). The second parameter is the
-- qubit indexes (starts from 0. 0, 1, 2, ...) that the gadget acts
-- upon. For example, G 1 [0,1] is the following gadget:

-- 0 ---------O----------------
--             \
--              >-X-O(pi/4)
--             /
-- 1 ---------O---------------

--data Gadget = G Int [Int]

-- The HG (HGadget), XG, YG... are just H, X, Y diagrams in ZX
-- calculus. Think of the diagram of H, X, Y, Cnot, ...  as
-- "generalized gadget", we can construct ZXTerm from these
-- generalized gadgets, we call them ZXAtoms. InitG QState i is used
-- to record which wire is initialized which state. Similarly for
-- TermG.

-- | Qubit state {|0>, |1>, |+>, |->}. We can add finite many more
-- state in here.
data ZXAtom
  = InitG QState Int
  | TermG QState Int
  | SwapG Int Int
  | G Int [Int] -- the list must be an ordered list for improving efficiency.
  | HG Int -- we have hRewrite which decompses it into gadgets + Cliffords
  | CNZG [Int] -- we have cnzRewrite which decompses it into gadgets
  | XG Int
  | YG Int
  | ZG Int
  | CZG Int Int
  | SG Int
  | S'G Int
  | Tn Int Int
  | -- for polynomial calculation, 1st int the k in k * pi/4
    CTn Int Int Int
  | -- for polynomial calculation, 1st int is k * pi/4, 2nd is target,
    -- 3rd is control
    CCTn Int Int Int Int
  | -- for polynomial calculation, 1st int is k * pi/4, 2nd is target,
    -- 3rd and 4th are control
    CnotG Int Int
  deriving (Eq, Show, Ord, Read)

-- | hRewrite and cnzRewrite could have been done using gtrans, but
-- that is not convenient.

-- | We are using list to encode a zx-diagram. Here the diagram looks
-- like a quantum circuit, that is the reason we can use list to
-- encode them. Intuitively, the first element in the list is the left
-- most gate you see when you look at a circuit. And the index for
-- that gate (XG i) indicate which wire the gate is on counting from
-- top to bottom.
type ZXTerm = [ZXAtom]

-- | The translation goes this way. (1) Proprocess Toffoli and
-- multi-Toffoli into H and muli-control Z gate, translate Y gate into
-- X Z gates. (2) Translate all the gates to the corresponding gates
-- in ZXAtom with some exceptions. (3) Translate CNZG gate --- the
-- multi-control Z gate --- into gadgets and Clifford gates.

-- | Translate HZH to X (including the multi-controled HZH to multi-controled X)
z2x_step :: [Gate] -> Maybe [Gate]
z2x_step ((H i) : (CCZ a b c) : (H j) : t) =
  if i == j
    then case i of
      a -> Just $ Toffoli a b c : t
      b -> Just $ Toffoli b a c : t
      c -> Just $ Toffoli c a b : t
      _ -> do
        t' <- z2x_step t
        return ((H i) : (CCZ a b c) : (H j) : t')
    else do
      t' <- z2x_step t
      return ((H i) : (CCZ a b c) : (H j) : t')
z2x_step (h : t) = do
  t' <- z2x_step t
  return (h : t')
z2x_step [] = Nothing

z2x = repeatedly z2x_step

-- | Translate X to Z (including the multi-controled X to multi-controled Z)
x2z :: Gate -> Gate
x2z (X i) = Z i
x2z (Toffoli i j k) = CCZ i j k
x2z (Toffolin xs) = CNZ xs
x2z g = g

{-
x2z_ciri :: [Gate] -> [Gate] -> [Gate]
x2z_ciri (h@(X i) : t) ref = glok where
  gli = zip gl [0..]
  focusOnWires [i] ref
-}
-- c2zx function packed the above translation.

-- | preProcess decompose circuits into circuits using only the
-- following basic gates: {CNZ, Cnot, CZ, X, Z, H, S, S', T, T'}. That
-- is:

-- 1) preProcess decompose Toffolin into CNZ.
-- 2) CCZ -> CNZ
-- 3) Toffoli -> CNZ
-- 4) Y -> XZ (ignoring global phase).

preProcess :: [Gate] -> [Gate]
preProcess cir = cirok
  where
    auxfun (Toffolin xs@(h : t)) = case length xs of
      1 -> [X h]
      2 -> [Cnot h (head t)]
      _ -> [H h, CNZ (sort xs), H h]
    auxfun (CCZ i j k) = [CNZ (sort [i, j, k])]
    auxfun (Toffoli i j k) = [H i, CNZ (sort [i, j, k]), H i]
    auxfun (Y i) = [X i, Z i]
    auxfun x = [x]
    cirok = concatMap auxfun cir

-- a version of preprocess when the circuit only has tof and cnot. it
-- moves all the cont to the ends by commuting cot with tof. But
-- commuting tof and cnot, it will introduce new tof, so this function
-- also minimize the number of tofs intruduced by commuting.

id_tof_cnot :: (Gate, Gate) -> (Gate, Gate, Gate)
id_tof_cnot (Cnot i j, Toffoli a b c) = case i /= b && i /= c && j == a of
  True -> (Toffoli a b c, Toffoli i b c, Cnot i j)

preProcess_tof_cnot' :: [Gate] -> Maybe [Gate]
preProcess_tof_cnot' ((Cnot i j) : (Toffoli a b c) : t) = if i /= b && i /= c && j == a then (do
                                                            --    t' <-  (preProcess_tof_cnot' ((Cnot i j) : t))
                                                            return ((Toffoli a b c) : (Toffoli i b c) : (Cnot i j) : t)) else (do
                                                            --    t' <-  (preProcess_tof_cnot' ((Cnot i j) : t))
                                                            return ((Toffoli a b c) : (Cnot i j) : t))
preProcess_tof_cnot' (h : xs) = do
  xs' <- preProcess_tof_cnot' xs
  return (h : xs')
preProcess_tof_cnot' [] = Nothing

tofn2tof :: Gate -> Gate
tofn2tof h@(Toffolin (a : b : c : [])) = Toffoli a b c
tofn2tof h@(Toffolin (a : b : [])) = Cnot a b
tofn2tof x = x

tofns2tofs = map tofn2tof

tof2ccz (Toffoli a b c) = CCZ a b c
tof2ccz x = x

preProcess_tof_cnot = repeatedly preProcess_tof_cnot'

-- Specialied preprocess function for Galois field multiplier cirucuit.

-- commuting cnot and tof. two cases 1) cnot and tof commute. 2) need
-- to introdue addtional tof.

-- move a cnot to the left or right. l and r are tow sequence of tof
-- gates.
move_cnot :: Gate -> ([Gate], [Gate]) -> ([Gate], [Gate])
move_cnot (Cnot i j) (l, r) = (l', r')
  where
    target (Toffoli a b c) = a
    ltt = filter (\x -> target x == j) l
    rtt = filter (\x -> target x == j) r
    (l', r') = if length ltt <= length rtt then (l1, []) else ([], r1)
    new_tof (Toffoli a b c) = Toffoli i b c
    l1 = map new_tof ltt
    r1 = map new_tof rtt
move_cnot _ _ = error "wrong use of move_cnot. It must be used in Galois field multiplier circuit."

-- move a cnot to the left or right. l and r are tow sequence of tof
-- gates.
move_cnotg :: [Gate] -> ZXTerm
move_cnotg gl = zxt
  where
    xs = map tof2ccz_cnot2cnot gl
    l = takeWhile (not . isCnot) xs
    t = drop (length l) xs
    m = takeWhile isCnot t
    r = drop (length (l ++ m)) xs
    nl = length m `div` 2 + 1
    nr = length m - nl
    ml = take nl m
    mr = drop nl m
    lml = l ++ ml
    rmr' = reverse (mr ++ r)
    lmlg = bubleCliffordLeft $ c2zx2 lml
    rmrg = reverse $ bubleCliffordLeft $ c2zx2 rmr'
    zxt = lmlg ++ rmrg

isCnot (Cnot i j) = True
isCnot _ = False

move_cnots :: [Gate] -> [Gate]
move_cnots xs = xsok
  where
    l = takeWhile (not . isCnot) xs
    t = drop (length l) xs
    m = takeWhile isCnot t
    r = drop (length (l ++ m)) xs
    aux (h@(Cnot i j) : t) (l, r) = case move_cnot h (l, r) of
      (l', []) -> Data.Bifunctor.first ((++) l') (aux t (l, r))
      ([], r') -> ([], h : t)
    aux [] (l, r) = ([], [])
    ll = aux m (l, r)
    rr = aux (reverse $ snd ll) (reverse r, reverse l)
    xsok = if null (snd rr) then fst ll `union` fst rr ++ l ++ r else error "move_cnot has not been completed"

move_cnots' :: [Gate] -> LMR [Gate]
move_cnots' xs = xsok
  where
    l = takeWhile (not . isCnot) xs
    t = drop (length l) xs
    m = takeWhile isCnot t
    r = drop (length (l ++ m)) xs
    aux (h@(Cnot i j) : t) (l, r) = case move_cnot h (l, r) of
      (l', []) -> Data.Bifunctor.first ((++) l') (aux t (l, r))
      ([], r') -> ([], h : t)
    aux [] (l, r) = ([], [])
    ll = aux m (l, r)
    rr = aux (reverse $ snd ll) (reverse r, reverse l)
    xsok' = if null (snd rr) then fst ll `union` fst rr ++ l ++ r else error "move_cnot has not been completed"
    --ll' = ll1 ++ [Cnot i j ]
    xsok = LMR [] xsok' []

-- | Translation from Gate to ZXTerm. H gate is translated to HGadget,
-- later the HGadget between gadgets will be decomposed to into the
-- ZXTerm without H gate. The HGadgets in the two ends of the diagram
-- will stay. Other generalized gadget gates (Clifford gates) will be
-- moved to the left.
gtrans :: Gate -> ZXTerm
gtrans (I i) = []
gtrans (X i) = [XG i]
gtrans (Y i) = [XG i, ZG i]
gtrans (H i) = [HG i]
gtrans (Cnot i j) = [CnotG i j]
gtrans (CX i j) = [CnotG i j]
gtrans (Swap i j) = [SwapG i j]
gtrans (T i) = [G 1 [i]]
--gtrans (T' i) = [G 7 [i]]
--gtrans (CS i j) = []  -- Not considered for now
--gtrans (CS' i j) = [] -- Not considered for now
gtrans (CZ i j) = [CZG i j] --[G 6 [i, j], G 2 [i], G 2 [j]]
--gtrans (CCZ i j k) = [G 1 [i], G 1 [j], G 1 [k], G 7 [i, j], G 7 [i, k], G 7 [j, k],G 1 [i, j, k]]
gtrans (CCZ i j k) = [CNZG (sort [i, j, k])]
gtrans (CNZ inds) = [CNZG (sort inds)]
gtrans (S i) = [SG i]
--gtrans (S' i) = [S'G i]
gtrans (Z i) = [ZG i]
gtrans (Toffoli i j k) = [HG i] ++ gtrans (CCZ i j k) ++ [HG i]
-- Toffolin has been preprocssed
--gtrans (P is) = [] -- Not cosidered for now
--gtrans (M i) = [] -- Not cosidered for now
gtrans (Init s i) = [InitG s i]
gtrans (Term s i) = [TermG s i]

cir2zx :: [Gate] -> ZXTerm
cir2zx cir = concatMap gtrans cir

-- | Deocompe HG and CNZG into basic gate sets i.e. BGS = {gadgets and
-- Clifford and Init and Term}.

-- | translate HGs to BGS. g2h stands for gate to gadgets.
h2g :: Int -> ZXAtom -> ZXTerm
h2g m (HG i) = [InitG QMY (m + 1), SwapG i (m + 1), G 2 (sort [i, m + 1]), TermG QMY (m + 1)]
h2g m x = [x]

hRewrite' :: Int -> ZXTerm -> ZXTerm
hRewrite' m term@((HG i) : t) = h2g m (HG i) ++ hRewrite' (m + 1) t
hRewrite' m [] = []
hRewrite' m (h : t) = h : hRewrite' m t

hRewrite t = hRewrite' (maxIndexOfZXTerm t) t

maxIndexOfZXTerm :: ZXTerm -> Int
--maxIndexOfZXTerm [] = error "maximum over empty list"
maxIndexOfZXTerm zxterm = maximum $ wiresOfTerm zxterm

-- | CCZ to 7 gadgets
ccz_to_7gs :: ZXAtom -> ZXTerm
ccz_to_7gs (CNZG inds) = case inds of
  [h] -> [ZG h]
  [i, j] -> [G 6 (sort [i, j]), G 2 [i], G 2 [j]]
  [i, j, k] ->
    [ G 1 [i],
      G 1 [j],
      G 1 [k],
      G 7 (sort [i, j]),
      G 7 (sort [i, k]),
      G 7 (sort [j, k]),
      G 1 (sort [i, j, k])
    ]

-- | rewrite CNZ gate to BGS.
cnzRewrite' :: Int -> ZXTerm -> ZXTerm
cnzRewrite' n term@((CNZG inds) : t) = case inds of
  [h] -> ZG h : cnzRewrite' n t
  [i, j] -> [G 6 [i, j], G 2 [i], G 2 [j]] ++ cnzRewrite' n t
  --  [i,j] -> [CZG i j] ++ cnzRewrite' n t
  [i, j, k] ->
    [ G 1 [i],
      G 1 [j],
      G 1 [k],
      G 7 [i, j],
      G 7 [i, k],
      G 7 [j, k],
      G 1 [i, j, k]
    ]
      ++ cnzRewrite' n t
    where
      --  (i:j:r) -> [InitG QP (m), G 7 [m], G 1 (sort[j,m]), G 1 (sort[i,m]), G 7 (sort[i,j,m]), HG m] ++  cnzRewrite' m ((CNZG (sort(m:r))):t) ++ [TermG QMY m]

      m = n + 1
cnzRewrite' n [] = []
cnzRewrite' n (h : t) = h : cnzRewrite' n t

cnzRewrite t = cnzRewrite' (maxIndexOfZXTerm t) t

twire :: ZXAtom -> Int
twire (CnotG i j) = i

cwire :: ZXAtom -> Int
cwire (CnotG i j) = j

isCZG :: ZXAtom -> Bool
isCZG (CZG _ _) = True
isCZG _ = False

isCnotG :: ZXAtom -> Bool
isCnotG (CnotG _ _) = True
isCnotG _ = False

isCNZG :: ZXAtom -> Bool
isCNZG (CNZG _) = True
isCNZG _ = False

type MM = M.Map Int ZXAtom

--mmcnz :: MM -> MM

type ZXTermI = [(ZXAtom, Int)]

mcnzok t = l ++ m
  where
    (l, m, r) = mcnz (maxIndexOfZXTerm t) (t2ti t) t ([], [], [])

t2ti :: ZXTerm -> ZXTermI
t2ti xs = zip xs [0 .. (length xs)]

unjust :: Maybe a -> a
unjust (Just a) = a

focusOnwiresI :: [Int] -> ZXTermI -> ZXTermI
focusOnwiresI ws xs = zsok
  where
    zs' =
      filter
        ( \(x, y) ->
            not . null $ (wiresOfAtom x `intersect` ws)
        )
        xs
    (rzs, zs) = partition (\(x, y) -> isCZG x || (isCNZG x && length (wiresOfAtom x) <= 2)) zs'
    (rest, zsok) = partition (\(x, y) -> isCnotG x && not ((twire x) `elem` ws)) zs

mcnz :: Int -> ZXTermI -> ZXTerm -> (ZXTerm, ZXTerm, ZXTerm) -> (ZXTerm, ZXTerm, ZXTerm)
mcnz mm ((CNZG ws, k) : t) ys (lll, rrr, sss) = case length ws of
  1 -> mcnz mm t ys (lll ++ [ZG (head ws)], rrr, sss ++ [ZG (head ws)])
  2 -> mcnz mm t ys (lll ++ [CZG (head ws) (last ws)], rrr, sss ++ [CZG (head ws) (last ws)])
  -- new added clause for 7 gadges translation.
  3 -> mcnz mm t ys (lll ++ ccz_to_7gs (CNZG ws), rrr, sss ++ ccz_to_7gs (CNZG ws))
  _ -> mzx
  where
    asows =
      [ (cc, ws2)
        | ws2 <- choosen 2 ws,
          let cc = focusOnwiresI ws2 t,
          not . null $cc
      ]
    fit = filter (\(x, y) -> isCNZG (fst . head $ x) && y == y `intersect` wiresOfAtom (fst . head $ x)) asows
    fit2 =
      map
        ( \(x, y) ->
            (filter (\(a, b) -> (isCNZG a && y == y `intersect` wiresOfAtom a) || (not . isCNZG $ a)) x, y)
        )
        fit
    ordf = sortBy (compare `on` mylen) fit2
    mylen (x, y) = length $ takeWhile (isCNZG . fst) x
    best = last ordf
    ij = snd best
    bb = takeWhile (isCNZG . fst) (fst best)
    mzx = if null fit then mcnz (mm + 1) ((CNZG (sort ((mm + 1) : drop 2 ws)), k) : t) ys (lll ++ (mcnzh2 (mm + 1) (take 2 ws)), ([TermG QMY (mm + 1)] ++ rrr), sss) else mcnz (mm + 1) xsi ys' (lll ++ (mcnzh2 (mm + 1) ij), ([TermG QMY (mm + 1)] ++ rrr), sss)
    ys' = mcnzh (mm + 1) (head ij, last ij) (map snd bb) ys
    xsi = ssk ++ drop (k + 1) (t2ti ys')
    ssk = [(CNZG (sort ((mm + 1) : (ws \\ ij))), k)]
mcnz mm [] ys (lll, rrr, sss) = (lll, rrr, sss)
mcnz mm (h : t) ys (lll, rrr, sss) = mcnz mm t ys (lll ++ [fst h], rrr, sss ++ [fst h])

mcnzh2 :: Int -> [Int] -> ZXTerm
mcnzh2 n [i, j] = [InitG QP m, G 7 [m], G 1 (sort [j, m]), G 1 (sort [i, m]), G 7 (sort [i, j, m]), HG m]
  where
    m = n

mcnzh :: Int -> (Int, Int) -> [Int] -> ZXTerm -> ZXTerm
mcnzh _ _ [] ys = ys
mcnzh mm (w1, w2) (h : t) ys = zs''
  where
    oa = ys !! h
    na = CNZG (sort ((wiresOfAtom oa \\ [w1, w2]) ++ [mm]))
    zs = updateList ys h na
    zs' = mcnzh mm (w1, w2) t zs
    zs'' = zs'

updateList :: Eq a => [a] -> Int -> a -> [a]
updateList xs i a = ys
  where
    l = take i xs
    r = drop (i + 1) xs
    ys = l ++ [a] ++ r

-- | New Translation. To combine as many overlaped Control Z gate as
-- possible.

-- | Given a n-control Z Gate, find all n-control Z gates in the given
-- term sharing 2 controls without interception.
-- findCNZsharing2ControlsAs :: (Int,Int) -> ZXTerm -> [ZXAtom]
-- findCNZsharing2ControlsAs (i,j) xs =
c2zx' :: [Gate] -> ZXTerm
c2zx' t = foldr ((++) . gtrans) [] t

c2zx = mcnzok . c2zx' . hreduce . desugar_to_ZZHST . preProcess

c2zx2 = cnzRewrite . c2zx' . hreduce . desugar_to_ZZHST . preProcess

moveh x = (hred35 . bubCLR) (LMR [] (desugar35 x) [])

c2zx35 x = (cnzRewrite . c2zx' . mid) ((hred35 . bubCLR) (LMR [] (desugar35 x) []))

c2term35 x = LMR l m r
  where
    LMR a b c = zx2term $ c2zx35 x
    LMR d e f = (hred35 . bubCLR) (LMR [] (desugar35 x) [])
    l = c2zx d ++ a
    m = b
    r = c ++ c2zx f

mod5tc = [HG 6, InitG QP 7, InitG QP 8, InitG QP 9, InitG QP 10, InitG QP 11, InitG QP 12, InitG QP 13, InitG QP 14, InitG QP 15, InitG QP 16, InitG QP 17, InitG QP 18, InitG QP 19, InitG QP 20, InitG QP 21, InitG QP 22, InitG QP 23, InitG QP 24, InitG QP 25, InitG QP 26, InitG QP 27, InitG QMY 28, InitG QMY 29, InitG QMY 30, InitG QMY 31, InitG QMY 32, InitG QMY 33, InitG QMY 34, InitG QMY 35, InitG QMY 36, SwapG 7 28, SwapG 8 29, SwapG 11 30, SwapG 14 31, CZG 6 14, CZG 6 4, SwapG 15 32, CZG 6 15, CZG 6 3, SwapG 16 33, CZG 6 16, CZG 6 2, SwapG 17 34, SwapG 20 35, CZG 6 20, CZG 6 5, SwapG 23 36, CZG 6 23, CZG 6 1, G 2 [7, 28], G 2 [8, 29], G 2 [11, 30], G 2 [14, 31], G 2 [15, 32], G 2 [16, 33], G 2 [17, 34], G 2 [20, 35], G 2 [23, 36], G 0 [22], G 0 [25], G 0 [5, 22], G 0 [5, 25], G 0 [6, 22], G 0 [6, 25], G 0 [5, 6, 22], G 0 [5, 6, 25], G 0 [21], G 0 [27], G 0 [1, 21], G 0 [1, 27], G 0 [2, 21], G 0 [2, 27], G 0 [1, 2, 21], G 0 [1, 2, 27], G 2 [2], G 2 [5], G 2 [6], G 0 [9], G 6 [5, 6], G 0 [5, 9], G 0 [6, 9], G 0 [5, 6, 9], G 1 [1], G 7 [10], G 7 [12], G 7 [13], G 7 [18], G 7 [19], G 7 [24], G 7 [26], G 7 [28], G 7 [29], G 7 [30], G 7 [31], G 7 [32], G 7 [33], G 7 [34], G 7 [35], G 7 [36], G 7 [1, 2], G 1 [1, 24], G 1 [1, 26], G 1 [28, 1], G 1 [30, 1], G 1 [34, 1], G 1 [35, 1], G 7 [2, 5], G 7 [2, 6], G 7 [2, 9], G 1 [28, 2], G 1 [30, 2], G 1 [33, 2], G 1 [34, 2], G 1 [36, 2], G 1 [3, 18], G 1 [3, 24], G 1 [29, 3], G 1 [32, 3], G 1 [33, 3], G 1 [4, 12], G 1 [4, 26], G 1 [29, 4], G 1 [31, 4], G 1 [32, 4], G 1 [5, 12], G 1 [5, 18], G 1 [31, 5], G 1 [35, 5], G 1 [36, 5], G 1 [6, 13], G 1 [6, 19], G 1 [7, 10], G 1 [8, 10], G 1 [11, 13], G 1 [17, 19], G 7 [21, 27], G 7 [22, 25], G 7 [28, 1, 2], G 7 [30, 1, 2], G 7 [34, 1, 2], G 7 [1, 3, 24], G 7 [1, 4, 26], G 7 [35, 1, 5], G 1 [1, 21, 27], G 7 [33, 2, 3], G 1 [2, 5, 6], G 1 [2, 5, 9], G 7 [36, 2, 5], G 1 [2, 6, 9], G 1 [2, 21, 27], G 7 [29, 3, 4], G 7 [32, 3, 4], G 7 [3, 5, 18], G 7 [4, 5, 12], G 7 [31, 4, 5], G 1 [5, 22, 25], G 7 [6, 11, 13], G 7 [6, 17, 19], G 1 [6, 22, 25], G 7 [7, 8, 10], G 7 [1, 2, 21, 27], G 7 [2, 5, 6, 9], G 7 [5, 6, 22, 25], TermG QMY 28, TermG QMY 29, TermG QMY 30, TermG QMY 31, TermG QMY 32, TermG QMY 33, TermG QMY 34, TermG QMY 35, TermG QMY 36, HG 9, HG 10, CZG 9 10, HG 12, HG 13, CZG 12 13, HG 18, HG 19, CZG 18 19, HG 21, HG 22, CZG 21 22, HG 24, HG 25, CZG 24 25, HG 26, CZG 6 26, HG 27, CZG 6 27, HG 6, TermG QMY 27, TermG QMY 26, TermG QMY 25, TermG QMY 24, TermG QMY 23, TermG QMY 22, TermG QMY 21, TermG QMY 20, TermG QMY 19, TermG QMY 18, TermG QMY 17, TermG QMY 16, TermG QMY 15, TermG QMY 14, TermG QMY 13, TermG QMY 12, TermG QMY 11, TermG QMY 10, TermG QMY 9, TermG QMY 8, TermG QMY 7]

-- | First example in the paper, i.e. Mod5_4
mod54 = [CCZ 0 3 4, CCZ 2 3 4, CZ 3 4, CCZ 1 2 4, CZ 2 4, CCZ 0 1 4, CZ 1 4, CZ 0 4]

-- | Second example, i.e. tof3
tof2 = [Toffolin [2, 0, 1]]

tof3 = [Toffolin [3, 0, 1, 2]]

tof4 = [Toffolin [4, 0, 1, 2, 3]]

tof5 = [Toffolin [5, 0, 1, 2, 3, 4]]

tof6 = [Toffolin [5, 0, 1, 2, 3, 4, 6]]

tof7 = [Toffolin [5, 0, 1, 2, 3, 4, 6, 7]]

tof8 = [Toffolin [5, 0, 1, 2, 3, 4, 6, 7, 8]]

tof9 = [Toffolin [5, 0, 1, 2, 3, 4, 6, 7, 8, 9]]

-- | other example
rd32 = [Toffoli 4 1 2, Cnot 2 1, Toffoli 4 2 3, Cnot 3 2]

--qft_4 = [H 1,Toffolin [2,1],T' 2,Toffolin [2,1],T 1,T 2,H 5,Toffolin [1,3],S' 3,Toffolin [5,1],Toffolin [3,5],T 3,T' 5,Toffolin [3,1],Toffolin [5,1],T 3,T' 5,Toffolin [5,3],H 3,T 3,H 3,Toffolin [5,3],T' 3,T 5,Toffolin [5,1],Toffolin [3,1],T 5,T' 3,Toffolin [3,5],Toffolin [5,1],S 3,Toffolin [1,3],H 5,Toffolin [5,1],H 1,CCZ 4 5 1,H 1,Toffolin [5,1],T 5,S 5,H 5,T 5,S 5,H 5,T 5,H 5,T 5,S 5,H 5,T 5,S 5,H 5,T 5,S 5,H 5,T 5,H 5,T 5,S 5,H 5,T 5,S 5,H 5,T 5,S 5,H 5,T 5,H 5,T 5,S 5,H 5,T 5,S 5,H 5,T 5,H 5,T 5,S 5,H 5,T 5,H 5,T 5,H 5,T 5,H 5,T 5,H 5,T 5,S 5,H 5,T 5,H 5,T 5,S 5,H 5,T 5,S 5,H 5,T 5,S 5,H 5,T 5,S 5,H 5,T 5,S' 5,H 5,T 5,H 5,T 5,S 5,Toffolin [5,1],H 1,CCZ 4 5 1,H 1,Toffolin [5,1],H 2,Toffolin [3,2],T' 3,Toffolin [3,2],T 2,T 3,H 5,Toffolin [2,4],S' 4,Toffolin [5,2],Toffolin [4,5],T 4,T' 5,Toffolin [4,2],Toffolin [5,2],T 4,T' 5,Toffolin [5,4],H 4,T 4,H 4,Toffolin [5,4],T' 4,T 5,Toffolin [5,2],Toffolin [4,2],T 5,T' 4,Toffolin [4,5],Toffolin [5,2],S 4,Toffolin [2,4],H 5,H 3,Toffolin [4,3],T' 4,Toffolin [4,3],T 3,T 4]

-- | after translation, we need to do some rewriting on ZXTerm.
-- | First, we consider the case where the ZXTerm is a list of
-- homogneous gadgets, i.e. no "generalzed gadgets". In this case,
-- they are all commute with each other, so we can freely reorder
-- them. Actually, we will sort them in some order easier for us to do
-- rewrite. We don't have a normal form here, but the idea is to fuse
-- as many gadgets as possible, so the sorting should act grouping
-- fusable gadgets. Fusable gadgets are the ones with the same
-- targets. So the ordering used to sort is first comparing the number
-- of targets, then comparing the indexes.

-- | another way to fuse is also considered for efficency reasons,
-- will see it later.

-- | Since we need constantly divides a circuits into three part, we
-- define the following data structure that is the term we will do
-- reduction on. LMR stands for left mid right. Normally we want that
-- in LMR l m r, l is the left end of the circuits consisting only of
-- Init and Clifford gates, r is the right end consisting only of Term
-- and Clifford gates. We have several function for doing so. moveIT
-- moves Init gate to l, and Term gate to r. moveClifford moves all
-- movable Cliffords to l or r.
data LMR a = LMR a a a deriving (Eq, Ord)

type Term = LMR ZXTerm

instance Show Term where
  show = showterm

instance Show (LMR [Gate]) where
  show = showcir

showterm :: Term -> String
showterm (LMR l m r) = show l ++ "\n" ++ show m ++ "\n" ++ show r

showcir :: LMR [Gate] -> String
showcir (LMR l m r) = show l ++ "\n" ++ show m ++ "\n" ++ show r

-- | class for Thing that can be show in pdf.
class PDFable a where
  topdf :: a -> IO ()
  topdf_file :: a -> IO ()

instance PDFable [Gate] where
  topdf cir = do
    print_generic
      Preview
      ( \x -> do
          foldM gl2cir x (reindexCir (+ 1) cir)
      )
      (replicate (1 + maximum (wiresOfCir cir)) qubit)

  topdf_file cir = do
    print_generic
      PDF
      ( \x -> do
          foldM gl2cir x (reindexCir (+ 1) cir)
      )
      (replicate (1 + maximum (wiresOfCir cir)) qubit)

instance PDFable [[Gate]] where
  topdf cir = topdf cir'
    where
      cir' = intercalate [M 0] cir

instance PDFable ZXTerm where
  topdf cir = do
    print_generic
      Preview
      ( \x -> do
          foldM zx2cir x cir
      )
      (replicate (1 + maximum (wiresOfTerm cir) - initqw) qubit)
    where
      initqw = length $ wiresOfTerm $ filter isInit cir
  topdf_file cir = do
    print_generic
      PDF
      ( \x -> do
          foldM zx2cir x cir
      )
      (replicate (1 + maximum (wiresOfTerm cir) - initqw) qubit)
    where
      initqw = length $ wiresOfTerm $ filter isInit cir

instance PDFable Term where
  topdf term@(LMR l m r) = do
    let xs = replicate (1 + (maximum $ wiresOfTerm cir) - initqw) qubit
    print_generic
      Preview
      ( \x -> do
          x' <- foldM zx2cir x l
          --                        comment "Clifford on the left"
          comment ("t-count: " ++ show (tCount term))
          --                        comment "t-count per wire"
          label x' (map show (aveTGs term))
          foldM_ zx2cir x' m
          comment "Clifford on the right"
          foldM zx2cir x' r
      )
      xs
    where
      (ll, lr) = partition isInit l
      l' = lr ++ ll
      cir = l ++ m ++ r
      initqw = length $ wiresOfTerm $ filter isInit cir
  topdf_file term@(LMR l m r) = do
    let xs = replicate (1 + (maximum $ wiresOfTerm cir) - initqw) qubit
    print_generic
      PDF
      ( \x -> do
          x' <- foldM zx2cir x l
          --                        comment "Clifford on the left"
          comment ("t-count: " ++ show (tCount term))
          --                        comment "t-count per wire"
          label x' (map show (aveTGs term))
          foldM_ zx2cir x' m
          comment "Clifford on the right"
          foldM zx2cir x' r
      )
      xs
    where
      (ll, lr) = partition isInit l
      l' = lr ++ ll
      cir = l ++ m ++ r
      initqw = length $ wiresOfTerm $ filter isInit cir

instance PDFable (LMR [Gate]) where
  topdf term@(LMR l' m' r') = do
    let xs = replicate ((maximum $ wiresOfCir cir)) qubit
    print_generic
      Preview
      ( \x -> do
          foldM_ gl2cir x l
          comment "Clifford on the left"
          label x (repeat "|")
          foldM_ gl2cir x m
          label x (repeat "|")
          comment "Clifford on the right"
          foldM gl2cir x r
      )
      xs
    where
      l = reindexCir (+ 1) l'
      m = reindexCir (+ 1) m'
      r = reindexCir (+ 1) r'
      cir = l ++ m ++ r

topdf' term@(LMR l m r) = do
  let xs = replicate (1 + (maximum $ wiresOfTerm cir) - initqw) qubit
  print_generic
    PDF
    ( \x -> do
        foldM_ zx2cir x l
        comment (show (tCount term))
        label x (map show (aveTGs term))
        foldM_ zx2cir x m
        foldM zx2cir x r
    )
    xs
  where
    cir = l ++ m ++ r
    initqw = length $ wiresOfTerm $ filter isInit cir

unJust :: Maybe a -> a
unJust (Just x) = x

focusOnwiresCir :: [Int] -> [Gate] -> [Gate]
focusOnwiresCir wires = filter (\x -> not . null $ wiresOfGate x `intersect` wires)

focusOnwiresCirExactly :: [Int] -> [Gate] -> [Gate]
focusOnwiresCirExactly wires = filter
    ( \x ->
        (Set.fromList $ wiresOfGate x) `Set.isSubsetOf` (Set.fromList wires)
    )

maxIndexOfCir :: [Gate] -> Int
maxIndexOfCir cir = maximum $ wiresOfCir cir

focusOnwires :: [Int] -> ZXTerm -> ZXTerm
focusOnwires wires = filter (\x -> not . null $ wiresOfAtom x `intersect` wires)

focusOnwiresExactly :: [Int] -> ZXTerm -> ZXTerm
focusOnwiresExactly wires = filter
    ( \x ->
        (Set.fromList $ wiresOfAtom x) `Set.isSubsetOf` (wires')
    )
  where
    wires' = Set.fromList wires

-- | We use Wire and Int interchangably.
type Wire = Int

wiresOfAtom :: ZXAtom -> [Wire]
wiresOfAtom (InitG s i) = [i]
wiresOfAtom (TermG s i) = [i]
wiresOfAtom (SwapG i j) = [i, j]
wiresOfAtom (Tn i j) = [j]
wiresOfAtom (CTn i j k) = [j, k]
wiresOfAtom (CCTn i j k l) = [j, k, l]
wiresOfAtom (G p inds) = inds
wiresOfAtom (CNZG inds) = inds
wiresOfAtom (HG i) = [i]
wiresOfAtom (XG i) = [i]
wiresOfAtom (YG i) = [i]
wiresOfAtom (ZG i) = [i]
wiresOfAtom (CZG i j) = [i, j]
wiresOfAtom (SG i) = [i]
wiresOfAtom (S'G i) = [i]
wiresOfAtom (CnotG i j) = [i, j]

wiresOfTerm :: ZXTerm -> [Wire]
wiresOfTerm term = foldl' union [] (map wiresOfAtom term)

gorder :: ZXAtom -> ZXAtom -> Ordering
gorder (G k is) (G l is') = if (length is) == (length is') then compare (sort is) (sort is') else compare (length is) (length is')

gorder1 :: ZXAtom -> ZXAtom -> Ordering
gorder1 (G k is) (G l is') =
  if is == is' then compare k l else compare is is'

-- | fuse two reduced homogeneous gadget lists. xs ys must be reduced.
fuse' :: ZXTerm -> ZXTerm
fuse' [] = []
fuse' [a] = [a]
fuse' (a : t) = last t' : fuse' (init t')
  where
    t' = fuse'1 a t

fuse'1 :: ZXAtom -> ZXTerm -> ZXTerm
fuse'1 (G p xs) [] = [G p xs]
fuse'1 a@(G p xs) (h@(G q ys) : t) = case xs == ys of
  --(sort xs) == (sort ys), xs ys will always be sorted list
  True -> fuse'1 (G (p + q `mod` 8) xs) t
  False -> h : fuse'1 a t

-- | fast way to do fusion O(logt)

-- | Fuse adjacent gadets with the same target indexes. The rest stays
-- the same.
fuse2 :: ZXTerm -> Maybe ZXTerm
fuse2 [] = Nothing
fuse2 [a] = Nothing
fuse2 (a@(G k is) : b@(G l is') : t) =
  if (sort is) == (sort is') then return ((G ((k + l) `mod` 8) is) : t) else (do
    r1 <- fuse2 $ b : t
    return $ a : r1)
fuse2 (h : t) = do
  t' <- fuse2 t
  return (h : t')

-- | first sort, then repeatedly fuse 2 adjacent Gadgets. Only applies
-- to homogeneous Gadget diagram i.e. all the components are gadgets.
fuseHomoGs :: ZXTerm -> ZXTerm
fuseHomoGs t = repeatedly fuse2 (sortBy gorder t)

sortGs = sortBy gorder

-- | determine whether the ZXAtom is a Gadget or not.
isGadget :: ZXAtom -> Bool
isGadget (G _ _) = True
isGadget _ = False

-- | determine whether the ZXAtom is a Init or not.
isInit :: ZXAtom -> Bool
isInit (InitG s i) = True
isInit _ = False

-- | determine whether the ZXAtom is a Term or not.
isTerm :: ZXAtom -> Bool
isTerm (TermG s i) = True
isTerm _ = False

-- | determine whether the ZXAtom is a HG or not.
isHG :: ZXAtom -> Bool
isHG (HG i) = True
isHG _ = False

-- | determine whether the ZXAtom is a nonIT or not.
isClifford :: ZXAtom -> Bool
isClifford (SwapG _ _) = True
isClifford (HG _) = True
isClifford (XG _) = True
isClifford (YG _) = True
isClifford (ZG _) = True
isClifford (CZG _ _) = True
isClifford (SG _) = True
isClifford (S'G _) = True
isClifford (CnotG _ _) = True
isClifford _ = False

-- | substute short for long if possible, otherwise identity. only
-- consider the gadget with targets in the given index list. And the
-- term must be homogeneous and distinct (i.e. after being fused)
-- gadgets term.

-- | the first argument should be an identity. Both argumens should be
-- homogeneous lists of gadgets. This one works on terms.
rewriteM :: [ZXTerm] -> Term -> Term
rewriteM eqns term = foldl' (flip rewriteWithIdentity) term eqns

rewriteMM :: [ZXTerm] -> Term -> (Term, [LogEntry])
rewriteMM eqns term = runWriter $ foldM (flip rewriteWithIdentityM) term eqns

heuri5M = heuri'M

heuri'M (LMR l m r) = rewriteMM (i45effun $wiresOfTerm m) (LMR l m r)

heuriM (LMR l m r) = rewriteMM (id4s $wiresOfTerm m) (LMR l m r)

heuri5 (LMR l m r) = rewriteM (i45effun $wiresOfTerm m) (LMR l m r)

heuri5_fast (LMR l m r) = rewriteM (i45effun_fast $wiresOfTerm m) (LMR l m r)

heuri5_inc (LMR l m r) = rewriteM (i45effun_inc $wiresOfTerm m) (LMR l m r)

heuri5_dec (LMR l m r) = rewriteM (i45effun_dec $wiresOfTerm m) (LMR l m r)

heuri6 (LMR l m r) = rewriteM (i6effun $wiresOfTerm m) (LMR l m r)

heurif idfun (LMR l m r) = rewriteM (idfun $wiresOfTerm m) (LMR l m r)

rewriteWithIdentity :: ZXTerm -> Term -> Term
rewriteWithIdentity eo term@(LMR lo mo ro) =
  if t_count m2' >= 8
    then if t_count commonGadgets > ((t_count e) `div` 2) then fused else term
    else term
  where
    e = sortGinds eo
    (l2, m2, r2) = (sortGinds lo, sortGinds mo, sortGinds ro)
    m2' = focusOnwiresExactly (wiresOfTerm e) m2
    m2'' = filter isoddG m2'
    e' = filter isoddG e
    commonGadgets = e' `intersectModuloParity` m2''
    m1 = fuseHomoGs (e ++ m2)
    (om, em) = partition isoddG m1
    fused = LMR (l2 ++ em) om r2

type LogEntry = ZXTerm

rewriteWithIdentityM :: ZXTerm -> Term -> Writer [LogEntry] Term
rewriteWithIdentityM eo term@(LMR lo mo ro) =
  if t_count m2' >= 8
    then if t_count commonGadgets > ((t_count e) `div` 2) then (do
           tell [e]
           return fused) else (do
           return term)
    else do
      return term
  where
    e = sortGinds eo
    (l2, m2, r2) = (sortGinds lo, sortGinds mo, sortGinds ro)
    m2' = focusOnwiresExactly (wiresOfTerm e) m2
    m2'' = filter isoddG m2'
    e' = filter isoddG e
    commonGadgets = e' `intersectModuloParity` m2''
    m1 = fuseHomoGs (e ++ m2)
    (om, em) = partition isoddG m1
    fused = LMR (l2 ++ em) om r2

intersectModuloParity :: ZXTerm -> ZXTerm -> ZXTerm
intersectModuloParity xs ys = zs
  where
    xs' = moduloParityOnTerm xs
    ys' = moduloParityOnTerm ys
    zs = xs' `intersect` ys'

-- | Identity is represented by a function on wires, which generates
-- the corresponding identity when given wires. When using this type,
-- programmer need to make sure that the term is indeed an identity by
-- himself. The int indicate how may wires this identity needs.
type MyId = (Int, [Wire] -> ZXTerm)

rewriteUsingMyId :: MyId -> Term -> Term
rewriteUsingMyId myid@(numOfwire, fun) term@(LMR l m r) = termReduced
  where
    wiresNeedConsidered = wiresOfTerm m
    choosenWiress = chooseNWireFrom numOfwire wiresNeedConsidered
    myIdsOnChoosenWiress = map fun choosenWiress
    termReduced' = foldl' (flip rewriteWithIdentity) term myIdsOnChoosenWiress
    termReduced = moveEvenGToLeft termReduced'

--- | Identities we will consider.

myid4 :: MyId
myid4 = (4, idFunOn4Wires)

-- | when calling this function, make sure there are 4 distinct indexes in the
-- first argument.
idFunOn4Wires :: [Int] -> ZXTerm
idFunOn4Wires inds = [G 1 [x] | x <- inds] ++ [G 7 (sort xs) | xs <- choosen 2 inds] ++ [G 1 (sort xs) | xs <- choosen 3 inds] ++ [G 7 (sort inds)]

id4 :: [Int] -> ZXTerm
id4 = idFunOn4Wires

myid5 :: MyId
myid5 = (5, idFunOn5Wires)

-- | when calling this function, make sure there are 5 distinct
-- indexes in the first argument.
idFunOn5Wires :: [Int] -> ZXTerm
idFunOn5Wires inds = [G 3 [x] | x <- inds] ++ [G 6 (sort xs) | xs <- choosen 2 inds] ++ [G 1 (sort xs) | xs <- choosen 3 inds] ++ [G 7 (sort inds)]

id5 :: [Int] -> ZXTerm
id5 = idFunOn5Wires

-- | when calling this function, make sure there are 5 distinct
-- indexes in the first argument.
idFunOn6Wires :: [Int] -> ZXTerm
idFunOn6Wires inds = [G 6 [x] | x <- inds] ++ [G 5 (sort xs) | xs <- choosen 2 inds] ++ [G 1 (sort xs) | xs <- choosen 3 inds] ++ [G 7 (sort inds)]

id6 :: [Int] -> ZXTerm
id6 = idFunOn6Wires

myid6 :: MyId
myid6 = (6, idFunOn6Wires)

myid7 :: MyId
myid7 = (7, idFunOn7Wires)

-- | used for decrease number of wires of a gadget.
fun_dec (G x ws) = case length ws of
  4 -> concat $ replicate (x `mod` 8) (drop 1 $ reverse (id4 ws))
  5 -> concat $ replicate (x `mod` 8) (drop 1 $ reverse (id5 ws))
  6 -> concat $ replicate (x `mod` 8) (drop 1 $ reverse (id6 ws))
  7 -> concat $ replicate (x `mod` 8) (drop 1 $ reverse (id7 ws))
  _ -> [G x ws]

-- | when calling this function, make sure there are 5 distinct
-- indexes in the first argument.
idFunOn7Wires :: [Int] -> ZXTerm
idFunOn7Wires inds = [G 2 [x] | x <- inds] ++ [G 4 (sort xs) | xs <- choosen 2 inds] ++ [G 1 (sort xs) | xs <- choosen 3 inds] ++ [G 7 (sort inds)]

id7 :: [Int] -> ZXTerm
id7 = idFunOn7Wires

id4s :: [Int] -> [ZXTerm]
id4s wires = [id4 fourwires | fourwires <- choosen 4 wires]

id5s :: [Int] -> [ZXTerm]
id5s wires = [] : [id5 fivewires | fivewires <- choosen 5 wires]

id6s :: [Int] -> [ZXTerm]
id6s wires = [] : [id6 sixwires | sixwires <- choosen 6 wires]

id7s :: [Int] -> [ZXTerm]
id7s wires = [] : [id7 sevenwires | sevenwires <- choosen 7 wires]

-- | when calling this function, make sure there are 5 distinct
-- indexes in the first argument.
id45s :: [Int] -> [ZXTerm]
id45s w =
  [(fuseHomoGs) (e4 ++ e5) |
   ff <- choosen 5 w, e4 <- id4s ff, e5 <- id5s ff]

id45s234 w = id45s2 w ++ id45s3 w ++ id45s4 w

id45s5 :: [Int] -> [ZXTerm]
id45s5 w =
  [(fuseHomoGs) (e1 ++ e2 ++ e3 ++ e4 ++ e5 ++ e6) |
   ff <- choosen 5 w,
   let c4 = id4s ff,
   let e1 = c4 !! 1,
   let e2 = c4 !! 2,
   let e3 = c4 !! 3,
   let e4 = c4 !! 4,
   let e5 = c4 !! 5,
   let c5 = id5s ff,
   let e6 = c5 !! 1]

id45s3 :: [Int] -> [ZXTerm]
id45s3 w =
  [(fuseHomoGs) (e3 ++ e4 ++ e5 ++ e6) |
   ff <- choosen 5 w,
   let c4 = id4s ff,
   let e1 = c4 !! 1,
   let e2 = c4 !! 2,
   let e3 = c4 !! 3,
   let e4 = c4 !! 4,
   let e5 = c4 !! 5,
   let c5 = id5s ff,
   let e6 = c5 !! 1]

id45s4 :: [Int] -> [ZXTerm]
id45s4 w =
  [(fuseHomoGs) (e2 ++ e3 ++ e4 ++ e5 ++ e6) |
   ff <- choosen 5 w,
   let c4 = id4s ff,
   let e1 = c4 !! 1,
   let e2 = c4 !! 2,
   let e3 = c4 !! 3,
   let e4 = c4 !! 4,
   let e5 = c4 !! 5,
   let c5 = id5s ff,
   let e6 = c5 !! 1]

id45s2 :: [Int] -> [ZXTerm]
id45s2 w =
  [(fuseHomoGs) (e4 ++ e5 ++ e6) |
   ff <- choosen 5 w,
   let c4 = id4s ff,
   let e1 = c4 !! 1,
   let e2 = c4 !! 2,
   let e3 = c4 !! 3,
   let e4 = c4 !! 4,
   let e5 = c4 !! 5,
   let c5 = id5s ff,
   let e6 = c5 !! 1]

id45s1n :: [Int] -> [ZXTerm]
id45s1n w =
  nub $
    [(fuseHomoGs) (e1 ++ e2) |
   ff <- choosen 5 w,
   ff4 <- choosen 4 ff,
   let ffc = id4s ff4,
   e1 <- ffc,
   e2 <- id5s ff]

id45s2n :: [Int] -> [ZXTerm]
id45s2n w =
  [(fuseHomoGs) (e1 ++ e2 ++ e3) |
   ff <- choosen 5 w,
   let ffc = id4s ff,
   e1 <- ffc,
   e2 <- id5s ff,
   e3 <- ffc,
   e1 /= e3]

id45s3n :: [Int] -> [ZXTerm]
id45s3n w =
  nub $
    [(fuseHomoGs) (e1 ++ e2 ++ e3 ++ e4) |
   ff <- choosen 5 w,
   let ffc = id4s ff,
   e1 <- ffc,
   e2 <- id5s ff,
   e3 <- ffc,
   e4 <- ffc]

id45s4n :: [Int] -> [ZXTerm]
id45s4n w =
  nub $
    [(fuseHomoGs) (e1 ++ e2 ++ e3 ++ e4 ++ e5) |
   ff <- choosen 5 w,
   ff4 <- choosen 4 ff,
   let ffc = id4s ff4,
   e1 <- ffc,
   e2 <- id5s ff,
   e3 <- ffc,
   e4 <- ffc,
   e5 <- ffc]

id45s5n :: [Int] -> [ZXTerm]
id45s5n w =
  nub $
    [(fuseHomoGs) (e1 ++ e2 ++ e3 ++ e4 ++ e5 ++ e6) |
   ff <- choosen 5 w,
   let ffc = id4s ff,
   e1 <- ffc,
   e2 <- id5s ff,
   e3 <- ffc,
   e4 <- ffc,
   e5 <- ffc,
   e6 <- ffc,
   let em = filter (\ x -> x == []) [e1, e2, e3, e4, e5, e6],
   let eml = length em,
   eml == 0 && (length . nub) [e1, e2, e3, e4, e5, e6] == 6
     || (length . nub) [e1, e2, e3, e4, e5, e6] == 6 - eml + 1]

i45effun :: [Int] -> [ZXTerm]
i45effun xs = [map (\(G k ws) -> G k (map efff ws)) i45effi | i45effi <- i45eff, xs4 <- choosen 5 xs, let efff = p2f $ zip [0 .. 4] xs4]

i45effun_fast :: [Int] -> [ZXTerm]
i45effun_fast xs = [map (\(G k ws) -> G k (map efff ws)) i45effi | i45effi <- i45eff, xs4 <- choosen_fast 5 xs, let efff = p2f $ zip [0 .. 4] xs4]

i45effun_inc :: [Int] -> [ZXTerm]
i45effun_inc xs = [map (\(G k ws) -> G k (map efff ws)) i45effi | i45effi <- i45eff_inc, xs4 <- choosen 5 xs, let efff = p2f $ zip [0 .. 4] xs4]

i45effun_dec :: [Int] -> [ZXTerm]
i45effun_dec xs = [map (\(G k ws) -> G k (map efff ws)) i45effi | i45effi <- i45eff_dec, xs4 <- choosen 5 xs, let efff = p2f $ zip [0 .. 4] xs4]

p2f :: [(Int, Int)] -> (Int -> Int)
p2f [] = id
p2f xs = \x -> Data.Maybe.fromMaybe x (lookup x xs)

i6effun :: [Int] -> [ZXTerm]
i6effun ws = [e5s ++ e6 | ws6 <- choosen 6 ws, e6 <- id6s ws6, e5s <- i45effun ws6]

id45s' :: [Int] -> [ZXTerm]
id45s' w =
  [(fuseHomoGs) (e1 ++ e2 ++ e3 ++ e4 ++ e5 ++ e6) |
   ff <- choosen 5 w,
   e1 <- id4s ff,
   e2 <- id4s ff,
   e3 <- id4s ff,
   e4 <- id4s ff,
   e5 <- id4s ff,
   e6 <- id5s ff]

id56s' :: [Int] -> [ZXTerm]
id56s' w =
  [(fuseHomoGs) (e4 ++ e5) |
   ff <- choosen 6 w, e4 <- id5s ff, e5 <- id6s ff]

-- | call with 7 number
id67s :: [Int] -> [ZXTerm]
id67s w =
  [(fuseHomoGs) (e1 ++ e2 ++ e3 ++ e4 ++ e5 ++ e6 ++ e7 ++ e7') |
   ff <- choosen 7 w,
   let f6 = id6s ff,
   let f7 = id7s ff,
   e1 <- [f6 !! 1, []],
   e2 <- [f6 !! 2, []],
   e3 <- [f6 !! 3, []],
   e4 <- [f6 !! 4, []],
   e5 <- [f6 !! 5, []],
   e6 <- [f6 !! 6, []],
   e7 <- [f6 !! 7, []],
   e7' <- [f7 !! 1, []]]

-- | call with 6 number
id56s :: [Int] -> [ZXTerm]
id56s w =
  [(fuseHomoGs) (e1 ++ e2 ++ e3 ++ e4 ++ e5 ++ e6 ++ e7) |
   ff <- choosen 6 w,
   let f5 = id5s ff,
   let f6 = id6s ff,
   e1 <- [f5 !! 1, []],
   e2 <- [f5 !! 2, []],
   e3 <- [f5 !! 3, []],
   e4 <- [f5 !! 4, []],
   e5 <- [f5 !! 5, []],
   e6 <- [f5 !! 6, []],
   e7 <- [f6 !! 1, []]]

-- | when calling this function, make sure there are 5 distinct
-- indexes in the first argument.
-- id456s :: [Int] -> [ZXTerm]
-- id45s inds = e456s
--   where
--     ind6s = choosen 6 inds
--     e4s = []:[id4 inds4 ++ id5 ind5 | ind5 <- ind5s, inds4 <- choosen 4 ind5]
id4567s :: [Int] -> [ZXTerm]
id4567s w =
  [ e4 ++ e5 ++ e6 ++ e7
    | e4 <- id4s w,
      e5 <- id5s w,
      e6 <- id6s w,
      e7 <- id7s w
  ]

moduloParityOnAtom :: ZXAtom -> ZXAtom
moduloParityOnAtom (G k inds) = G (k `mod` 2) inds
moduloParityOnAtom x = x

moduloParityOnTerm = map moduloParityOnAtom

sortGind :: ZXAtom -> ZXAtom
sortGind (G p inds) = G p (sort inds)
sortGind x = x

sortGinds :: ZXTerm -> ZXTerm
sortGinds t = map sortGind t

invertG :: ZXTerm -> ZXTerm
invertG t = map invG t

invG :: ZXAtom -> ZXAtom
invG (G p ts) = G ((- p) `mod` 8) ts
invG x = x

-- | first we focus on focus4. from all the indexes (say n in total),
-- we choose 4 wrie from these n indexes. So there are "n choose 4"
-- possible ways which we consider all.
chooseNWireFrom :: Int -> [Int] -> [[Int]]
chooseNWireFrom = choosen

choosen :: Int -> [Int] -> [[Int]]
choosen n [] = []
choosen 1 t = [[x] | x <- t]
choosen n t@(h : r) = case compare (length t) n of
  EQ -> [t]
  LT -> []
  GT -> havinghead ++ nothavinghead
    where
      havinghead = map (h :) (choosen (n -1) r)
      nothavinghead = choosen n r

choosen_fast :: Int -> [Int] -> [[Int]]
choosen_fast n xs = if length xs >= n then take n xs : choosen_fast n (drop n xs) else []

-- | conveinent functions. move all gadgets in the middle, other gates
-- to the beginning and end.
isoddG :: ZXAtom -> Bool
isoddG (G k is) = k `mod` 2 == 1
isoddG _ = False

t_count :: ZXTerm -> Int
t_count t = length $ filter isoddG t

tCount :: Term -> Int
tCount (LMR l m r) = t_count m

moveITToEnds :: Term -> Term
moveITToEnds term@(LMR l m r) = LMR (l ++ lend) mid (rend ++ r)
  where
    lend = filter isInit m
    rend = filter isTerm m
    mid = filter (\x -> (not . isInit) x && (not . isTerm) x) m

moveHGToEnds :: Term -> Term
moveHGToEnds term@(LMR l m r) = LMR l' m' r'
  where
    ml@(LMR l1 m1 r1) = foldl' (flip moveHGtoLeftIndvidually) term (wiresOfTerm m)
    mr@(LMR l' m' r') = foldl' (flip moveHGtoRightIndvidually) ml (wiresOfTerm m1)

moveHGtoLeftIndvidually :: Int -> Term -> Term
moveHGtoLeftIndvidually i term@(LMR l m r) =
  case focusOnwires [i] m of
    (b@(HG k) : ts) -> LMR (l ++ [b]) (delete b m) r
    _ -> term

moveHGtoRightIndvidually :: Int -> Term -> Term
moveHGtoRightIndvidually i term@(LMR l m r) =
  case focusOnwires [i] (reverse m) of
    (b@(HG k) : ts) -> LMR l (deleteBackward b m) (b : r)
    _ -> term

-- | delete x removes the last occurrence of x from its list argument.
deleteBackward x xs = reverse $ delete x (reverse xs)

moveCliffordToEnds :: Term -> Term
moveCliffordToEnds (LMR l m r) = LMR l' m' r'
  where
    l1 = takeWhile isClifford m
    mm = drop (length l1) m
    r1 = reverse $ takeWhile isClifford (reverse mm)
    m1 = dropWhile isClifford m
    m2 = dropWhile isClifford (reverse m1)
    m' = reverse m2
    l' = l ++ l1
    r' = r1 ++ r

moveEvenGToLeft :: Term -> Term
moveEvenGToLeft (LMR l m r) = LMR l' m' r
  where
    l' = l ++ filter (not . isoddG) m
    m' = filter isoddG m

-- | bubChoice a b t = m. if a commutes with b, and a:t reduces to t', then m = b:t', otherwise a : (bubCL1 (b:t))
bubChoice :: Gate -> Gate -> [Gate] -> Maybe [Gate]
--bubChoice a b@(CCZ i j k) [] = Just $ [b,a]
--bubChoice a b [] = Nothing
--bubChoice a b (c:[]) = Just $ [b,a]
bubChoice a b t =
  let c1 = bubCL1 (a : t)
   in let c2 = bubCL1 (b : t)
       in if c1 == Nothing then (do
            t' <- bubCL1 $ b : t
            return $ a : t') else (do
            t' <- bubCL1 $ a : t
            return $ b : t')

-- | bubule Clifford H, CZ, Z to the left end.
bubCL1 :: [Gate] -> Maybe [Gate]
bubCL1 [] = Nothing
bubCL1 [a] = Nothing
bubCL1 ((H i) : (CCZ j k l) : t) = if i `elem` [j, k, l] then (do
                                     t' <- bubCL1 $ (CCZ j k l) : t
                                     return $ (H i) : t') else Just $ (CCZ j k l) : (H i) : t
bubCL1 ((X i) : (CCZ j k l) : t) = if i `elem` [j, k, l] then (do
                                     t' <- bubCL1 $ (CCZ j k l) : t
                                     return $ (X i) : t') else Just $ (CCZ j k l) : (X i) : t
bubCL1 ((Z i) : (CCZ j k l) : t) = Just $ CCZ j k l : Z i : t
bubCL1 ((CZ i a) : (CCZ j k l) : t) = Just $ CCZ j k l : CZ i a : t
bubCL1 ((Cnot i a) : (CCZ j k l) : t) = if i `elem` [j, k, l] then (do
                                          t' <- bubCL1 $ (CCZ j k l) : t
                                          return $ (Cnot i a) : t') else Just $ (CCZ j k l) : (Cnot i a) : t
bubCL1 ((H i) : (CZ j k) : t) = if i `elem` [j, k] then (do
                                  t' <- bubCL1 $ (CZ j k) : t
                                  return $ (H i) : t') else bubChoice (H i) (CZ j k) t
bubCL1 (p@(CZ i a) : q@(CZ j k) : t) = if sort [i, a] == sort [j, k] then Just t else bubChoice p q t
bubCL1 (p@(CZ j k) : q@(Z i) : t) = bubChoice p q t
bubCL1 (p@(CZ j k) : q@(H i) : t) = if i `elem` [j, k] then (do
                                      t' <- bubCL1 (q : t)
                                      return $ p : t') else bubChoice p q t
bubCL1 (p@(CZ j k) : q@(X i) : t) = if i `elem` [j, k] then (do
                                      t' <- bubCL1 (q : t)
                                      return $ p : t') else bubChoice p q t
bubCL1 (p@(CZ j k) : q@(Cnot i a) : t) = if i `elem` [j, k] then (do
                                           t' <- bubCL1 (q : t)
                                           return $ p : t') else bubChoice p q t
bubCL1 (p@(Cnot i a) : q@(Cnot j k) : t) = if [i, a] == [j, k] then Just t else (case i /= k && a /= j of
                                                                          True -> bubChoice p q t
                                                                          -- we will probably introduce Swap, here is one reason
                                                                          False -> do
                                                                            t' <- bubCL1 $ q : t
                                                                            return $ p : t')
bubCL1 (p@(Cnot j k) : q@(Z i) : t) = if i == j then (do
                                        t' <- bubCL1 $ q : t
                                        return $ p : t') else bubChoice p q t
bubCL1 (p@(Cnot j k) : q@(H i) : t) = if i == j then Just $ (H i) : (CZ j k) : t else (case i == k of
                                                                                True -> do
                                                                                  t' <- bubCL1 $ q : t
                                                                                  return $ p : t'
                                                                                False -> bubChoice p q t)
bubCL1 (p@(Cnot j k) : q@(X i) : t) = if i `elem` [j, k] then (do
                                        t' <- bubCL1 $ q : t
                                        return $ p : t') else bubChoice p q t
-- commuting Z with H X Z
bubCL1 ((Z i) : (H j) : t) = if i == j then (do
                               t' <- bubCL1 $ (H j) : t
                               return $ (Z i) : t') else bubChoice (Z i) (H j) t
bubCL1 ((Z i) : (X j) : t) = if i == j then (do
                               t' <- bubCL1 $ (X j) : t
                               return $ (Z i) : t') else bubChoice (Z i) (X j) t
bubCL1 (p@(Z i) : q@(Z j) : t) = if i == j then Just t else bubChoice p q t
-- commuting X with H X
bubCL1 (p@(X i) : q@(X j) : t) = if i == j then Just t else bubChoice p q t
bubCL1 ((X i) : (H j) : t) = if i == j then Just $ (H j) : (Z i) : t else bubChoice (X i) (H j) t
-- catch all
{-bubCL1 (p:q:t) = case wiresOfGate p `intersect` wiresOfGate q == [] of
  True -> bubChoice p q t
  False -> do
    t' <- bubCL1 $ q : t
    return $ p : t'
-}
bubCL1 (h : t) = do
  t' <- bubCL1 t
  return $ h : t'

bubCL = repeatedly bubCL1

bubCLR :: LMR [Gate] -> LMR [Gate]
bubCLR (LMR l m r) = LMR l' m' r'
  where
    m1 = bubCL m
    r1 = takeWhileB isCliffordg m1
    m1' = dropWhileB isCliffordg m1
    r' = r1 ++ r
    m2 = reverse m1'
    l1 = reverse $ takeWhileB isCliffordg $ bubCL m2
    m2' = reverse $ dropWhileB isCliffordg $ bubCL m2
    l' = l ++ l1
    m' = m2'

-- | Special consideration for moving certain HG to the begining. This
-- only works for circuit without Init and Term. moveHGtoLeft move the
-- HG on i-th wire to the left end if possilbe.
move1 :: ZXTerm -> Maybe ZXTerm
move1 (g2@(G p ts) : g1@(CnotG t c) : r) = case (t `elem` ts, c `elem` ts) of
  (True, True) -> Just $ g1 : G p (delete c ts) : r
  (True, False) -> Just $ g1 : G p (c : ts) : r
  (_, _) -> Just $ g1 : g2 : r
move1 (g@(G p ts) : s@(SwapG i j) : r) = case (i `elem` ts, j `elem` ts) of
  (True, True) -> Just (s : g : r)
  (True, False) -> Just (s : (G p (j : delete i ts) : r))
  (False, True) -> Just (s : (G p (i : delete j ts) : r))
  (False, False) -> Just (s : g : r)
-- same as the first clause of this rule.
move1 (g@(G p ts) : s@(XG i) : r) = if i `elem` ts then Just (s : (G ((8 - p) `mod` 8) ts) : r) else Just (s : g : r)
move1 (g@(G p ts) : s@(ZG i) : r) = Just (s : g : r)
move1 (g@(G p ts) : s@(CZG i j) : r) = Just (s : g : r)
move1 (g@(G p ts) : s@(SG i) : r) = Just (s : g : r)
move1 (g@(G p ts) : s@(S'G i) : r) = Just (s : g : r)
move1 (h : t) = do
  t' <- move1 t
  return (h : t')
move1 [] = Nothing

-- | acting on Cliffod -HG + gadget circuit. before call this, call
-- hRwrite and cnzRewrite first.
bubleCliffordLeft :: ZXTerm -> ZXTerm
bubleCliffordLeft = repeatedly move1

moveNonGsToEnds :: Term -> Term
moveNonGsToEnds term@(LMR li mi ri) = termok
  where
    ml@(LMR l m _) = moveNonGsL [] mi (LMR [] [] [])
    mr@(LMR _ m' r) = moveNonGsR [] m (LMR [] [] [])
    --  mr@(LMR l2 m' _) = moveNonGsL [] (reverse mi) (LMR [] [] [])
    termok = LMR (li ++ l) m' (r ++ ri)

-- | first argument is the barriers prevent moving NonGs and IT.
moveNonGsL :: [Wire] -> ZXTerm -> Term -> Term
moveNonGsL _ [] term = term
moveNonGsL barr (h : t) term@(LMR l m r) = if (not . isGadget) h then (case barr `block` h of
                                                                 True -> moveNonGsL (barr `union` wiresOfAtom h) t (LMR l (m ++ [h]) r)
                                                                 False -> moveNonGsL (barr) t (LMR (l ++ [h]) m r)) else moveNonGsL (barr `union` wiresOfAtom h) t (LMR l (m ++ [h]) r)

-- | first argument is the barriers prevent moving NonGs.
moveNonGsR :: [Wire] -> ZXTerm -> Term -> Term
moveNonGsR barr zxterm term = termok
  where
    zxterm' = reverse zxterm
    (LMR l m r) = moveNonGsL barr zxterm' term
    termok = LMR [] (reverse m) (reverse l)

block :: [Wire] -> ZXAtom -> Bool
block bs a = (not . null) $ bs `intersect` wiresOfAtom a

-- | Two strategies to reduce T-count. 1) fuse 2) rewrite. We define
-- them on Term.
-- t2term :: Term -> Term
-- t2term t@(LMR l1 m1 r1) = t5 where
--   t3@(LMR l3 m4 r3) =  moveNonGsToEnds $ t
--   (isc, notc) = partition (isClifford) m5
--   notc' = fuseHomoGs notc
--   (og,eg) = partition isoddG notc'
--   t5 = LMR (l3++isc++eg) (sortBy gorder1 og) r3

-- | Two strategies to reduce T-count. 1) fuse 2) rewrite. We define
-- them on Term.
zx2term :: ZXTerm -> Term
zx2term zxterm = t5
  where
    t1@(LMR l1 m1 r1) = moveNonGsToEnds $ LMR [] zxterm []
    m3 = hRewrite m1
    t3@(LMR l3 m4 r3) = moveNonGsToEnds $ LMR l1 m3 r1
    m5 = bubleCliffordLeft m4
    (isc, notc) = partition isClifford m5
    notc' = fuseHomoGs notc
    (og, eg) = partition isoddG notc'
    t5 = LMR (l3 ++ isc ++ eg) (sortBy gorder1 og) r3

c2term :: [Gate] -> Term
c2term = zx2term . c2zx2

-- | get the middle part of a term.
-- mid :: Term -> ZXTerm
mid (LMR l m r) = m

fuse :: Term -> Term
fuse (LMR l m r) = LMR l m' r
  where
    m' = fuseHomoGs m

heuri :: Term -> Term
heuri = rewriteUsingMyId myid4 . fuse

-- | Printing
zx2cir :: [Qubit] -> ZXAtom -> Circ [Qubit]
zx2cir qs (InitG s i) = do
  qi <- qinit False
  --  gate_H qi   -- (i+1) or i
  let qs' = take i qs ++ [qi] ++ drop (i + 1) qs
  --  named_gate (decodeQState s) (qi )
  return qs'
zx2cir qs (TermG s i) = do
  --  gate_H (qs !! i)
  --  named_gate (decodeQState' s) (qs !! (i))
  measure (qs !! i)
  return qs -- (delete (qs !! i) qs)
zx2cir qs (SwapG i j) = do
  swap_at (qs !! i) (qs !! j)
  return qs
zx2cir qs (CnotG i j) = do
  qnot (qs !! i) `controlled` (qs !! j)
  return qs
--zx2cir (qs) (G k is) = do
--named_gate (decodeAngle k)  (map (\x -> qs !! (x)) $ is)
--return qs
zx2cir qs (G k is) = do
  named_gate (show (k `mod` 8)) (qs !! minimum is) `controlled` map (\x -> qs !! (x)) (is \\ [minimum is])
  return qs
zx2cir qs (HG i) = do
  gate_H (qs !! i)
  return qs
zx2cir qs (CNZG is) = do
  gate_Z (qs !! head is) `controlled` map (\x -> qs !! (x)) (drop 1 is)
  return qs
zx2cir qs (Tn i j) = do
  named_gate ("T" ++ show (i `mod` 8)) (qs !! j)
  return qs
zx2cir qs (CTn i j k) = do
  named_gate ("T" ++ show (i `mod` 8)) (qs !! j) `controlled` [qs !! k]
  return qs
zx2cir qs (CCTn i j k l) = do
  named_gate ("T" ++ show (i `mod` 8)) (qs !! j) `controlled` [qs !! k, qs !! l]
  return qs
zx2cir qs (XG i) = do
  gate_X (qs !! i)
  return qs
zx2cir qs (YG i) = do
  gate_Y (qs !! i)
  return qs
zx2cir qs (ZG i) = do
  gate_Z (qs !! i)
  return qs
zx2cir qs (CZG i j) = do
  gate_Z (qs !! i) `controlled` (qs !! j)
  return qs
zx2cir qs (SG i) = do
  gate_S (qs !! i)
  return qs
zx2cir qs (S'G i) = do
  gate_S_inv (qs !! i)
  return qs

-- ----------------------------------------------------------------------

-- * Useful general-purpose functions

-- | Repeatedly apply a one-step reduction until it's done.
repeatedly :: (a -> Maybe a) -> (a -> a)
repeatedly f a = case f a of
  Nothing -> a
  Just b -> repeatedly f b

-- ----------------------------------------------------------------------

-- | desugar to gate set { CNZ, CZ, Cnot, X, H, S, T}
desugar_to_ZZHST :: [Gate] -> [Gate]
desugar_to_ZZHST circ = concat [desugar_gate x | x <- circ]
  where
    --    desugar_gate (Cnot i j) = [H i, CZ i j, H i]
    --    desugar_gate (X i) = [H i, S i, S i, H i]
    --    desugar_gate (S' i) = [S i, S i, S i]
    desugar_gate (Z i) = [S i, S i]
    --    desugar_gate (T' i) = [S i, S i, S i, T i]
    desugar_gate g = [g]

-- | desugar to gate set {H, T, S, Z, CZ, CCZ, X, CX, Swap}
desugar_cir :: [Gate] -> [Gate]
desugar_cir circ = concat [desugar_gate x | x <- circ]
  where
    --    desugar_gate (Cnot i j) = [H i, CZ i j, H i]
    --    desugar_gate (X i) = [H i, S i, S i, H i]
    --    desugar_gate (S' i) = [S i, S i, S i]
    --    desugar_gate (Z i) = [S i, S i]
    --    desugar_gate (T' i) = [S i, S i, S i, T i]
    desugar_gate (Toffoli i j k) = [H i, CCZ i j k, H i]
    desugar_gate (CCX i j k) = [H i, CCZ i j k, H i]
    desugar_gate g = [g]

-- | desugar to gate set {H, T, S, Z, CZ, CCZ, X, CX, Swap}
desugar_cir' :: [Gate] -> [Gate]
desugar_cir' circ = concat [desugar_gate x | x <- circ]
  where
    desugar_gate (Cnot i j) = [H i, CZ i j, H i]
    --    desugar_gate (X i) = [H i, S i, S i, H i]
    --    desugar_gate (S' i) = [S i, S i, S i]
    --    desugar_gate (Z i) = [S i, S i]
    --    desugar_gate (T' i) = [S i, S i, S i, T i]
    desugar_gate (Toffoli i j k) = [H i, CCZ i j k, H i]
    desugar_gate g = [g]

-- | desugar gate set {CCZ, CCX, CZ, CX, Z, X, H} to gate set {CCZ,
-- CZ, Z, H}
desugar35 :: [Gate] -> [Gate]
desugar35 circ = concat [desugar_gate x | x <- circ]
  where
    desugar_gate (Toffoli i j k) = [H i, CCZ i j k, H i]
    --    desugar_gate (Cnot i j) = [H i, CZ i j, H i]
    --    desugar_gate (X i) = [H i, Z i,  H i]
    desugar_gate g = [g]

-- | commute relation between {CCZ, CZ, Cnot, X, H, Z}
commute4 :: Gate -> Gate -> Bool
commute4 (H i) (CCZ j k l) = if i `elem` [j, k, l] then False else True
commute4 (H i) (CZ j k) = if i `elem` [j, k] then False else True
commute4 (H i) (Z j) = if i == j then False else True
commute4 (H i) (H j) = i > j
commute4 (Z i) (CCZ j k l) = True
commute4 (Z i) (CZ j k) = True
commute4 (Z i) (Z j) = i > j
commute4 (CZ i a) (CCZ j k l) = True
commute4 (CZ i a) (CZ j k) = (i + a) > (j + k)
commute4 (CCZ i a b) (CCZ j k l) = (i + a + b) > (j + k + l)
commute4 _ _ = False

csort4 = sort_word commute4

-- | commute relation between {CNZ, CZ, Cnot, X, H, S, T}
commute :: Gate -> Gate -> Bool
commute (H i) (CNZ ws) = if i `elem` ws then False else True
commute (H i) (CZ i' j) = if i /= i' && i /= j then True else False
commute (H i) (Cnot i' j) = if i /= i' && i /= j then True else False
commute (H i) (X i') = if i /= i' then True else False
commute (H i) (H i') = True
commute (H i) (S i') = if i /= i' then True else False
commute (H i) (T i') = if i /= i' then True else False
commute g (H i) = commute (H i) g
commute (X i) (CNZ ws) = if i `elem` ws then False else True
commute (X i) (CZ i' j) = if i /= i' && i /= j then True else False
commute (X i) (Cnot i' j) = if i /= i' && i /= j then True else False
commute (X i) (X i') = True
commute (X i) (H i') = if i /= i' then True else False
commute (X i) (S i') = if i /= i' then True else False
commute (X i) (T i') = if i /= i' then True else False
commute g (X i) = commute (X i) g
commute (Cnot i j) (CNZ ws) = if i `elem` ws then False else True
commute (Cnot i j) (CZ i' j') = if i /= i' && i /= j' then True else False
commute (Cnot i j) (Cnot i' j') = if i /= j' && i' /= j then True else False
commute (Cnot i j) (X i') = if i /= i' && i' /= j then True else False
commute (Cnot i j) (H i') = if i /= i' && i' /= j then True else False
commute (Cnot i j) (S i') = if i /= i' then True else False
commute (Cnot i j) (T i') = if i /= i' then True else False
commute g (Cnot i j) = commute (Cnot i j) g
commute _ _ = True

-- ----------------------------------------------------------------------

-- * Standardize commuting operators

-- | Return all possible ways to split a list into a prefix, a single
-- element, and a postfix. Example:
--
-- > splits [1,2,3,4] = [ ([],1,[2,3,4]),
-- >                      ([1],2,[3,4]),
-- >                      ([1,2],3,[4]),
-- >                      ([1,2,3],4,[])
-- >                    ].
splits :: [a] -> [([a], a, [a])]
splits [] = []
splits (h : t) = ([], h, t) : [(h : pre, x, post) | (pre, x, post) <- splits t]

-- | Input a word, and a function to determine whether two letters
-- commute. Output the alphabetically first representative of the
-- equivalence class of the given word modulo commutations.
sort_word :: (Ord a, Eq a) => (a -> a -> Bool) -> [a] -> [a]
sort_word com w = case best of
  [] -> []
  (pre, x, post) : _ -> x : sort_word com (pre ++ post)
  where
    candidates = [(pre, x, post) | (pre, x, post) <- splits w, all (com x) pre]
    best = sortBy (compare `on` (\(p, x, q) -> x)) candidates

-- | Consider the equivalence relation on circuits generated by
-- pairwise permutation of commuting gates. This function picks out
-- the alphabetically first representative in each equivalence class.
constrained_sort :: [Gate] -> [Gate]
constrained_sort = sort_word commute

toBasicGS :: [Gate] -> [Gate]
toBasicGS = desugar_to_ZZHST . preProcess

bgsorder :: Gate -> Gate -> Ordering
bgsorder x y =
  if (minimum $ wiresOfGate x) == (minimum $ wiresOfGate y)
    then compare x y
    else compare (minimum $ wiresOfGate x) (minimum $ wiresOfGate y)

-- ----------------------------------------------------------------------
hreduce_step :: [Gate] -> Maybe [Gate]
hreduce_step [] = Nothing
hreduce_step [a] = Nothing
hreduce_step ((H i) : (H j) : t) = if i == j then Just t else (do
                                     t' <- hreduce_step ((H j) : t)
                                     return $ (H i) : t')
hreduce_step xs@((H i) : (S j) : (H k) : t) =
  if nub [i, j, k] == [i] then Just $ [S i, S i, S i, H i, S i, S i, S i] ++ t else (do
    t' <- hreduce_step $ drop 1 xs
    return $ (H i) : t')
hreduce_step ((H i) : (S j) : (S l) : (H m) : t) =
  if nub [i, j, l, m] == [i] then Just $ [X i] ++ t else (do
    t' <- hreduce_step ((S j) : (S l) : (H m) : t)
    return $ (H i) : t')
hreduce_step xs@((H i) : (X j) : (H m) : t) =
  if nub [i, j, m] == [i] then Just $ [S i, S i] ++ t else (do
    t' <- hreduce_step $drop 1 xs
    return $ (H i) : t')
hreduce_step xs@((H i) : (S j) : (S k) : (S k') : (CZ l m) : (H n) : (CZ o p) : t) =
  case nub [i, j, k, k', n] == [i] && sort [l, m] == sort [o, p] && i `elem` [l, m] of
    True -> Just $ [CZ o p, S i, S q, H i] ++ t
      where
        q = head $ [o, p] \\ [i]
    False -> do
      t' <- hreduce_step $ drop 1 xs
      return $ H i : t'
hreduce_step xs@((H i) : (CZ j k) : (H l) : t) =
  if nub [i, l] == [i] && i `elem` [j, k] then Just $ [Cnot i k] ++ t else (do
    t' <- hreduce_step $ drop 1 xs
    return $ (H i) : t')
hreduce_step xs@((H i) : (Cnot j k) : (H l) : t) =
  if nub [i, l] == [i] && i == j then Just $ [CZ j k] ++ t else (do
    t' <- hreduce_step $ drop 1 xs
    return $ (H i) : t')
hreduce_step ((H i) : (S j) : (S l) : t) =
  if nub [i, j, l] == [i] then Just $ [X i, H i] ++ t else (do
    t' <- hreduce_step ((S j) : (S l) : t)
    return $ (H i) : t')
hreduce_step xs@((H i) : (X j) : t) =
  if nub [i, j] == [i] then Just $ [S i, S i, H i] ++ t else (do
    t' <- hreduce_step $drop 1 xs
    return $ (H i) : t')
hreduce_step xs@((H i) : (S j) : (S k) : (S k') : (CZ l m) : (H n) : t) =
  case nub [i, j, k, k', n] == [i] && i `elem` [l, m] of
    True -> Just $ [CZ l m, S i, S q, H i, CZ l m] ++ t
      where
        q = head $ [l, m] \\ [i]
    False -> do
      t' <- hreduce_step $ drop 1 xs
      return $ H i : t'
hreduce_step xs@((H i) : (CZ j k) : t) =
  if i `elem` [j, k] then Just $ [Cnot i k, H i] ++ t else (do
    t' <- hreduce_step $ drop 1 xs
    return $ (H i) : t')
hreduce_step xs@((H i) : (Cnot j k) : t) =
  if i == j then Just $ [CZ j k, H i] ++ t else (do
    t' <- hreduce_step $ drop 1 xs
    return $ (H i) : t')
hreduce_step (h : t) = do
  t' <- hreduce_step t
  return (h : t')

-- | Keep reducing until there's nothing to reduce.
hreduce :: [Gate] -> [Gate]
hreduce = repeatedly (hreduce_step . constrained_sort)

-- ----------------------------------------------------------------------
hreduce_step35 :: [Gate] -> Maybe [Gate]
hreduce_step35 [] = Nothing
hreduce_step35 [a] = Nothing
hreduce_step35 ((H i) : (H j) : t) = if i == j then Just t else (
                                       let t1 = hreduce_step35 $ (H i) : t
                                        in let t2 = hreduce_step35 $ (H j) : t
                                            in case t1 == Nothing of
                                                 True -> do
                                                   t' <- hreduce_step35 ((H j) : t)
                                                   return $ (H i) : t'
                                                 False -> case t2 == Nothing of
                                                   True -> return $ (H j) : (unJust t1)
                                                   False -> Just $ (H j) : (unJust t1))
hreduce_step35 xs@((H i) : (X j) : t) =
  if i == j then Just $ [Z i, H i] ++ t else Just $ [X j, H i] ++ t
hreduce_step35 xs@((H i) : (Z j) : t) =
  if i == j then Just $ [X i, H i] ++ t else Just $ [Z j, H i] ++ t
hreduce_step35 xs@((H i) : (CZ j k) : t) =
  if i == j then Just $ [Cnot j k, H i] ++ t else (case i == k of
                                            True -> Just $ [Cnot k j, H i] ++ t
                                            False -> Just $ [CZ j k, H i] ++ t)
hreduce_step35 xs@((H i) : (Cnot j k) : t) =
  if i == j then Just $ [CZ i k, H i] ++ t else (case i == k of
                                          True -> do
                                            t' <- hreduce_step35 $ (Cnot j k) : t
                                            return $ (H i) : t'
                                          False -> return $ [Cnot j k, (H i)] ++ t)
hreduce_step35 xs@((H i) : (CCZ j k l) : t) =
  if i `elem` [j, k, l] then (do
    t' <- hreduce_step35 $ (CCZ j k l) : t
    return $ (H i) : t') else return $ (CCZ j k l) : (H i) : t
hreduce_step35 (h : t) = do
  t' <- hreduce_step35 t
  return (h : t')

-- | Keep reducing until there's nothing to reduce.
hreduce35 :: [Gate] -> [Gate]
hreduce35 = repeatedly hreduce_step35

takeWhileB :: (a -> Bool) -> [a] -> [a]
takeWhileB p xs = reverse $ takeWhile p $ reverse xs

dropWhileB :: (a -> Bool) -> [a] -> [a]
dropWhileB p xs = reverse $ dropWhile p $ reverse xs

hred35 :: LMR [Gate] -> LMR [Gate]
hred35 (LMR l cir r) = LMR cl' cir' cr'
  where
    c' = hreduce35 cir
    cir1 = dropWhileB isCliffordg c'
    cr = takeWhileB isCliffordg c'
    cr' = cr ++ r
    c'' = hreduce35 $ reverse cir1
    cir2 = reverse $ dropWhileB isCliffordg c''
    cl = reverse $ takeWhileB isCliffordg c''
    cl' = l ++ cl
    cir' = cir2

-- | determine whether a Gate is Clifford
isCliffordg :: Gate -> Bool
isCliffordg (Swap _ _) = True
isCliffordg (H _) = True
isCliffordg (Y _) = True
isCliffordg (S _) = True
isCliffordg (Z _) = True
isCliffordg (CZ _ _) = True
--isCliffordg (S' _) = True
isCliffordg (Cnot _ _) = True
isCliffordg (X _) = True
isCliffordg (CX _ _) = True
isCliffordg _ = False

-- main = do
--   args <- getArgs
--   str <- readFile $ (head args)
--   let cir_in = runParser pcir_in str
--   let nn_in = n cir_in
--   let xn_in = nx cir_in
--   let zn_in = nz cir_in
--   let qs_in = replicate nn_in qubit

{-
readCir :: String -> IO [Gate]
readCir s = do
  str <- readFile $ "TODD/" ++ s ++ ".log"
  let cir_in = runParser pcir_in str
  let nn_in = n cir_in
  let xn_in = nx cir_in
  let zn_in = nz cir_in
  return $ gates cir_in
-}

{-
--main :: IO String
mmmain = do
  args <- getArgs
  str <- readFile $ (head args)
  let cir_in = runParser pcir_in str
  let nn_in = n cir_in
  let xn_in = nx cir_in
  let zn_in = nz cir_in
  let qs_in = replicate nn_in qubit
  print_generic ASCII (\x -> do
                         label (drop xn_in x) "|0>"
                         foldM gl2cir x (gates cir_in)
                         label (drop xn_in x) "|0>"
                     ) qs_in
  -- return $ show cir_in ++ show xn_in
  -- let cir_out = runParser pcir_out str
  -- let nn_out = n cir_out
  -- let xn_out = nx cir_out
  -- let zn_out = nz cir_out
  -- let qs_out = replicate nn_out qubit
  -- -- print_generic PDF (\x -> do
  -- --                       label (drop xn_out x) "|0>"
  -- --                       foldM gl2cir x (gates cir_out)
  -- --                       label (drop xn_out x) "|0>"
  -- --                   ) qs_out
  -- return $ show cir_out ++ show xn_out
  let gl_in = gates cir_in
  let term = c2term gl_in
  let t_before = tCount term
  let term_reduced = heuri term
  let t_after = tCount term_reduced
  appendFile (args !! 1) ("\n" ++ prettyprint (drop 7 (head args)) ++ "           " ++ show t_before  ++ "            " ++ show t_after )
-}

quip s = do
  str <- readFile $ "gf2^4_mult_pyzx_todd" ++ s ++ ".quip"
  let (q, f) = parse_circuit str
  print_generic Preview f q

avefun :: ZXAtom -> Rational
avefun (G i inds) = 1 / fromIntegral (length inds)
avefun _ = 0

aveTG :: Int -> Term -> Rational
aveTG i (LMR l m r) = ok
  where
    msf = focusOnwires [i] m
    rmsf = map avefun msf
    ok = sum rmsf

aveTG' :: Int -> ZXTerm -> Rational
aveTG' i m = ok
  where
    msf = focusOnwires [i] m
    rmsf = map avefun msf
    ok = sum rmsf

width :: ZXTerm -> Int
width xs = 1 + maximum (wiresOfTerm xs)

aveTGs :: Term -> [Rational]
aveTGs (LMR l m r) = ok
  where
    candis = [0 .. (width (l ++ m ++ r) - 1)]
    candisf = map aveTG' candis
    ok = map (\f -> f m) candisf

aveTGsi :: Term -> [(Int, Rational)]
aveTGsi (LMR l m r) = ok
  where
    candis = [0 .. (width (l ++ m ++ r) - 1)]
    candisf = map (\x -> (x, aveTG' x)) candis
    ok = map (\(i, f) -> (i, f m)) candisf

recho :: Int -> [Int] -> [[Int]]
recho 1 xs = choosen 1 xs
recho _ [] = []
recho n xs = [t ++ h | h <- choosen 1 xs, t <- recho (n -1) xs]

main4 = do
  args <- getArgs
  let ids = id45s5n [0, 1, 2, 3, 4]
  let index = read (args !! 1) :: Int
  topdf' $ zx2term (ids !! index)

{-
mmmmain = do
  args <- getArgs
  str <- readFile $ (head args)
  let cir_in = runParser pcir_in str
  let gl_in = gates cir_in
  let gl_in' = reindexCir (\x -> x-1) gl_in
  let term = c2term gl_in'
  let tt2 = c2zx gl_in'
  let t_before = t_count tt2
  let term_reduced = heuri5 term
  let mw = wiresOfTerm $ mid term
  let w5 = choosen 6 mw
  let w5t = map (\x -> let g5t = focusOnwiresExactly x (mid term) in (x,t_count g5t)) w5
  let w5t' = filter (\(x,y) -> y>=19) w5t
  let t_after = tCount term_reduced
  appendFile (args !! 1) ("\n" ++ prettyprint (drop 7 (head args)) ++ "           " ++ show w5t')

mainff = do
  args <- getArgs
  str <- readFile $ (head args)
  let cir_in = runParser pcir_in str
  let gl_in = gates cir_in
  let gl_in' = reindexCir (\x -> x-1) gl_in
  let term = c2term gl_in'
  let tt2 = c2zx gl_in'
  let t_before = t_count tt2
  let term_reduced = heuri5 term
  let t_after = tCount term_reduced
  appendFile (args !! 1) ("\n" ++ prettyprint (drop 7 (head args)) ++ "           " ++ show t_before  ++ "            " ++ show (t_after) )

-}

print_list :: Show a => [a] -> String
print_list [] = ""
print_list (h : t) = show h ++ "\n" ++ print_list t

prettyprint :: String -> String
prettyprint s = take 10 (reverse (drop 4 $reverse s) ++ "         ")

prettyprint' :: String -> String
prettyprint' s = take 15 (s ++ "             ")

c2pdf = zx2pdf . c2zx

zx2pdf :: ZXTerm -> IO ()
zx2pdf zxterm = do
  let numOfWires = length $ wiresOfTerm zxterm
  print_generic
    Preview
    ( \x -> do
        foldM zx2cir x zxterm
    )
    (replicate numOfWires qubit)

cir2pdf :: [Gate] -> IO ()
cir2pdf cir = do
  let numOfWires = length $ wiresOfCir cir
  print_generic
    Preview
    ( \x -> do
        foldM gl2cir1 x cir
    )
    (replicate numOfWires qubit)

previewzx = do
  print_simple Preview cir
  where
    cir = do
      q <- qinit False
      p <- qinit True
      r <- qinit True
      q1 <- qinit False
      p1 <- qinit True
      r1 <- qinit True
      zx2cir [p, q, r, p1, q1, r1] (G 1 [1, 2, 5])

--preview :: Term -> IO ()
term2pdf (LMR l m r) = do
  let lmr = l ++ m ++ r
  let numOfWires = length $ wiresOfTerm lmr
  print_generic
    Preview
    ( \x -> do
        foldM zx2cir x lmr
    )
    (replicate numOfWires qubit)

powerset [] = [[]]
powerset (h : t) = map (h :) (powerset t) ++ powerset t

tof2ccz_cnot2cnot :: Gate -> Gate
tof2ccz_cnot2cnot (Cnot i j) = Cnot j i
tof2ccz_cnot2cnot (Toffoli i j k) = CCZ i j k
tof2ccz_cnot2cnot x = x

-- -- | Gadgets to polynomial
-- g2p :: ZXAtom -> [(Int,V5)]
-- g2p (G k ws) = case length ws of
--   1 -> map (\x -> (k, VX x)) ws
--   2 -> map (\x -> (k, VX x)) ws ++ [(-2*k, XX (ws!!0), ( ws!!1))]
--   3 -> map (\x -> (k, VX x)) ws ++ [(-2*k), ( XX (ws!!0), ( ws!!1))]
--     ++ [(-2*k, XX (ws!!0), ( ws!!2))]++ [(-2*k, XX (ws!!1), ( ws!!2))]
--     ++ [(4*k,XXX (ws!!0), ( ws!!1), ( we!!2))]
--   4 -> map (\x -> (k, VX x)) ws ++ [(-2*k, XX (ws!!0), ( ws!!1))]
--     ++ [(-2*k, XX (ws!!0), ( ws!!2))]++ [(-2*k, XX (ws!!0), ( ws!!3))]
--     ++ [(-2*k, XX (ws!!1), ( ws!!2))]++ [(-2*k, XX (ws!!1), ( ws!!3))]
--     ++ [(-2*k, XX (ws!!2), ( ws!!3))]
--     ++ [(4*k,XXX (ws!!0), ( ws!!1), ( we!!2))]
--     ++ [(4*k,XXX (ws!!0), ( ws!!1), ( we!!3))]
--     ++ [(4*k,XXX (ws!!0), ( ws!!2), ( we!!3))]
--     ++ [(4*k,XXX (ws!!1), ( ws!!2), ( we!!3))]
--   5 -> map (\x -> (k, VX x)) ws
--     ++ concat $ map (\x -> (-2*k, XX (x!!0) (x!!1) )) $ choosen 2 ws
