{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DemotePairsPass where
import C
import Utils
import AST
import CDefs
import qualified Data.Map as Map
import qualified Data.Set as Set

