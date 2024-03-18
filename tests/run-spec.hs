-- This file is not necessary for `cabal test` (which uses Spec.hs instead) but
-- makes if possible to load the tests in GHCi and ghcid.

import Curiosity.Runner (runSpec) 


--------------------------------------------------------------------------------
main :: IO ()
main = runSpec
