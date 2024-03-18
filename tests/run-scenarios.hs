-- \| This test program run scripts similarly to @cty run@, ensuring their
-- outputs are identical to "golden" (expected) results. Scenarios (and their
-- golden files) can be found in the @scenarios/@ directory in the Curiosity
-- repository.
--
-- Use e.g. @scripts/run-tests.sh@ to execute this program.

import Curiosity.Runner (runScenarios)

--------------------------------------------------------------------------------
main :: IO ()
main = runScenarios
