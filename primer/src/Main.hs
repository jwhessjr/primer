module Main (main) where

import Foreword

import Data.Text qualified as T
import Hedgehog ( Property, withSkip, Seed )
import Tests.Refine (tasty_refinement_synths)
import Hedgehog.Internal.Runner (checkReport)
import Hedgehog.Internal.Property (Property(propertyConfig, propertyTest), Skip(SkipToShrink), TestCount, ShrinkPath)
import qualified Hedgehog.Internal.Seed as Seed
import Hedgehog.Internal.Report (reportStatus, Report (reportSeed, reportTests), Result (..), FailureReport (failureShrinkPath))
import Numeric.Natural (Natural)
import qualified Data.Map.Strict as M
import Data.List.Extra (enumerate)

main :: IO ()
main = do
  let n = 100
  rs <- replicateM (fromIntegral n) runAndRecheck
  let cs = count rs
  void $ M.traverseWithKey (\ri c -> putStrLn $ showPad ri <> " : " <> show c) cs
  if (cs M.! RecheckPass) + (cs M.! RecheckDefeat) > n `div` 4
    then putStrLn @Text "This tickled non-replay bug > 25%"
    else die "This did not tickle non-replay bug much"

-- Bounded & Enum: we explicitly give counts of 0 for those which did not appear
count :: (Bounded a, Enum a, Ord a) => [a] -> Map a Natural
count as = M.unionsWith (+) $ M.fromList [(a,0) | a <- enumerate] : fmap (`M.singleton` 1) as

-- This runs the test once with a random seed, and
-- - if it fails then rechecks it with the reported skip/shrink, reporting whether it finds an error again
-- - if it passes or gives up, report that
runAndRecheck :: IO RRInfo
runAndRecheck = either identity absurd <$> runExceptT go
 where
   go :: ExceptT RRInfo IO Void
   go = do
    seed <- Seed.random
    shrink <- ExceptT $ runProp seed tasty_refinement_synths <&> \case
      Passed -> Left RunPass
      Defeat -> Left RunDefeat
      Fail tc sp -> Right $ SkipToShrink tc sp
    -- This is essentially "recheckAt", with the skip/shrink info from above
    ExceptT $ fmap Left $ runProp seed (withSkip shrink tasty_refinement_synths) <&> \case
      Passed -> RecheckPass
      Defeat -> RecheckDefeat
      Fail _ _ -> RecheckRefind

data RRInfo
  = RunPass
  | RunDefeat
  | RecheckPass
  | RecheckDefeat
  | RecheckRefind -- rechecking finds /an/ error, not asserted /the same/ error
  deriving stock (Show, Eq, Ord, Enum, Bounded)

showPad :: RRInfo -> Text
showPad ri = let s = show ri in s <> T.replicate (13 - T.length s) " "

data RunInfo
  = Passed
  | Defeat
  | Fail TestCount ShrinkPath

runProp :: Seed -> Property -> IO RunInfo
runProp seed prop = do
  report <- checkReport (propertyConfig prop) 0 seed (propertyTest prop) $ const $ pure ()
  let testcount = reportTests report
  let seed' = reportSeed report
  -- check my understanding
  unless (seed == seed') $ die "seed /= seed'"
  pure $ case reportStatus report of
        GaveUp -> Defeat
        OK -> Passed
        Failed x -> Fail testcount $ failureShrinkPath x
