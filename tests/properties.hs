
module Main where

import           Data.Stringable
import           Debug.Trace
import qualified Data.Aeson as A
import qualified Data.Binary as B
import qualified Data.Serialize as S
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.TH

import           NLP.Text.BTI



-- * BTI (TODO move to LinguisticsTypes)

prop_InternTwice (t :: String) = getBTI x == getBTI y
  where x = bti $ fromString t
        y = bti $ fromString t

-- basic property of interning

prop_BTI (t :: String)
  | t == u    = True
  | otherwise = traceShow (t, getBTI i, u) False
  where i :: BTI = fromString t
        u        = toString   i

-- binary

prop_Binary (t :: String) = t == toString j
  where i :: BTI = fromString t
        j :: BTI = B.decode $ B.encode i

-- cereal

prop_Serialize (t :: String) = Right t == (toString <$> j)
  where i ::               BTI = fromString t
        j :: Either String BTI = S.decode $ S.encode i

-- aeson (more complicated to due the json format!

prop_Aeson (t :: String) = Just [t] == (map toString <$> j)
  where i ::       [BTI] = [fromString t]
        j :: Maybe [BTI] = A.decode $ A.encode i



main :: IO ()
main = $(defaultMainGenerator)

