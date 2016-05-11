
module Main where

import           Data.String.Conversions
import           Debug.Trace
import qualified Data.Aeson as A
import qualified Data.Binary as B
import qualified Data.Serialize as S
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

import           NLP.Text.BTI



-- * BTI (TODO move to LinguisticsTypes)

prop_InternTwice (t :: String) = getBTI x == getBTI y
  where x = bti $ cs t
        y = bti $ cs t

-- basic property of interning

prop_BTI (t :: String)
  | t == u    = True
  | otherwise = traceShow (t, getBTI i, u) False
  where i :: BTI = btiFromCS t
        u        = btiToCS   i

-- binary

prop_Binary (t :: String) = t == btiToCS j
  where i :: BTI = btiFromCS t
        j :: BTI = B.decode $ B.encode i

-- cereal

prop_Serialize (t :: String) = Right t == (btiToCS <$> j)
  where i ::               BTI = btiFromCS t
        j :: Either String BTI = S.decode $ S.encode i

-- aeson (more complicated to due the json format!

prop_Aeson (t :: String) = Just [t] == (map btiToCS <$> j)
  where i ::       [BTI] = [btiFromCS t]
        j :: Maybe [BTI] = A.decode $ A.encode i



main :: IO ()
main = $(defaultMainGenerator)

