
module Main where

import           Criterion.Main
import           Data.Monoid
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB



tText = T.pack "Small"
{-# NoInline tText #-}

tBS = BC.pack "Small"
{-# NoInline tBS #-}

buildText :: Int -> TLB.Builder
buildText = go
  where go 0 = mempty
        go k = TLB.fromText tText <> go (k-1)
{-# NoInline buildText #-}

buildBS :: Int -> BB.Builder
buildBS = go
  where go 0 = mempty
        go k = BB.byteString tBS <> go (k-1)



main :: IO ()
main = defaultMain
  [ bgroup "10"
    [ bench "text" $ whnf (TL.length . TLB.toLazyText . buildText) 10
    , bench "bs  " $ whnf (BL.length . BB.toLazyByteString . buildBS) 10
    ]
  , bgroup "100"
    [ bench "text" $ whnf (TL.length . TLB.toLazyText . buildText) 100
    , bench "bs  " $ whnf (BL.length . BB.toLazyByteString . buildBS) 100
    ]
  , bgroup "1000"
    [ bench "text" $ whnf (TL.length . TLB.toLazyText . buildText) 1000
    , bench "bs  " $ whnf (BL.length . BB.toLazyByteString . buildBS) 1000
    ]
  ]

