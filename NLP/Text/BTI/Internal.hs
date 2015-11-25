
-- | This module keeps a persistent @bimap@ between @Text@s and @Int@s
--
-- TODO make this a bimap @Text <-> Vector@. Compare performance when
-- printing backtracking results. (Do this after the Builder-based
-- backtracking is online)

module NLP.Text.BTI.Internal where

import Data.IORef (newIORef,IORef,readIORef,atomicWriteIORef,atomicModifyIORef')
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO,unsafeDupablePerformIO)

import Data.Bijection.Hash (Bimap,empty,lookupL,lookupR,size,insert)



btiBimap :: IORef (Bimap Text Int)
btiBimap = unsafePerformIO $ newIORef empty
{-# NoInline btiBimap #-}

-- | Add @Text@ and return @Int@ key. Will return key for
-- existing string and thereby serves for lookup in left-to-right
-- direction.

btiBimapAdd :: Text -> Int
btiBimapAdd k = unsafeDupablePerformIO $ atomicModifyIORef' btiBimap $ \m ->
  case lookupL m k of Just i  -> (m,i)
                      Nothing -> let s = size m
                                 in  (insert m (k,s) , s)
{-# Inline btiBimapAdd #-}

-- | Lookup the @InternedMultiChar@ based on an @Int@ key. Unsafe totality
-- assumption.

btiBimapLookupInt :: Int -> Text
btiBimapLookupInt r = seq r . unsafeDupablePerformIO $ atomicModifyIORef' btiBimap $ \m ->
  case lookupR m r of Just l  -> (m,l)
                      Nothing -> error "btiBimapLookupInt: totality assumption invalidated"
{-# Inline btiBimapLookupInt #-}

