
-- | This module keeps a persistent @bimap@ between @Text@s and @Int@s.

module NLP.Text.BTI.Internal where

import Data.IORef (newIORef,IORef,readIORef,atomicWriteIORef,atomicModifyIORef')
import System.IO.Unsafe (unsafePerformIO,unsafeDupablePerformIO)
import Data.ByteString.UTF8

import Data.Bijection.HashMap
import Data.Bijection.Vector



btiBimap :: IORef (Bimap (HashMap ByteString Int) (Vector ByteString))
btiBimap = unsafePerformIO $ newIORef empty
{-# NoInline btiBimap #-}

-- | Add @UTF8 ByteString@ and return @Int@ key. Will return key for
-- existing string and thereby serves for lookup in left-to-right
-- direction.

btiBimapAdd :: ByteString -> Int
btiBimapAdd k = seq k .unsafeDupablePerformIO . atomicModifyIORef' btiBimap $ \m ->
  case lookupL m k of Just i  -> (m,i)
                      Nothing -> let s = size m
                                 in  (insert m (k,s) , s)
{-# Inline btiBimapAdd #-}

-- | Lookup the @InternedMultiChar@ based on an @Int@ key. Unsafe totality
-- assumption.

btiBimapLookupInt :: Int -> ByteString
btiBimapLookupInt r = seq r . unsafeDupablePerformIO $ go <$> readIORef btiBimap
  where go m = case (m `seq` lookupR m r) of
                 Just l  -> l
                 Nothing -> error "btiBimapLookupInt: totality assumption invalidated"
{-# Inline btiBimapLookupInt #-}

