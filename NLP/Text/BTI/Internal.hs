
-- | This module keeps a persistent @bimap@ between @Text@s and @Int@s.

module NLP.Text.BTI.Internal where

import Data.IORef (newIORef,IORef,readIORef,atomicWriteIORef,atomicModifyIORef')
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO,unsafeDupablePerformIO)

import Data.Bijection.HashMap
import Data.Bijection.Vector



btiBimap :: IORef (Bimap (HashMap Text Int) (Vector Text))
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
{-
btiBimapLookupInt r = seq r . unsafeDupablePerformIO $ atomicModifyIORef' btiBimap $ \m ->
  case lookupR m r of Just l  -> (m,l)
                      Nothing -> error "btiBimapLookupInt: totality assumption invalidated"
-}
btiBimapLookupInt r = seq r . unsafeDupablePerformIO $ go <$> readIORef btiBimap
  where go m = case (m `seq` lookupR m r) of
                 Just l  -> l
                 Nothing -> error "btiBimapLookupInt: totality assumption invalidated"
{-# Inline btiBimapLookupInt #-}

