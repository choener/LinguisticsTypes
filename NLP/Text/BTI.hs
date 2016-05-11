
-- | An implementation of @Int@-mapped @Text@s with internalization. Wrap
-- a @Text@ with 'bti' to receive a @BTI@. This internalizes the given
-- @Text@, meaning that two text inputs @x@ and @y@ will yield the same
-- @BTI@ if they have the same textual representation.
--
-- Since internalized @Text@ values are never released, be sure to use it
-- sparingly. I.e. to internalize words, not full sentences.

module NLP.Text.BTI where

import           Control.Applicative
import           Control.DeepSeq (NFData(..))
import           Data.Aeson as A
import           Data.Binary      as DB
import           Data.Hashable
import           Data.Serialize   as DS
import           Data.Serialize.Text
import           Data.String as IS
import           Data.String.Conversions
import           Data.Text.Binary
import           Data.Text (Text)
import           Data.Vector.Unboxed.Deriving
import           GHC.Generics

import           NLP.Text.BTI.Internal



-- | A @BTI@ behaves much like a @Text@, but is represented as an @Int@
-- internally.

newtype BTI = BTI { getBTI :: Int }
  deriving (Eq,Generic)

derivingUnbox "BTI"
  [t| BTI -> Int |]
  [|  getBTI     |]
  [|  BTI        |]

instance Ord BTI where
  BTI l `compare` BTI r = btiBimapLookupInt l `compare` btiBimapLookupInt r
  {-# Inline compare #-}

-- | Handy wrapper to internalize a @Text@ and get a 'BTI'.

bti :: Text -> BTI
bti s = BTI $! btiBimapAdd s
{-# Inline bti #-}

instance IsString BTI where
  fromString = bti . IS.fromString
  {-# Inline fromString #-}

instance Show BTI where
  showsPrec p i r = showsPrec p (btiToCS i :: String) r
  {-# Inline showsPrec #-}

instance Read BTI where
  readsPrec p str = [ (bti $ IS.fromString s, y) | (s,y) <- readsPrec p str ]
  {-# Inline readsPrec #-}

instance Hashable BTI

{-
instance Stringable BTI where
  toString   = toString . btiBimapLookupInt . getBTI
  fromString = bti . SA.fromString
  length     = SA.length . btiBimapLookupInt . getBTI
  toText     = toText . btiBimapLookupInt . getBTI
  fromText   = bti . fromText
  {-# Inline toString   #-}
  {-# Inline fromString #-}
  {-# Inline length     #-}
  {-# Inline toText     #-}
  {-# Inline fromText   #-}
-}

btiFromCS :: ConvertibleStrings x Text => x -> BTI
btiFromCS = bti . convertString

btiToCS :: ConvertibleStrings Text x => BTI -> x
btiToCS = convertString . btiToText

btiToText :: BTI -> Text
btiToText = btiBimapLookupInt . getBTI

instance NFData BTI where
  rnf = rnf . getBTI
  {-# Inline rnf #-}

instance Binary BTI where
  put = DB.put . btiToText
  get = bti <$> DB.get
  {-# Inline put #-}
  {-# Inline get #-}

instance Serialize BTI where
  put = DS.put . btiToText
  get = bti <$> DS.get
  {-# Inline put #-}
  {-# Inline get #-}

instance FromJSON BTI where
  parseJSON s = bti <$> parseJSON s
  {-# Inline parseJSON #-}

instance ToJSON BTI where
  toJSON = toJSON . btiToText
  {-# Inline toJSON #-}

