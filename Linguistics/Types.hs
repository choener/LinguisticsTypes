
-- | Assorted types used in linguistics.

module Linguistics.Types where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Data.Binary (Binary)
import Data.Aeson (FromJSON,ToJSON)
import Data.Serialize.Text ()



-- * Connectivity with @LingPy@.
--
-- <http://lingulist.de/lingpy/tutorial/formats.html>



-- | Concept
--
-- TODO ???

newtype Concept = Concept { getConcept :: Text }
  deriving (Eq,Ord,Generic)

instance Binary     Concept
instance Serialize  Concept
instance FromJSON   Concept
instance ToJSON     Concept

-- |
--
-- TODO ???

newtype Counterpart = Counterpart { getCountpart :: Text }
  deriving (Eq,Ord,Generic)

instance Binary     Counterpart
instance Serialize  Counterpart
instance FromJSON   Counterpart
instance ToJSON     Counterpart

-- | A word in phonetic @IPA@ notation.

newtype IPA = IPA { getIPA :: Text }
  deriving (Eq,Ord,Generic)

instance Binary     IPA
instance Serialize  IPA
instance FromJSON   IPA
instance ToJSON     IPA

-- |
--
-- TODO ???

newtype Doculect = Doculect { getDoculect :: Text }
  deriving (Eq,Ord,Generic)

instance Binary     Doculect
instance Serialize  Doculect
instance FromJSON   Doculect
instance ToJSON     Doculect

