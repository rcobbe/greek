-- | Normalization logic for polytonic Greek text in Unicode.  Using the ICU
--   bindings doesn't work, for two reasons:
--
--   (1) Linking nonsense with libiconv on MacOS/MacPorts.  See
--   <http://blog.omega-prime.co.uk/?p=96> for a partial discussion of the
--   issues.  Unfortunately, none of the three alternatives proposed there
--   works in my case: I need libiconv for other MacPorts packages, linking
--   against the system libiconv doesn't work, and the version of GHC in
--   MacPorts is ancient.
--
--   (2) The primary application in which I wanted to use Unicode requires that
--   the combining characters appear in a particular order, but Unicode's
--   notion of normalized order doesn't actually enforce this.  In particular,
--   attempting to normalize @\"\\x3b1\\x300\\x314\"@ into NFD form returns
--   @\"\\x3b1\\x300\\x314\"@, but normalizing @\"\\x3b1\\x314\\x300\"@ returns
--   @\"\\x3b1\\x314\\x300\"@, which means that I can't rely on the order of
--   the combining characters.  NFC normalization has similar problems:
--   converting @\"\\x3b1\\x300\\x314\"@ (lowercase alpha, combining grave,
--   combining rough breathing) to NFC gives @\"\\x1f70\\x314\"@ (precomposed
--   alpha-grave, combining rough-breathing), and converting
--   @\"\\x3b1\\x314\\x300\"@ gives @\"\\x1f03\"@ (precomposed
--   alpha-rough-grave).
module Data.Greek.Normalize(normalize) where

import Data.Greek.Private.Normalize
