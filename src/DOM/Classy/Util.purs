module DOM.Classy.Util where

import Prelude

import Control.Monad.Except (runExcept)

import Data.Either (either)
import Data.Foreign (F, Foreign)
import Data.Maybe (Maybe(..))

import Unsafe.Coerce (unsafeCoerce)

fromAny :: forall n a. (Foreign -> F n) -> a -> Maybe n
fromAny f = either (const Nothing) Just <<< runExcept <<< unsafeCoerce f
