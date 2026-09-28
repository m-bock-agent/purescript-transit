-- | Applying a message, and being handed only the states it can
-- | produce.
module Transit.Apply
  ( class Applying
  , mkApply
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Variant (Variant, contract, expand, inj)
import Data.Variant.Internal (class Contractable)
import Prim.Row as Row
import Transit.Class.Landings (class Landings)
import Type.Proxy (Proxy(..))

-- | What `mkApply` needs, named once so a caller writes this and
-- | nothing else: the walk over the specification, the narrowing and
-- | the row arithmetic are all behind it.
class
  Applying :: forall k. k -> Symbol -> Row Type -> Row Type -> Constraint
class
  Applying spec msg (rowState :: Row Type) (landings :: Row Type)
  | spec msg rowState -> landings

instance
  ( Landings spec msg rowState landings
  , Contractable rowState landings
  , Row.Union landings (stuck :: Variant rowState) (stuck :: Variant rowState | landings)
  , Row.Lacks "stuck" landings
  ) =>
  Applying spec msg rowState landings

-- | Turns a machine's `apply` - a message in, the state it produced
-- | out - into one that answers with the states this message can
-- | produce, and `stuck` with the whole state when it produced none of
-- | them.
-- |
-- | ```purescript
-- | landed <- mkApply @Spec @"Asked" api.applyMsg msg
-- | ```
mkApply
  :: forall @spec @msg m rowMsg rowState landings
   . Functor m
  => Applying spec msg rowState landings
  => Contractable rowState landings
  => Row.Union landings (stuck :: Variant rowState) (stuck :: Variant rowState | landings)
  => (Variant rowMsg -> m (Variant rowState))
  -> Variant rowMsg
  -> m (Variant (stuck :: Variant rowState | landings))
mkApply apply msg =
  apply msg <#> \produced ->
    case contract produced :: Maybe (Variant landings) of
      Just one -> expand one
      Nothing -> inj (Proxy @"stuck") produced
