{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-|
Definition of Widget
-}
module Graphics.UI.Widget.Internal.Widget where

import Control.Lens
import Control.Monad.Trans
import Data.Extensible
import Data.Extensible.Internal
import Data.Proxy
import GHC.OverloadedLabels
import Unsafe.Coerce

-- | Module of widget
newtype Module op = Module { runModule :: forall (m :: * -> *) (val :: * -> *). (Functor m, Functor val) => op m val -> m (val (Module op)) }

-- | Widget Type
newtype Widget ops = Widget { runWidget :: Module :* ops }

-- | Call a method of a widget
call :: (k ∈ xs, Functor m, Functor val) => Widget xs -> k m val -> m (val (Widget xs))
call w op = fmap (\t -> Widget $ runWidget w & piece .~ t) <$> runModule (runWidget w ^. piece) op

callAt :: (Functor m, Functor val) => Widget xs -> Membership xs k -> k m val -> m (val (Widget xs))
callAt w mem op = fmap (\t -> Widget $ runWidget w & pieceAt mem .~ t) <$> runModule (runWidget w ^. pieceAt mem) op

-- | Turn a operator into Getter
_Op :: (k ∈ xs, Functor m, Functor val) => k m val -> Getter (Widget xs) (m (val (Widget xs)))
_Op opr = _OpAt membership opr

_OpAt :: (Functor m, Functor val) => Membership xs k -> k m val -> Getter (Widget xs) (m (val (Widget xs)))
_OpAt mem op = to (\w -> callAt w mem op)

type family MapFst xs where
  MapFst '[] = '[]
  MapFst ((k >: v) : xs) = k : MapFst xs

type family MapSnd xs where
  MapSnd '[] = '[]
  MapSnd ((k >: v) : xs) = v : MapSnd xs

data Widx ops
  = Widx
  { _keys :: Proxy :* (MapFst ops)
  , _widget :: Widget (MapSnd ops) 
  }

makeLenses ''Widx

mkWidx :: (Generate (MapFst ops)) => Widget (MapSnd ops) -> Widx ops
mkWidx = Widx (hrepeat Proxy)

keyOf :: proxy1 xs -> proxy2 x -> Membership xs x -> Membership ys y
keyOf _ _ = unsafeCoerce

_keyPair :: (x ∈ xs, Functor m, Functor val) => Proxy x -> y m val -> Getter (Widget ys, Proxy :* xs) (m (val (Widget ys)))
_keyPair px op = to $ \(w,keys) -> callAt w (keyOf keys px membership) op

_key :: (x ∈ MapFst ops, Functor m, Functor val) => Proxy x -> y m val -> Getter (Widx ops) (m (val (Widx ops)))
_key px op = to $ \widx -> fmap (\t -> widx & widget .~ t) <$> (widx^.widget, widx^.keys) ^. _keyPair px op

-- | Represents that operator will return the widget itself
type Self = Identity

-- | Represents that operator will return a value discarding the widget
type Value = Const

