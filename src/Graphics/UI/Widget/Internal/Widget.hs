{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyCase #-}
{-|
Definition of Widget
-}
module Graphics.UI.Widget.Internal.Widget where

import Control.Lens
import Control.Monad.Trans
import Data.Extensible

-- | Polymorphic function type
type (~>) f g = forall x. f x -> g x

-- | Module of widget
newtype Module op = Module { runModule :: forall m val. op m val -> m (val (Module op)) }

-- | Widget Type
newtype Widget ops = Widget { runWidget :: Module :* ops }

-- | Call a method of a widget
call :: (k ∈ xs, Functor m, Functor val) => Widget xs -> k m val -> m (val (Widget xs))
call w op = fmap (\t -> Widget $ runWidget w & piece .~ t) <$> runModule (runWidget w ^. piece) op

-- | Turn a operator into Getter
_Op :: (k ∈ xs, Functor m, Functor val) => k m val -> Getter (Widget xs) (m (val (Widget xs)))
_Op opr = to (\w -> call w opr)

-- | Represents that operator will return the widget itself
type Self = Identity

-- | Represents that operator will return a value discarding the widget
type Value = Const

{-
-- | @br@ can continue with the current widget state
class NodeW br where
  continue :: Monad m => Widget xs -> br (Widget xs) m a
  continueM :: Functor m => m (Widget xs) -> br (Widget xs) m a

  _self :: (k ∈ xs, TransBifunctor br m, Functor m) => k br m () -> Getter (Widget xs) (m (Widget xs))

-- | @br@ can finish with given value
class LeafW br where
  finish :: Monad m => model -> br (Widget xs) m model
  finishM :: Functor m => m model -> br (Widget xs) m model

  _value :: (k ∈ xs, Functor m) => k br m a -> Getter (Widget xs) (m a)

instance NodeW Self where
  continue = Self . return
  continueM = Self

  _self opr = to $ \w -> runSelf $ w `call` opr

instance LeafW Value where
  finish = Value . return
  finishM = Value

  _value opr = to $ \w -> getValue $ w `call` opr

-- | '_self' with Identity monad
_self' :: (k ∈ xs, TransBifunctor br Identity, NodeW br) => k br Identity () -> Getter (Widget xs) (Widget xs)
_self' opr = _self opr . to runIdentity

-- | '_value'' with Identity monad
_value' :: (k ∈ xs, LeafW br) => k br Identity a -> Getter (Widget xs) a
_value' opr = _value opr . to runIdentity
-}

