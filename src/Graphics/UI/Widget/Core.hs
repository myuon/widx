{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-|
Core of 'Widget'
-}
module Graphics.UI.Widget.Core
  (

  -- * Widget functions
    override

  , runSwitch
  , runSwitchM
  , op'isFreeze

  -- * Config
  , Config

  -- * Common operators
  , op'render
  , op'renderAlpha
  , op'run
  , op'reset
  , op'handleEvent
  , op'switch

  -- * Operator types
  , Op'Render(..)
  , Op'Run(..)
  , Op'Reset(..)
  , Op'HandleEvent(..)
  , Op'Switch(..)
  
  -- * Re-exports
  , type (++)
  , type (∈)
  , module Graphics.UI.Widget.Internal.Widget
  , module Graphics.UI.Widget.Internal.TH
  , module Graphics.UI.Widget.Internal.Named
  , module Graphics.UI.Widget.Internal.Freeze
  ) where

import Control.Lens
import qualified Data.Map as M
import Data.Functor.Sum
import Data.Reflection
import Data.Extensible
import Data.Proxy
import GHC.TypeLits
import Data.Widget.Stylesheet
import Graphics.UI.Widget.Renderer
import Graphics.UI.Widget.Internal.Widget
import Graphics.UI.Widget.Internal.TH
import Graphics.UI.Widget.Internal.Named
import Graphics.UI.Widget.Internal.Freeze

makeOp "Run" [t| _ Self RenderM () |]
makeOp "HandleEvent" [t| M.Map Keycode Int -> _ Self RenderM () |]
makeOp "Switch" [t| _ FreezeT Identity () |]

data Op'Render br m r where
  Op'Render :: Double -> Op'Render Value RenderM ()

data Op'Reset arg br m r where
  Op'Reset :: arg -> Op'Reset arg Self Identity a

op'renderAlpha :: (KnownName xs, Op'Render ∈ xs) => Double -> Getter (Widget xs) (RenderM ())
op'renderAlpha d = to $ \w -> w ^. _value (Op'Render d)

-- | @op'render = 'op'renderAlpha' 1.0@
op'render :: (Given StyleSheet, KnownName xs, Op'Render ∈ xs) => Getter (Widget xs) (RenderM ())
op'render = op'renderAlpha 1.0

-- | @op'reset = '_self'' . 'Op'Reset'@
op'reset :: (Op'Reset arg ∈ xs) => arg -> Getter (Widget xs) (Widget xs)
op'reset = _self' . Op'Reset

-- | Recursive extension
override :: (Widget old -> Widget new) -> Widget old -> (forall br m. Union new br m ~> (br (Widget new) m `Sum` Union old br m)) -> Widget new
override updater wx fu = Widget $ elim id (bimapT updater id . runWidget wx) . fu where
  elim :: (f ~> r) -> (g ~> r) -> (f `Sum` g ~> r)
  elim f g x = case x of
    InL a -> f a
    InR a -> g a

-- | Combinator for running a widget with switching method like `op'switch`
runSwitch :: Widget xs -> Getter (Widget xs) (FreezeT (Widget xs) Identity a) -> (Freeze (Widget xs) a -> r) -> r
runSwitch w op k = k $ runIdentity $ runFreezeT (w ^. op)

-- | 'runSwitch' with monad
runSwitchM :: (k ∈ xs, Monad m) => Widget xs -> k FreezeT m a -> (Freeze (Widget xs) a -> m r) -> m r
runSwitchM w op k = runFreezeT (w `call` op) >>= k

-- | Is a widget frozen?
op'isFreeze :: Widget xs -> Getter (Widget xs) (FreezeT (Widget xs) Identity a) -> Bool 
op'isFreeze w op = runSwitch w op isFreeze

type family Config (k :: Symbol) :: *

