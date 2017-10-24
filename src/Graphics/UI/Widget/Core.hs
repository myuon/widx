{-|
Core of 'Widget'
-}
module Graphics.UI.Widget.Core
  (
  -- * Config
  Config

  -- * Module combinator
  , selfM
  , valueM
  , valued
  , Op'Render(..)

  -- * Re-exports
  , module Graphics.UI.Widget.Internal.Widget
  ) where

import Control.Lens
import Data.Extensible
import GHC.TypeLits
import Linear.V2
import Graphics.UI.Widget.Renderer
import Graphics.UI.Widget.Internal.Widget

selfM :: Monad m => m w -> m (Self w)
selfM = fmap Identity

valueM :: Monad m => m a -> m (Value a w)
valueM = fmap Const

valued :: Value a w -> Value a z
valued v = Const $ getConst v

data Op'Render m val where
  Op'Render :: V2 Int -> Double -> Op'Render RenderM (Value ())

type family Config (k :: Symbol) :: *

