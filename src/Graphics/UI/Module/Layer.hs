{-|
Layer module
-}
module Graphics.UI.Module.Layer
  (
  -- * Widget
    mLayer

  -- * Method
  , Op'Layer(..)

  ) where

import qualified Data.Map as M
import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.Reader
import Data.Reflection
import Data.Extensible
import Linear.V2
import Data.Widget.Stylesheet
import Graphics.UI.Widget.Renderer
import Graphics.UI.Widget.Core

-- | Layer datatype
data Layer
  = Layer
  { _size :: V2 Int
  , _texture :: Texture
  }

makeLenses ''Layer

-- | Method of 'mLayer'
data Op'Layer m val where
  Op'Render :: V2 Int -> Double -> Op'Layer RenderM (Value ())

type instance Config "layer"
  = Record
  [ "windowTexture" >: Texture
  , "size" >: V2 Int
  ]

-- | Layer module
--
-- Methods
--
-- * 'op'render' Render operator
--
mLayer :: Config "layer" -> RenderM (Module Op'Layer)
mLayer cfg = go <$> new where
  new = return $ Layer (cfg ^. #size) (cfg ^. #windowTexture)
  
  go :: Layer -> Module Op'Layer
  go layer = Module $ \op -> case op of
    Op'Render v a -> valueM $ alpha a $ translate v $ picture $ layer^.texture

