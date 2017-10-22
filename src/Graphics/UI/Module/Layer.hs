{-|
Layer module
-}
module Graphics.UI.Module.Layer
  (
  -- * Widget
    mLayer

  -- * Method
  , Op'Layer

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
  , _texture :: Texture  -- ^ Texture type
  }

makeLenses ''Layer

-- | Method of 'wLayer'
data Op'Layer m val where
  Op'Render :: V2 Int -> Double -> Op'Layer RenderM (Value ())

type instance Config "layer"
  = Record
  [ "windowTexture" >: Texture
  , "size" >: V2 Int
  ]

-- | Layer widget
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

{-
wLayer cfg = wNamed mempty . go <$> new where
  new = return $ Layer (cfg ^. #size) (cfg ^. #windowTexture)

  go :: Layer -> Widget Op'Layer
  go layer = Widget $
    (\(Op'Render a) -> finishM $ alpha a $ picture (layer^.texture))
    @> (\(Op'RenderAt v a) -> finishM $ alpha a $ translate v $ picture (layer^.texture))
    @> emptyUnion

-- | Layer widget and its texture will be loaded using given filepath
wLayerFilePath :: Given StyleSheet => FilePath -> V2 Int -> RenderM (NamedWidget Op'Layer)
wLayerFilePath path v = do
  t <- loadTexture path
  wLayer (#windowTexture @= t <: #size @= v <: emptyRecord)


-- Delayed
data Delay
  = Delay
  { _counter :: Int
  , _delayCount :: Int
  }
  deriving (Eq, Show)

makeLenses ''Delay

makeOp "GetCounter" [t| _ Value Identity Int |]

-- | Method of 'wDelay'
type Op'Delay =
  [ Op'Reset ()
  , Op'Run
  , Op'GetCounter
  ]

-- | Delay widget, this widget has a looped counter
--
-- Methods
--
-- * 'op'reset' Reset operator
-- * 'op'run' Run operator, this increments the counter
-- * 'op'getCounter' Get the current counter
--
wDelay :: Int -> Widget Op'Delay
wDelay = \n -> go (Delay 0 n) where
  go :: Delay -> Widget Op'Delay
  go delay = Widget $
    (\(Op'Reset _) -> continue $ go $ delay & counter .~ 0)
    @> (\Op'Run -> continueM $ fmap go $ run delay)
    @> (\Op'GetCounter -> finish $ delay^.counter)
    @> emptyUnion

  run delay = return $ delay & counter %~ (`mod` delay^.delayCount) . (+1)
-}

