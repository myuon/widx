{-|
A widget with layer
-}
module Graphics.UI.Widget.Layer
  (
  -- * Widget
    wLayer
  , wDelay

  -- * Method
  , Op'Layer
  , Op'Delay

  -- * Operator
  , op'renderLayer
  , op'getCounter
  ) where

import qualified Data.Map as M
import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.Reader
import Data.Reflection
import Data.Extensible
import Linear.V2
import Data.Widget.Stylesheet
import Graphics.UI.Widget.Class
import Graphics.UI.Widget.Core

-- | Layer datatype
data Layer m
  = Layer
  { _size :: V2 Int
  , _texture :: Texture m  -- ^ Texture type
  }

makeLenses ''Layer

-- | Method of 'wLayer'
type Op'Layer =
  '[ Op'Render
  ]

type instance Config "layer" m
  = Record
  [ "windowTexture" >: Texture m
  , "size" >: V2 Int
  ]

-- | Layer widget
--
-- Methods
--
-- * 'op'render' Render operator
--
wLayer :: (Given StyleSheet, Renderer m) => Config "layer" m -> m (NamedWidget Op'Layer)
wLayer cfg = wNamed mempty . go <$> new where
  new = return $ Layer (cfg ^. #size) (cfg ^. #windowTexture)

  go :: Renderer m => Layer m -> Widget Op'Layer
  go layer = Widget $
    (\(Op'Render a) -> finishM $ RenderM $ alpha a $ picture (layer^.texture))
    @> emptyUnion

-- | Layer widget and its texture will be loaded using given filepath
wLayerFilePath :: Given StyleSheet => FilePath -> Config "layer" m -> m (NamedWidget Op'Layer)
wLayerFilePath path cfg = do
  rend <- use renderer
  wLayer cfg

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
-- == Methods
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
