{-|
Interfaces and classes for widget
-}
module Graphics.UI.Widget.Class where

import Control.Monad.IO.Class

-- | Render objects
class Renderer m where
  renders :: t -> m ()

-- | Render monad, which is base monad for widget
newtype RenderM a = RenderM { runRenderM :: forall m. (Renderer m, MonadIO m, Monad m) => m a }

instance Functor RenderM where
  fmap f ma = RenderM $ f <$> runRenderM ma

