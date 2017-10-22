{-# LANGUAGE PolyKinds #-}
{-|
Utility module
-}
module Graphics.UI.Widget.Util
  (
  -- * Lenses
  functorial
  , monadic

  , (^%~), (^%%~), (^%=), (^%%=)

  -- * Extensible
  , hmerge
  , hmergeAssoc
  ) where

import Control.Monad
import Control.Monad.State
import Control.Lens
import Data.Extensible
import Linear.V2
import Linear.V4

-- | functorial lifting
functorial :: Functor f => Getter a b -> Getter (f a) (f b)
functorial l = to $ fmap (^.l)

-- | monadic lifting
monadic :: Monad m => Lens' a b -> Lens' (m a) (m b)
monadic l = lens (^. functorial l) (liftM2 (\a b -> a & l .~ b))

infixr 4 ^%~
-- | > s ^%~ f = s %~ (^. f)
(^%~) :: Lens' s a -> (Getter a a) -> s -> s
s ^%~ f = s %~ (^. f)

infixr 4 ^%%~
-- | > s ^%%~ f = s %%~ (^. f)
(^%%~) :: Functor m => Lens' s a -> (Getter a (m a)) -> s -> m s
s ^%%~ f = s %%~ (^. f)

infixr 4 ^%=
-- | > s ^%= f = s %= (^. f)
(^%=) :: MonadState s m => Lens' s a -> (Getter a a) -> m ()
s ^%= f = s %= (^. f)

infixr 4 ^%%=
-- | > s ^%%= f = use s >>= \s' -> s <~ s' ^. f
(^%%=) :: MonadState s m => Lens' s a -> (Getter a (m a)) -> m ()
s ^%%= f = do
  x <- use s
  s <~ x ^. f

-- | Merges two products when first one is included in second one
hmerge :: (xs ⊆ ys, Wrapper h) => h :* xs -> h :* ys -> h :* ys
hmerge hx hy = hfoldrWithIndex (\xin x hy -> hy & itemAt (hlookup xin inclusion) .~ x^._Wrapper) hy hx

-- | Merges two associated products when first one is included in second one
hmergeAssoc :: (IncludeAssoc ys xs, Wrapper h) => h :* xs -> h :* ys -> h :* ys
hmergeAssoc hx hy = hfoldrWithIndex (\xin x hy -> hy & itemAt (hlookup xin inclusionAssoc) .~ x^._Wrapper) hy hx
