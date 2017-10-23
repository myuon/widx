{-|
Selector is a module that user can select one or multiple items with a pointer
-}
module Graphics.UI.Module.Selector
  (
  -- * Module
    mSelector

  -- * Method
  , Op'Selector(..)

  -- * Config
  , SelectorRenderConfig
  ) where

import Control.Lens
import Control.Monad
import Data.Scoped
import Data.Color.Names
import Data.Extensible
import Linear.V2
import Graphics.UI.Widget.Core
import Graphics.UI.Widget.Renderer

data Selector
  = Selector
  { _labels :: [(Int,String)]
  , _pointer :: Maybe (Scoped Int)
  , _pagerStyle :: Maybe Int
  , _selectNum :: Int
  , _selecting :: [Int]
  , _isFinished :: Bool
  }

makeLenses ''Selector

-- | Custom Renderer
type SelectorRenderConfig = Record
  [ "label" >: String
  , "index" >: Int
  , "isSelected" >: Bool
  , "isFocused" >: Bool
  ]

-- | Method of 'mSelector'
data Op'Selector m val where
  Render :: (SelectorRenderConfig -> RenderM ()) -> Op'Selector RenderM (Value ())
  RenderDropdown :: V2 Int -> Op'Selector RenderM (Value ())
  GetSelecting :: Op'Selector Identity (Value [Int])
  GetPointer :: Op'Selector Identity (Value (Maybe Int))
  GetLabels :: Op'Selector Identity (Value [String])
  SetLabels :: [String] -> Maybe Int -> Op'Selector Identity Self

type instance Config "selector" =
  Record
  [ "labels" >: [String]
  , "selectNum" >: Int
  , "pager" >: Maybe Int
  ]

-- | Selector module
--
-- Methods
--
-- * 'op'render' Render a selector by given custom renderer
-- * 'op'renderDropdown' Render a selector as dropdown style
-- * 'op'getSelecting' Get selecting indices as integer list
-- * 'op'getPointer' Get the current pointer index
-- * 'op'getLabels' Get the current labels
-- * 'op'setLabels' Set labels
--
mSelector :: Config "selector" -> Module Op'Selector
mSelector cfg = go new where
  pointerFromPagerStyle labels pager = maybe (rangeScope labels (length labels - 1)) (rangeScope labels) pager

  new = Selector
    (zip [0..] $ cfg ^. #labels)
    (pointerFromPagerStyle (cfg ^. #labels) (cfg ^. #pager))
    (cfg ^. #pager)
    (cfg ^. #selectNum)
    []
    False

  render :: Selector -> (SelectorRenderConfig -> RenderM ()) -> RenderM ()
  render sel rendItem = do
    forM_ (zip [0..] $ fmap ((sel^.labels) !!) $ maybe [0..length (sel^.labels)-1] rangeOf (sel^.pointer)) $ \(i,label) ->
      rendItem
        $ #label @= (snd label)
        <: #index @= i
        <: #isSelected @= (i `elem` (sel^.selecting))
        <: #isFocused @= (Just (fst label) == ((sel^.pointer) <&> (^.scoped)))
        <: emptyRecord

  renderDropdown :: Selector -> V2 Int -> RenderM ()
  renderDropdown sel p = do
    render sel $ \rcfg -> do
      when (rcfg ^. #isFocused) $ translate (p + V2 20 (20 + 30 * (rcfg ^. #index))) $ shaded black $ colored white $ text "▶"

      let color = if rcfg ^. #isSelected then red else white
      translate (p + V2 (20+20) (20 + 30 * (rcfg ^. #index))) $ shaded black $ colored color $ text $ rcfg ^. #label

  go :: Selector -> Module Op'Selector
  go model = Module $ \case
    Render renderer -> valueM $ render model renderer
    RenderDropdown v -> valueM $ renderDropdown model v
    GetSelecting -> valueM $ Identity (model ^. selecting)
    GetPointer -> valueM $ Identity $ (model^.pointer) <&> (^.scoped)
    GetLabels -> valueM $ Identity (fmap snd $ model ^. labels)
    SetLabels lbls pager -> selfM $ Identity $ go $ model & labels .~ zip [0..] lbls & pointer .~ pointerFromPagerStyle lbls pager & pagerStyle .~ pager

