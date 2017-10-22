{-# LANGUAGE UndecidableInstances #-}
{-|
Selector is a widget that user can select one or multiple items with a pointer
-}
module Graphics.UI.Widget.Selector
  (

  -- * Widget
    wSelector
  , wSelectLayer

  -- * Method
  , Op'Selector
  , Op'SelectLayer

  -- * Operator
  , op'renderSelector
  , op'getSelecting
  , op'getPointer
  , op'getLabels
  , op'setLabels

  , Op'GetSelecting(..)
  , Op'GetPointer(..)
  , Op'GetLabels(..)

  -- * Config Parameter
  , SelectorRenderConfig
  ) where

import qualified Data.Map as M
import Data.Maybe
import Data.List
import Data.Reflection
import Data.Extensible
import Data.Color.Names
import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.Trans
import Linear.V2
import Data.Scoped
import Data.Widget.Stylesheet
import Graphics.UI.Widget.Renderer
import Graphics.UI.Widget.Util
import Graphics.UI.Widget.Core
import Graphics.UI.Widget.Layer

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
  , "location" >: V2 Int
  ]

makeOp "RenderSelector" [t| (SelectorRenderConfig -> RenderM ()) -> _ Value RenderM () |]
makeOp "GetSelecting" [t| _ Value Identity [Int] |]
makeOp "GetPointer" [t| _ Value Identity (Maybe Int) |]
makeOp "GetLabels" [t| _ Value Identity [String] |]
makeOp "SetLabels" [t| [String] -> Maybe Int -> _ Self Identity () |]

-- | Method of 'wSelector'
type Op'Selector =
  [ Op'Reset ()
  , Op'Render
  , Op'RenderSelector
  , Op'Run
  , Op'HandleEvent
  , Op'Switch
  , Op'GetSelecting
  , Op'GetPointer
  , Op'GetLabels
  , Op'SetLabels
  ]

-- とりあえずrenderDropDownの実装
-- 必要があればoverrideする

type instance Config "selector"
  = Record '[]

{-
  def =
    #labels @= []
    <: #selectNum @= 1
    <: #pager @= Nothing
    <: emptyRecord
-}

-- | Selector widget
--
-- Methods
--
-- * 'op'reset' Reset operator
-- * 'op'render' Render operator; dropdown style
-- * 'op'renderSelector' Render operator with custom renderer
-- * 'op'run' Run operator
-- * 'op'handleEvent' Keyevent handle operator
-- * 'op'switch' Check if this widget is alive/dead
-- * 'op'getSelecting' Get selecting indices as integer list
-- * 'op'getPointer' Get the current pointer index
-- * 'op'getLabels' Get the current labels
-- * 'op'setLabels' Set labels
--
wSelector :: Given StyleSheet => Config "selector" -> NamedWidget Op'Selector
wSelector cfg = wNamed (mempty </> WId "selector") $ go $ new where
  pointerFromPagerStyle labels pager = maybe (rangeScope labels (length labels - 1)) (rangeScope labels) pager

  def =
    #labels @= []
    <: #selectNum @= 1
    <: #pager @= Nothing
    <: emptyRecord

  new :: Selector
  new = Selector
    (zip [0..] $ def ^. #labels)
    (pointerFromPagerStyle (def ^. #labels) (def ^. #pager))
    (def ^. #pager)
    (def ^. #selectNum)
    []
    False

  go :: Selector -> Widget Op'Selector
  go sel = Widget $
    (\(Op'Reset _) -> continue $ go $ reset sel)
    @> (\(Op'Render _) -> lift $ renderDropdown sel 0) --(getLocation wix))
    @> (\(Op'RenderSelector rend) -> lift $ render sel rend)
    @> (\Op'Run -> continueM $ fmap go $ return sel)
    @> (\(Op'HandleEvent keys) -> continueM $ fmap go $ handler keys sel)
    @> (\Op'Switch -> (if sel^.isFinished then freeze' else continue) $ go sel)
    @> (\Op'GetSelecting -> finish $ sel^.selecting)
    @> (\Op'GetPointer -> finish $ sel^.pointer <&> (^.scoped))
    @> (\Op'GetLabels -> finish $ fmap snd $ sel^.labels)
    @> (\(Op'SetLabels ls pager) -> continue $ go $ sel & labels .~ zip [0..] ls & pointer .~ pointerFromPagerStyle ls pager & pagerStyle .~ pager)
    @> emptyUnion

  reset :: Selector -> Selector
  reset sel = sel & pointer._Just %~ adjustTo0 & selecting .~ [] & isFinished .~ False

  render :: Selector -> (SelectorRenderConfig -> RenderM ()) -> RenderM ()
  render sel rendItem = do
    forM_ (zip [0..] $ fmap ((sel^.labels) !!) $ maybe [0..length (sel^.labels)-1] rangeOf (sel^.pointer)) $ \(i,label) ->
      rendItem
        $ #label @= (snd label)
        <: #index @= i
        <: #isSelected @= (i `elem` (sel^.selecting))
        <: #isFocused @= (Just (fst label) == ((sel^.pointer) <&> (^.scoped)))
        <: #location @= 0 --getLocation wix
        <: emptyRecord

  renderDropdown :: Selector -> V2 Int -> RenderM ()
  renderDropdown sel p = do
    render sel $ \rcfg -> do
      when (rcfg ^. #isFocused) $ do
        translate (p + V2 20 (20 + 30 * (rcfg ^. #index))) $ shaded black $ colored white $ text "▶"

      let color = if rcfg ^. #isSelected then red else white
      translate (p + V2 (20+20) (20 + 30 * (rcfg ^. #index))) $ shaded black $ colored color $ text $ rcfg ^. #label

  handler :: M.Map Keycode Int -> Selector -> RenderM Selector
  handler keys sel
    | keyjudge (keys M.! _KeyUp) = return $ sel & pointer._Just %~ back
    | keyjudge (keys M.! _KeyDown) = return $ sel & pointer._Just %~ forward
    | keys M.! _KeycodeZ == 1 && not (sel^.isFinished) && (isJust $ sel^.pointer) = do
        let p = fst $ (sel^.labels) !! (sel^.pointer^?!_Just^.scoped)
        if p `elem` sel^.selecting
          then return $ sel & selecting %~ delete p
          else return $ sel
               & selecting %~ (p :)
               & isFinished .~ (length (sel^.selecting) + 1 == sel^.selectNum)
    | otherwise = return sel
    where
      keyjudge n | n < 100 = n `mod` 20 == 1
      keyjudge n = n `mod` 7 == 1

-- | Method of 'wSelectLayer'
type Op'SelectLayer =
  [ Op'Reset ()
  , Op'Render
  , Op'Run
  , Op'HandleEvent
  , Op'Switch
  , Op'GetSelecting
  , Op'GetPointer
  , Op'GetLabels
  , Op'SetLabels
  ]

type SelectLayer = (NamedWidget Op'Layer, NamedWidget Op'Layer, NamedWidget Op'Selector)

type instance Config "select_layer"
  = Record
  [ "windowTexture" >: Texture
  , "cursorTexture" >: Texture
  , "size" >: V2 Int
  ]

-- | Selector widget with window layer and cursor layer
--
-- Methods
--
-- * 'op'reset' Reset operator
-- * 'op'render' Render operator; dropdown style
-- * 'op'run' Run operator
-- * 'op'handleEvent' Keyevent handle operator
-- * 'op'switch' Check if this widget is alive/dead
-- * 'op'getSelecting' Get selecting indices as integer list
-- * 'op'getPointer' Get the current pointer index
-- * 'op'getLabels' Get the current labels
-- * 'op'setLabels' Set labels
--
wSelectLayer :: Given StyleSheet => Config "select_layer" -> RenderM (Widget Op'SelectLayer)
wSelectLayer cfg = go <$> new where
  def =
    #labels @= []
    <: #selectNum @= 1
    <: #pager @= Nothing
    <: emptyRecord
  
  new :: RenderM SelectLayer
  new = liftM3 (,,)
    (wLayer (#windowTexture @= cfg ^. #windowTexture <: #size @= cfg ^. #size <: emptyRecord))
    (wLayer (#windowTexture @= cfg ^. #cursorTexture <: #size @= V2 (cfg ^. #size ^. _x - 20) 30 <: emptyRecord))
    (return $ wSelector emptyRecord)

  go :: SelectLayer -> Widget Op'SelectLayer
  go w = Widget $
    (\(Op'Reset args) -> continue $ go $ w & _3 ^%~ op'reset args)
    @> (\(Op'Render _) -> lift $ render 0 w) -- render (getLocation wix) w)
    @> (\Op'Run -> continue $ go w)
    @> (\(Op'HandleEvent keys) -> continueM $ fmap go $ (\x -> w & _3 .~ x) <$> (w^._3^.op'handleEvent keys))
    @> (\Op'Switch -> (if op'isFreeze (w^._3) op'switch then freeze' else continue) $ go w)
    @> (\Op'GetSelecting -> finish $ w^._3^.op'getSelecting)
    @> (\Op'GetPointer -> finish $ w^._3^.op'getPointer)
    @> (\Op'GetLabels -> finish $ w^._3^.op'getLabels)
    @> (\(Op'SetLabels t pager) -> continue $ go $ w & _3 ^%~ op'setLabels t pager)
    @> emptyUnion

  render :: V2 Int -> SelectLayer -> RenderM ()
  render v sel = do
    sel^._1^.op'render
    (sel^._3^.) $ op'renderSelector $ \rcfg -> do
      when (rcfg ^. #isFocused) $ do
        sel^._2^.op'renderAt ((rcfg ^. #location) + V2 0 (30 * (rcfg ^. #index))) 1.0

      let color = if rcfg ^. #isSelected then red else white
      translate ((rcfg ^. #location) + V2 10 (30 * (rcfg ^. #index))) $ shaded black $ colored color $ text $ rcfg ^. #label
      
