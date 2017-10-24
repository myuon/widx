module Graphics.UI.Widget.SelectLayer
  (
  -- * Module
    SelectLayer
  , wSelectLayer

  -- * Operator
  , op'render
  )
  where

import Control.Lens hiding (Field2, (:>))
import Control.Monad
import Data.Color.Names
import Data.Extensible
import Data.Functor
import Data.Proxy
import Graphics.UI.Module.Layer as Layer
import Graphics.UI.Module.Selector as Selector
import Graphics.UI.Widget.Core
import Graphics.UI.Widget.Renderer
import Linear.V2

type instance Config "select_layer"
  = Record
  [ "windowTexture" >: Texture
  , "cursorTexture" >: Texture
  , "size" >: V2 Int
  , "labels" >: [String]
  , "selectNum" >: Int
  , "pager" >: Maybe Int
  ]

newtype SelectLayer = SelectLayer {
  getSelectLayer :: Widx ["window" >: Op'Layer, "cursor" >: Op'Layer, "selector" >: Op'Selector] }

wSelectLayer :: Config "select_layer" -> RenderM SelectLayer
wSelectLayer cfg = do
  mwin <- mLayer (shrink cfg)
  mcur <- mLayer $ #windowTexture @= (cfg ^. #cursorTexture) <: #size @= (V2 (cfg ^. #size ^. _x - 20) 30) <: emptyRecord
  let msel = mSelector (shrink cfg)

  return $ SelectLayer $ mkWidx (Widget $ mwin <: mcur <: msel <: nil)

op'render :: V2 Int -> Getter SelectLayer (RenderM (Value () SelectLayer))
op'render v = to $ \(SelectLayer w) -> valued <$> do
  w ^. _key #window (Layer.Render v 1.0)
  let renderer rcfg = do
        when (rcfg ^. #isFocused) $ do
          w ^. _key #cursor (Layer.Render (V2 0 (30 * (rcfg ^. #index))) 1.0)
          return ()
  
        let color = if rcfg ^. #isSelected then red else white
        translate (V2 10 (30 * (rcfg ^. #index))) $ shaded black $ colored color $ text $ rcfg ^. #label
  w ^. _key #selector (Selector.RenderSelector renderer)

