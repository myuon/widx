module Graphics.UI.Widget.SelectLayer
  where

import Control.Lens
import Control.Monad
import Data.Color.Names
import Data.Extensible
import Data.Extensible.Internal
import Data.Functor
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

wSelectLayer :: Config "select_layer" -> RenderM (Widget [Op'Layer, Op'Layer, Op'Selector])
wSelectLayer cfg = do
  mwin <- mLayer (shrink cfg)
  mcur <- mLayer $ #windowTexture @= (cfg ^. #cursorTexture) <: #size @= (V2 (cfg ^. #size ^. _x - 20) 30) <: emptyRecord
  let msel = mSelector (shrink cfg)

  return $ Widget $ mwin <: mcur <: msel <: nil

op'render :: V2 Int -> Getter (Widget [Op'Layer, Op'Layer, Op'Selector]) (RenderM (Value () (Widget [Op'Layer, Op'Layer, Op'Selector])))
op'render v = to $ \w -> do
  w ^. _OpAt here (Layer.Render v 1.0)
  let renderer rcfg = do
        when (rcfg ^. #isFocused) $ do
          w ^. _OpAt (navNext here) (Layer.Render (V2 0 (30 * (rcfg ^. #index))) 1.0)
          return ()
  
        let color = if rcfg ^. #isSelected then red else white
        translate (V2 10 (30 * (rcfg ^. #index))) $ shaded black $ colored color $ text $ rcfg ^. #label
  w ^. _Op (Selector.RenderSelector renderer)


