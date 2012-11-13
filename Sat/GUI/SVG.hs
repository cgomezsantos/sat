module Sat.GUI.SVG ( generateSVG
                   , generateSVGFromEB
                   -- *
                   , path
                   -- *
                   , blue
                   , green
                   , red
                   -- *
                   , big
                   , normal
                   , small
                   -- *
                   , blackStroke
                   , color
                   , colorAlpha
                   -- *
                   , intValue
                   , colorValue
                   , pairCommaValue
                   , mconcat
                   , (<>)
                   -- *
                   , DrawMain
                   , DrawMod
                   , Predicate
                   ) where

import Numeric(showHex)
import qualified Data.Map as M
import Data.Maybe(fromJust)
import Data.Function(on)
import Data.Monoid (Monoid,mappend)

import Control.Monad.Reader

import qualified Data.Colour.Names as Names
import Data.Colour(Colour)
import Data.Colour.SRGB(sRGB24show)
import Graphics.Rendering.Cairo.SVG

import Text.Blaze (toValue)
import Text.Blaze.Renderer.String (renderMarkup)
import Text.Blaze.Svg11 ((!), mkPath, l, m, translate, scale)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

import Sat.Signatures.Figures

import Sat.Core
import Sat.VisualModel
import Sat.VisualModels.FiguresBoard
import Sat.GUI.GState

type DrawMain = M.Map Predicate S.Svg
type DrawMod = M.Map Predicate (S.Svg -> S.Svg)


(<>) :: S.AttributeValue -> S.AttributeValue -> S.AttributeValue
x <> y = (x `mappend` toValue " ") `mappend` y

mconcat = foldr (<>) (toValue "")

intValue :: Int -> S.AttributeValue
intValue = toValue . show

urlValue :: String -> S.AttributeValue
urlValue url = toValue $ "url(" ++ url ++ ")"

pairIntValue :: (Int,Int) -> S.AttributeValue
pairIntValue (x,y) = intValue x <> intValue y

pairSepValue ::  Show a => String -> (a,a) -> S.AttributeValue
pairSepValue sep (x,y) = toValue $show x ++ sep ++ show y

pairSpacedValue :: Show a => (a,a) -> S.AttributeValue
pairSpacedValue = pairSepValue " "

pairCommaValue :: Show a => (a,a) -> S.AttributeValue
pairCommaValue = pairSepValue ","

colorValue :: Colour Float -> S.AttributeValue
colorValue = toValue . sRGB24show

colorValueA :: Colour Float -> Int -> S.AttributeValue
colorValueA c a = toValue (sRGB24show c ++ showHex a "")


blackStroke :: S.Attribute
blackStroke = A.stroke (colorValue Names.black) `mappend` A.strokeWidth (intValue 2)

color :: Colour Float -> S.Svg -> S.Svg
color c f = f ! (A.fill $ colorValue c)

colorAlpha :: Colour Float -> Int -> S.Svg -> S.Svg
colorAlpha c a f = f ! (A.fill $ colorValueA c a)


red   = color Names.red
blue  = color Names.blue
green = color Names.lightgreen

path points = S.path ! A.d (toValue points)

figSize :: (Int,Int) -> Float -> S.Svg -> S.Svg
figSize (tx,ty) sc fig = fig ! A.transform (translate tx ty <> scale sc sc)

big    = figSize (25,25) 1.5
normal = figSize (50,50) 1
small  = figSize (75,75) 0.5

makeSVG :: S.Svg -> S.Svg
makeSVG figure = S.docTypeSvg ! A.version (toValue "1.1")
                              ! A.width   (intValue 200) 
                              ! A.height  (intValue 200)
                 $ do ensureSize `mappend` (figure ! A.clipPath (urlValue "#clip")
                                                   ! A.clipRule (toValue "evenodd"))
    where ensureSize = S.clippath (S.rect ! A.width  (intValue 100) 
                                          ! A.height (intValue 100)
                                  )
                                  ! A.id_ (toValue "clip")

generateSVG :: DrawMain -> DrawMod -> [Predicate]-> IO SVG
generateSVG figs mods = svgNewFromString
                        . renderMarkup 
                        . makeSVG 
                        . go
    where go [] = empty
          go (p:ps) = genSVG figs mods p ps
          empty = S.rect

genSVG :: DrawMain -> DrawMod -> Predicate -> [Predicate]-> S.Svg
genSVG figs mods p = foldl getPredM (getPredF p)
    where getPredF p = S.g $ fromJust (M.lookup p figs)
          getPredM s p = (fromJust $ M.lookup p mods) s

generateSVGFromEB :: DrawMain -> DrawMod -> ElemBoard -> IO SVG
generateSVGFromEB figs mod = generateSVG figs mod . interpPreds
                    