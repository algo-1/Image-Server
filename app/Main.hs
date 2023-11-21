{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text.Lazy
import qualified Text.Blaze.Html.Renderer.Text as R
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty
import Render (defaultWindow, render)

import Shapes


drawing :: Drawing
drawing = [( scale (point 0.5 0.5) <+>shearY (-0.5), square (225, 225, 225)), (identity, hexagon (200, 100, 200))]


m :: Mask
m = (translate (point 1.2 0.4), square (255, 255, 255))

maskedDrawing:: Either Drawing MaskedDrawing
maskedDrawing = Right $ mask m drawing


polygonsDrawing :: Drawing
polygonsDrawing = [(scale (point 0.4 0.4), triangle (200, 150, 20)), (scale (point 0.6 0.6), polygon 4 (100, 150, 200)), (scale (point 0.8 0.8), pentagon (220, 125, 50)), (scale (point 1 1), hexagon (20, 90, 10)), (scale (point 1.2 1.2), octagon (255, 255, 255))]

main :: IO ()
main =
  do
    -- print $ show $ nPolygon 3
    render "non_masked.png" defaultWindow $ Left drawing
    render "masked.png" defaultWindow maskedDrawing
    render "shear_transform.png" defaultWindow maskedDrawing
    render "covered_shapes_optimisation.png" defaultWindow maskedDrawing
    render "polygons.png" defaultWindow $ Left polygonsDrawing
    render "ellipse.png" defaultWindow maskedDrawing
    render "rectangle.png" defaultWindow maskedDrawing

    scotty 3000 $ do
      get "/" $ do
        html $ response [Left polygonsDrawing, Left drawing, maskedDrawing ]

      get "/images/:file" $ do 
        filename <- param "file"
        file filename

-- TODO: make it take a list of drawings and map ??
response :: [Either Drawing MaskedDrawing]-> Text
response dslProgram =
  R.renderHtml  $
    H.docTypeHtml $ do 
        H.head $ 
            H.title "Image Server"
        H.body $ do
            H.h2 "Polygons"
            H.h4 "Source Code"
            H.pre $ H.toHtml $ show $ dslProgram !! 0
            H.img H.! A.src "/images/polygons.png" 

            H.h2 "Non-masked Image"
            H.h4 "Source Code"
            H.pre $ H.toHtml $ show $ dslProgram !! 1
            H.img H.! A.src "/images/non_masked.png"
            
            H.h2 "Masked Image"      
            H.h4 "Source Code"
            H.pre $ H.toHtml $ show $ dslProgram !! 2       
            H.img H.! A.src "/images/masked.png"

