{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text.Lazy
import qualified Text.Blaze.Html.Renderer.Text as R
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty
import Render (defaultWindow, render)

import Shapes

exampleDrawing :: Either Drawing MaskedDrawing
exampleDrawing = Left [(identity, hexagon (200, 100, 200)), ( scale (point 0.5 0.5) <+>shearY (-0.5), square (225, 225, 225))] --  [(identity, polygon 11 )] -- [(scale (point 0.5 0.25) <+> rotate 2 <+> translate (point 1.2 0.4), square)]

m :: Mask
m = (translate (point 1.2 0.4), square (255, 255, 255))

drawing :: Drawing
drawing = [(identity, hexagon (200, 100, 200)), ( scale (point 0.5 0.5) <+>shearY (-0.5), square (225, 225, 225))]
maskedDrawing:: Either Drawing MaskedDrawing
maskedDrawing = Right $ mask m drawing 

main :: IO ()
main =
  do
    -- print $ show $ nPolygon 3
    render "black_and_white.png" defaultWindow exampleDrawing
    render "masked.png" defaultWindow maskedDrawing
    scotty 3000 $ do
      get "/" $ do
        html $ response exampleDrawing

      get "/images/:file" $ do 
        filename <- param "file"
        file filename

-- TODO: make it take a list of drawings and map 
response :: Either Drawing MaskedDrawing-> Text
response dslProgram =
  R.renderHtml  $
    H.docTypeHtml $ do 
        H.head $ 
            H.title "Image Server"
        H.body $ do
            H.h2 "Source Code"
            H.pre $ H.toHtml $ show dslProgram
            H.h2 "Black and White Image"
            H.img H.! A.src "/images/black_and_white.png" 
            H.h2 "Coloured Image"
            H.img H.! A.src "/images/black_and_white.png"
            H.h2 "Masked Image"
            H.img H.! A.src "/images/masked.png"
            H.h2 "Transformed Image 1"
            H.img H.! A.src "/images/black_and_white.png"
            H.h2 "Transformed Image 2"
            H.img H.! A.src "/images/black_and_white.png"
