{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text.Lazy
import qualified Text.Blaze.Html.Renderer.Text as R
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty
import Render (defaultWindow, render)

import Shapes

exampleDrawing :: Drawing
exampleDrawing = [( scale (point 0.5 0.5) <+>shearY (-0.5), square (225, 225, 225)), (identity, hexagon (200, 100, 200))] --  [(identity, polygon 11 )] -- [(scale (point 0.5 0.25) <+> rotate 2 <+> translate (point 1.2 0.4), square)]

main :: IO ()
main =
  do
    -- print $ show $ nPolygon 3
    render "black_and_white.png" defaultWindow exampleDrawing
    scotty 3000 $ do
      get "/" $ do
        html $ response exampleDrawing

      get "/images/:file" $ do 
        filename <- param "file"
        file filename

-- TODO: make it take a list of drawings and map 
response :: Drawing -> Text
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
            H.img H.! A.src "/images/black_and_white.png"
            H.h2 "Transformed Image 1"
            H.img H.! A.src "/images/black_and_white.png"
            H.h2 "Transformed Image 2"
            H.img H.! A.src "/images/black_and_white.png"
