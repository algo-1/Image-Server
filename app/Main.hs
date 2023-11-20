{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text.Lazy
import qualified Text.Blaze.Html.Renderer.Text as R
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty

-- I don't actually use any attributes in this example
-- so I commented this next line out, but this is how
-- you get them imported when you need them.
--

import Render (defaultWindow, render)
import Shapes

exampleDrawing = [(scale (point 0.5 0.25) <+> rotate 2 <+> translate (point 1.2 0.4), square)]

main :: IO ()
main =
  do
    render "output.png" defaultWindow exampleDrawing
    scotty 3000 $ do
      get "/" $ do
        html $ responseDSL exampleDrawing

      {-
        get "/greet/" $ do
            html  "Hello there"
      -}
      get "/output.png" $ do 
        file "output.png"

      get "/greet/:name" $ do
        name <- param "name"
        html $ response name

response :: Text -> Text
response n = do
  R.renderHtml $ do
    H.h1 ("Hello " >> H.toHtml n)


responseDSL :: Drawing -> Text
responseDSL dslProgram =
  R.renderHtml  $
    H.docTypeHtml $ do 
        H.head $ 
            H.title "DSL Program and Image"
        H.body $ do
            H.h2 "DSL Program:"
            H.pre $ H.toHtml $ show dslProgram
            H.h2 "Generated Image:"
            H.img H.! A.src "/output.png" H.! A.alt "Rendered Drawing"
