{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (div)
import Clay
import Control.Arrow (first)
import Data.Text.Lazy.IO as T

main :: IO ()
main = T.writeFile "./static/style.css" $ render myStylesheet

myStylesheet :: Css
myStylesheet = do body ?
                    do background $ colorstring "#221132"
                       color white
                  h1 ? do fontFamily ["Archistico"] [sansSerif]
                          textAlign $ alignSide sideCenter

                  (div # byId "loginbox") ?
                    do background red
                       color black
                       centered
                       padding (px 0) (px 10) (px 10) (px 10)
                       width  $ px 250
                       borderRadius (px 5) (px 5) (px 5) (px 5)
                  (div # byId "loginbox") |> h2 ?
                       do fontFamily ["Quicksand"] [sansSerif]
                          textAlign $ alignSide sideCenter
                  (div # byId "loginbox") |> a ?
                       do -- transform (rotateX $ deg 45)
                          background blue
                          color white
                  fontFace $ do fontFamily ["Archistico"] []
                                fontFaceSrc [FontFaceSrcUrl "/static/Archistico/Archistico_Bold.ttf" (Just TrueType)]
                                fontWeight bold
                  fontFace $ do fontFamily ["Quicksand"] []
                                fontFaceSrc [FontFaceSrcUrl "../static/Quicksand/Quicksand-Bold.otf" (Just OpenType)]
                                fontWeight bold
centered :: Clay.Css
centered = do width $ pct 100
              boxSizing borderBox
              sym2 margin (px 0) auto




colorstring :: String -> Color
colorstring ('#':str) = let (r',ggbb) = first readInt $ splitAt 2 str
                            (g',bb)   = first readInt $ splitAt 2 ggbb
                            (b',_)    = first readInt $ splitAt 2 bb
                            readInt int = read ("0x"++int) :: Integer
                        in rgb r' g' b'
colorstring _ = error "malformed colorstring"
