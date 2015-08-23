{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (div)
import Clay
import Data.Text.Lazy.IO as T

main :: IO ()
main = do T.writeFile "./static/css/style.css" $ render myStylesheet
          T.writeFile "./static/css/customBootstrap.css" $ render customBootstrap

customBootstrap :: Css
customBootstrap = do div # byId "loginbox"
                         ? do background grey
                              paddingBottom (px 20)
                     button # byId "login-btn" ? width (px 80)


myStylesheet :: Css
myStylesheet = do body ? background black'

                  h1 ? do fontFamily ["Archistico"] [sansSerif]
                          color steelblue
                          textAlign $ alignSide sideCenter
                          fontSize (pt 20)

                  (input # ("id" @= "username")) ? inputStyling
                  (input # ("id" $= "password")) ? inputStyling

                  (div # byClass "loginbox") ?
                    do background steelblue
                       color black'
                       centered
                       position relative
                       width  $ px 250
                       padding (px 0) (px 10) (px 10) (px 10)
                       borderRadius (px 5) (px 5) (px 5) (px 5)
                       boxShadows [(px x, px y, px 4, white)| x<-[-1,1], y<-[-1,1]]


                  (div # byClass "loginbox") |> h2 ? do
                      fontFamily ["Archistico"] [sansSerif]
                      fontWeight bold
                      textShadow (px 1) (px 0) (px 1) black'
                      textShadow (px (-1)) (px 0) (px 1) black'
                      textAlign $ alignSide sideCenter

                  (div # byClass "btn") ?
                      do width $ px 50
                         height $ px 50
                         background khaki
                         transform (rotate $ deg 45)
                         position absolute
                         right $ px (-25)
                         top  $ px 52
                         boxShadows [(px   0 , px   1 , px 2, black')
                                    ,(px (-1), px   0 , px 2, black')
                                    ,(px   0 , px (-1), px 4, white)
                                    ,(px   1 , px   0 , px 4, white)]
                  (div # byClass "btn") # hover  ? background yellow
                  (div # byClass "btn") # active ? background white

                  fontFace $ do fontFamily ["Archistico"] []
                                fontFaceSrc [FontFaceSrcUrl "/static/fonts/Archistico/Archistico_Bold.ttf" (Just TrueType)]
                                fontWeight bold
                  fontFace $ do fontFamily ["Quicksand"] []
                                fontFaceSrc [FontFaceSrcUrl "/static/fonts/Quicksand/Quicksand-Bold.otf" (Just OpenType)]
                                fontWeight bold
centered :: Css
centered = do width $ pct 100
              boxSizing borderBox
              sym2 margin (px 0) auto

inputStyling :: Css
inputStyling = do display block
                  sym2 margin (px 0) auto
                  background black'
                  color white

black' ::  Color
black' = parse "#222222"
