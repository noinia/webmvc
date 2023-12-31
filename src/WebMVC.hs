{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module WebMVC
  ( myHtmlPage
  , myHtmlPage2
  ) where

--------------------------------------------------------------------------------

import qualified Data.Map as Map
import           WebMVC.Types
import           WebMVC.Util
import           WebMVC.Html.Element

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

myHtmlPage :: View ()
myHtmlPage =
  html_ []
        [ head_ [ title_ "my title" ]
                []
        , body_ []
                [div_ []
                      [ text_ "woei" ]
                ]
        ]


myHtmlPage2 :: View ()
myHtmlPage2 =
  html_ []
        [ head_ [ title_ "my different title" ]
                []
        , body_ []
                [div_ []
                      [ text_ "bleh woei" ]
                ]
        ]
