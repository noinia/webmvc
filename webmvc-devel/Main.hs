module Main (main) where

import qualified Lucid.Base as Lucid
import           WebMVC
import           WebMVC.Diff

--------------------------------------------------------------------------------

main = do
  print $ Lucid.renderText $ Lucid.toHtml $ myHtmlPage
  print $ diff myHtmlPage myHtmlPage2
