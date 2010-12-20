{-# LANGUAGE QuasiQuotes
           , OverloadedStrings 
           , TypeFamilies
           , MultiParamTypeClasses
           , TypeSynonymInstances
           , TemplateHaskell
           #-}

import System.Process

import Yesod
import Yesod.Handler
import TailWidget

data Test = Test

getTailWidget :: Test -> TailWidget
getTailWidget _ = TailWidget 1 

mkYesod "Test" [$parseRoutes|
/ RootR GET
/tail TailWidgetR TailWidget getTailWidget
|]

instance Yesod Test where 
  approot _ = ""

instance YesodSubRoute TailWidget Test where
  fromSubRoute _ _ = TailWidgetR

getRootR :: GHandler Test Test RepHtml
getRootR = redirect RedirectTemporary $ TailWidgetR $ TailLogR "date.log"

main :: IO ()
main = do
  runCommand "./logger.sh"
  basicHandler 3000 Test

