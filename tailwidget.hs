{-# LANGUAGE QuasiQuotes
           , OverloadedStrings 
           , TypeFamilies
           , MultiParamTypeClasses
           , TypeSynonymInstances
           #-}

import Control.Applicative
import Control.Monad
import Data.List
import Data.Monoid
import System.Process

import Yesod
import Yesod.Handler
import Yesod.Dispatch
import TailWidget

data Test = Test

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

main = do
  runCommand "./logger.sh"
  basicHandler 3000 Test

