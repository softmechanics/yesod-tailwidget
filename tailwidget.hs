{-# LANGUAGE QuasiQuotes
           , OverloadedStrings 
           , TypeFamilies
           , MultiParamTypeClasses
           , TypeSynonymInstances
           , TemplateHaskell
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

import Web.Routes.Site

data Test = Test

mkYesod "Test" [$parseRoutes|
/ RootR GET
/tail/#FilePath/ TailWidgetR TailWidget getTailW 
|]

getTailW :: FilePath -> GHandler Test Test TailWidget
getTailW fp = return $ TailWidget 1 fp

instance Yesod Test where 
  approot _ = ""

getRootR :: GHandler Test Test RepHtml
getRootR = redirect RedirectTemporary $ TailWidgetR "date.log" TailLogR

main = do
  runCommand "./logger.sh"
  basicHandler 3000 Test

