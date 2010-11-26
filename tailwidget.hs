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

import Web.Routes.Site

data Test = Test

mkYesod "Test" [$parseRoutes|
/ RootR GET
/tail/#FilePath/*Strings TailWidgetR GET
|]

instance Yesod Test where 
  approot _ = ""

instance YesodSubRoute TailWidget Test where
  fromSubRoute (TailWidget _ fp) _ = TailWidgetR fp . fst . formatPathSegments tailWidgetSite

tailWidgetSite :: Site (Route TailWidget) (String -> Maybe (GHandler TailWidget Test ChooseRep))
tailWidgetSite = getSubSite

getRootR :: GHandler Test Test RepHtml
getRootR = redirect RedirectTemporary $ TailWidgetR "date.log" []

getTailWidgetR :: FilePath -> Strings -> GHandler Test Test ChooseRep
getTailWidgetR fp pieces = do
  y <- getYesod
  let tw = TailWidget 1 fp
      getTW = const tw
      site = tailWidgetSite
      (Just handler) = handleSite site (error "Cannot use subsite render function") route "GET"
      (Right route) = parsePathSegments site pieces

  toMasterHandler (fromSubRoute tw y) getTW route handler


main = do
  runCommand "./logger.sh"
  basicHandler 3000 Test

