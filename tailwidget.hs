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

import Yesod
import Yesod.Handler
import Yesod.Dispatch
import TailWidget

mkYesod "Test" [$parseRoutes|
/ RootR GET
|]

instance Yesod Test where 
  approot _ = ""

main = basicHandler 3000 Test

