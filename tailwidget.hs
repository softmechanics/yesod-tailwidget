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
import Yesod.Continuation
import Yesod.Handler
import Yesod.Dispatch
import TailWidget

data Test = Test { testContState :: ContState Test }

mkYesod "Test" [$parseRoutes|
/ RootR GET
/cont/#ContKey ContR GET
|]

instance Yesod Test where 
  approot _ = ""

instance YesodContinuations Test where
  getContState = testContState <$> getYesod
  getContPruneInterval = return 1
  getContinuationRoute _ = ContR


getRootR = defaultLayout $ tailWidget "date.log"
getContR = contHandler

main = do
  runCommand "./logger.sh"
  contState <- newContState
  basicHandler 3000 $ Test contState

