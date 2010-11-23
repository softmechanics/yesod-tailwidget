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

data Test = Test { testContinuations :: Continuations Test }
type TestContinuations = Continuations Test

mkYesod "Test" [$parseRoutes|
/ RootR GET
/cont/ ContSubR TestContinuations testContinuations
|]

instance Yesod Test where 
  approot _ = ""

instance YesodContinuations Test where
  yesodContinuations = testContinuations
instance YesodSubRoute TestContinuations Test where
  fromSubRoute _ _ = ContSubR

getRootR = defaultLayout $ tailWidget "date.log"

main = do
  runCommand "./logger.sh"
  contState <- newContinuations 1 -- check for and clean up expired session each request
  basicHandler 3000 $ Test contState

