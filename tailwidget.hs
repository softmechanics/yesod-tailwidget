{-# LANGUAGE QuasiQuotes
           , OverloadedStrings 
           , TypeFamilies
           , MultiParamTypeClasses
           , TypeSynonymInstances
           , TemplateHaskell
           #-}

import System.Process

import Yesod
import TailWidget

data Test = Test

mkYesod "Test" [$parseRoutes|
/ RootR GET
/tail/#FilePath/ TailWidgetR TailWidget getTailWidget
|]

-- getTailWidget can be written in a monadic or pure style

{-- Monadic version --
getTailWidget :: FilePath -> GHandler Test Test TailWidget
getTailWidget fp = return $ TailWidget 1 fp
--}

{-- Pure version --}
getTailWidget :: FilePath -> Test -> TailWidget
getTailWidget fp _ = TailWidget 1 fp
--}

instance Yesod Test where 
  approot _ = ""

getRootR :: GHandler Test Test RepHtml
getRootR = redirect RedirectTemporary $ TailWidgetR "date.log" TailLogR

main :: IO ()
main = do
  runCommand "./logger.sh"
  basicHandler 3000 Test

