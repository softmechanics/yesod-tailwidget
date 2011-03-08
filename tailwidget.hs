{-# LANGUAGE QuasiQuotes
           , OverloadedStrings 
           , TypeFamilies
           , MultiParamTypeClasses
           , TypeSynonymInstances
           , TemplateHaskell
           #-}

import System.Process

import Yesod
import Yesod.TailWidget

data Test = Test

mkYesod "Test" [$parseRoutes|
/ RootR GET
/tail/#FilePath/ TailWidgetR TailWidget getTailWidget
|]

-- getTailWidget can be written in a monadic or pure style

{-- Monadic version --  TODO broken --
getTailWidget :: Test -> FilePath -> GHandler Test Test TailWidget
getTailWidget _ fp = return $ TailWidget 1 fp
--}

{-- Pure version --}
getTailWidget :: Test -> FilePath -> TailWidget
getTailWidget _ fp = defaultTailWidget 
  { twPollInterval = 1
  , twFilePath = fp
  }
--}

instance Yesod Test where 
  approot _ = ""

getRootR :: GHandler Test Test RepHtml
getRootR = redirect RedirectTemporary $ TailWidgetR "date.log" TailLogR

main :: IO ()
main = do
  runCommand "./logger.sh"
  warp 3000 Test

