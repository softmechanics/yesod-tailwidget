{-# LANGUAGE QuasiQuotes
           , TypeFamilies
           , TypeSynonymInstances
           , FlexibleInstances
           , FlexibleContexts
           , UndecidableInstances
           , OverlappingInstances
           , MultiParamTypeClasses
           , TemplateHaskell
           #-}

module TailWidget where

import Yesod
import Text.Printf
import System.Process
import Control.Applicative
import Language.Haskell.TH.Syntax
import Data.Maybe

data TailWidget = TailWidget
  { twPollInterval :: Int
  }


mkYesodSub "TailWidget" 
  [ClassP ''Yesod [VarT $ mkName "master"]
  ,ClassP ''YesodSubRoute [ConT ''TailWidget 
                          ,VarT $ mkName "master"
                          ]
  ]
  [$parseRoutes|
/#FilePath                  TailLogR GET
/tailLines/#FilePath        TailStartR GET
/tailLines/#FilePath/#Int   TailContR GET
|]

getTailLogR fp = defaultLayout $ tailWidget fp

tailWidget :: YesodSubRoute TailWidget y => FilePath -> GWidget TailWidget y ()
tailWidget fp = do
  addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js"
  logPanel <- newIdent

  rtm <- liftHandler $ getRouteToMaster

  addJulius [$julius| 
    function handleLogResponse(resp) {
      $('#%logPanel%').append(resp.lines);
      var log = document.getElementById('%logPanel%');
      log.scrollTop = log.scrollHeight;

      function poll () { 
          $.ajax({url:resp.pollUrl, success:handleLogResponse}); 
      }

      if(resp.pollUrl) {
        setTimeout(poll, 1000);
      }
    }

    $(document).ready(function () {
      $.ajax({url:"@rtm TailStartR fp@", success:handleLogResponse})
    });
  |]

  addHamlet [$hamlet|
%pre!id=$logPanel$!style=width:800px;height:100px;overflow-y:scroll;
|]

getTailStartR :: FilePath -> GHandler TailWidget y RepJson
getTailStartR fp = do
  (last, lines) <- liftIO $ tailLast fp 50
  tailJsonResponse lines fp last

getTailContR :: FilePath -> Int -> GHandler TailWidget y RepJson
getTailContR fp last = do
  (last', lines) <- liftIO $ do 
    print last
    tailAfter fp last
  tailJsonResponse lines fp last'

tailJsonResponse :: String -> FilePath -> Int -> GHandler TailWidget y RepJson
tailJsonResponse lines fp last = do
  render <- getUrlRender
  rtm <- getRouteToMaster
  jsonToRepJson $ jsonMap [
      ("pollUrl", jsonScalar $ render $ rtm $ TailContR fp last)
    , ("lines", jsonScalar lines)
    ]

-- | start tailing, with at least n lines of context
tailLast :: FilePath -> Int -> IO (Int, String)
tailLast fp n = do
  lines <- countLinesFile fp
  let start = max 1 $ lines - n
  tailAfter fp start

tailAfter :: FilePath -> Int -> IO (Int, String)
tailAfter fp start = do
  text <- readProcess "tail" ["-n", "+" ++ show start, fp] []
  let end = start + (countLinesString text) 
  return (end, text)

countLinesFile :: FilePath -> IO Int
countLinesFile f = do
  lines <- readProcess "wc" ["-l", f] []
  putStrLn lines
  return $ read $ head $ words lines

countLinesString :: String -> Int
countLinesString = length . filter (=='\n')

