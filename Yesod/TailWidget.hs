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

module Yesod.TailWidget where

import Yesod
import Yesod.Json
import Text.Printf
import System.Process
import Control.Applicative
import Language.Haskell.TH.Syntax hiding (lift)
import Data.Maybe
import Data.JSON.Types

data TailWidget = TailWidget
  { twPollInterval :: Int
  , twFilePath :: FilePath
  , twTailAfter :: FilePath -> Int -> IO String
  , twCountLines :: FilePath -> IO Int
  }

defaultTailWidget = TailWidget
  { twPollInterval = 10 
  , twFilePath = error "twFilePath not set"
  , twTailAfter = defaultTwTail
  , twCountLines = defaultTwCountLines
  }

defaultTwTail :: FilePath -> Int -> IO String
defaultTwTail fp startLine = readProcess "tail" ["-n", "+" ++ show startLine, fp] []

defaultTwCountLines :: FilePath -> IO Int
defaultTwCountLines fp = do
  lines <- readProcess "wc" ["-l", fp] []
  return $ read $ head $ words lines

mkYesodSub "TailWidget" 
  [ClassP ''Yesod [VarT $ mkName "master"]]
  [$parseRoutes|
/             TailLogR GET
/start        TailStartR GET
/cont/#Int    TailContR GET
|]

getFilePath :: GHandler TailWidget y FilePath
getFilePath = twFilePath `fmap` getYesodSub

getTailLogR :: Yesod y => GHandler TailWidget y RepHtml
getTailLogR = defaultLayout $ tailWidget 

tailWidget :: GWidget TailWidget y ()
tailWidget = do
  addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js"
  logPanel <- lift newIdent

  rtm <- lift getRouteToMaster

  addJulius [$julius| 
    function handleLogResponse(resp) {
      $('##{logPanel}').append(resp.lines);
      var log = document.getElementById('#{logPanel}');
      log.scrollTop = log.scrollHeight;

      function poll () { 
          $.ajax({url:resp.pollUrl, success:handleLogResponse}); 
      }

      if(resp.pollUrl) {
        setTimeout(poll, 1000);
      }
    }

    $(document).ready(function () {
      $.ajax({url:"@{rtm TailStartR}", success:handleLogResponse})
    });
  |]

  addHamlet [$hamlet|
<pre id=#{logPanel} style=width:800px;height:100px;overflow-y:scroll;
|]

getTailStartR :: Yesod y => GHandler TailWidget y RepJson
getTailStartR = do
  fp <- getFilePath
  (last, lines) <- tailLast fp 50
  tailJsonResponse lines last

getTailContR :: Yesod y => Int -> GHandler TailWidget y RepJson
getTailContR last = do
  fp <- getFilePath
  (last', lines) <- tailAfter fp last
  tailJsonResponse lines last'

tailJsonResponse :: Yesod y => String -> Int -> GHandler TailWidget y RepJson
tailJsonResponse lines last = do
  render <- getUrlRender
  rtm <- getRouteToMaster
  jsonToRepJson $ jsonMap [
      ("pollUrl", jsonScalar $ render $ rtm $ TailContR last)
    , ("lines", jsonScalar lines)
    ]

-- | start tailing, with at least n lines of context
tailLast :: Yesod y => FilePath -> Int -> GHandler TailWidget y (Int, String)
tailLast fp n = do
  lines <- countLines fp
  let start = max 1 $ lines - n
  tailAfter fp start

tailAfter :: Yesod y => FilePath -> Int -> GHandler TailWidget y (Int, String)
tailAfter fp start = do
  tw <- getYesodSub
  text <- liftIO $ twTailAfter tw fp start
  let end = start + (countLinesString text) 
  return (end, text)

countLines :: Yesod y => FilePath -> GHandler TailWidget y Int
countLines fp = do
  tw <- getYesodSub
  liftIO $ twCountLines tw fp

countLinesString :: String -> Int
countLinesString = length . filter (=='\n')

