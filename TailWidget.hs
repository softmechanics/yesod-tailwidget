{-# LANGUAGE QuasiQuotes
           , TypeFamilies
           , TypeSynonymInstances
           , FlexibleInstances
           , UndecidableInstances
           , OverlappingInstances
           #-}

module TailWidget where

import Yesod
import Yesod.Continuation
import Text.Printf
import System.Process
import Control.Applicative
import Data.Maybe

getTailLogR fp = defaultLayout $ tailWidget fp

tailWidget :: YesodContinuations y => FilePath -> GWidget s y ()
tailWidget fp = do
  addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js"
  logPanel <- newIdent
  pollR <- liftHandler $ addContinuation $ getTailLinesR fp Nothing

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
      $.ajax({url:"@pollR@", success:handleLogResponse})
    });
  |]

  addHamlet [$hamlet|
%pre!id=$logPanel$!style=width:800px;height:100px;overflow-y:scroll;
|]

jsonNum :: Num a => a -> Json
jsonNum = jsonScalar . show

getTailLinesR :: YesodContinuations y => FilePath -> Maybe Int -> GHandler y y RepJson
getTailLinesR fp last = do
  (last', lines) <- case last of
                         Nothing -> liftIO $ tailLast fp 50 
                         Just ln -> do
                           liftIO $ print ln
                           liftIO $ tailAfter fp ln
  pollR <- addContinuation $ getTailLinesR fp $ Just last'
  render <- getUrlRender
  jsonToRepJson $ jsonMap [
      ("pollUrl", jsonScalar $ render pollR)
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


