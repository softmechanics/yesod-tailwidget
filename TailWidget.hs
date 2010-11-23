{-# LANGUAGE QuasiQuotes
           , TypeFamilies
           , TypeSynonymInstances
           , FlexibleInstances
           , UndecidableInstances
           , OverlappingInstances
           #-}

import Yesod
import Text.Printf
import System.Process
import Control.Applicative
import Data.Maybe


mkYesodSub "TailWidget s y" [$parseRoutes|
/#FilePath             TailLogR GET
/tailLines/#FilePath   TailLinesR GET
|]

data TailWidget s y = TailWidget
  { twPollInterval :: Int
  , twInit :: GHandler (TailWidget s y) y String
  , twCont :: GHandler (TailWidget s y) y String
  }

getTailLogR fp = defaultLayout $ tailWidget fp

tailWidget :: FilePath -> GWidget Test Test ()
tailWidget fp = do
  addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js"
  logPanel <- newIdent
  addJulius [$julius| 
    function handleLogResponse(resp) {
      $('#%logPanel%').append(resp.lines);
      var log = document.getElementById('%logPanel%');
      log.scrollTop = log.scrollHeight;

      function poll () { 
          $.ajax({url:"@TailLinesR fp@"+resp.lastLine, success:handleLogResponse}); 
      }

      setTimeout(poll, 1000);
    }

    $(document).ready(function () {
      $.ajax({url:"@TailLinesR fp@", success:handleLogResponse})
    });
  |]

  addHamlet [$hamlet|
%pre!id=$logPanel$!style=width:800px;height:100px;overflow-y:scroll;
|]

jsonNum :: Num a => a -> Json
jsonNum = jsonScalar . show

getTailLinesR :: GHandler (TailWidget s y) y RepJson
getTailLinesR fp = do
  last <- lookupGetParam "lastLine"
  (last', lines) <- case last of
                         Nothing -> liftIO $ tailLast fp 50 
                         Just ln -> do
                           liftIO $ print ln
                           liftIO $ tailAfter fp $ read ln
  jsonToRepJson $ jsonMap [
      ("lastLine", jsonScalar $ show last')
    , ("lines", jsonScalar lines)
    ]

data FileTailState = FileTailState
  { ftsPath :: FilePath
  , ftsContext :: Int
  }

type FTWHandler y = TWHandler FileTailState y
type TWHandler s y = GHandler (TailWidget s y) y 


ftsSessionKey fp = do
  -- TODO: decorate with master route to subsite
  return fp

ftsSaveCursor :: Int -> FTWHandler y ()
ftsSaveCursor fp cur = do
  k <- ftsSessionKey fp
  setSession k $ show cur

ftsLoadCursor :: FTWHandler y Int
ftsLoadCursor fp = do
  c <- lookupSession =<< ftsSessionKey fp
  fromMaybe notFound (read <$> c)

ftsInit :: FTWHandler y String
ftsInit state@(FileTailState path ctxt) = do
  (cur, txt) <- liftIO $ tailLast path ctxt
  ftsSaveCursor path cur
  return txt

ftsCont :: FTWHandler y String
ftsCont state@(FileTailState path _) = do
  cur <- ftsLoadCursor path
  (cur', txt) <- liftIO $ tailAfter path cur
  ftsSaveCursor path cur
  return txt

defaultFileTailState :: FilePath -> FileTailState
defaultFileTailState fp = FileTailState fp ftsInit ftsCont

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


