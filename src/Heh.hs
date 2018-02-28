{-# LANGUAGE OverloadedStrings #-}
module Heh
        ( Command(..)
        , run
        , withMySQL
        ) where

import Turtle
import qualified Control.Foldl as Fold (null)
import Data.Maybe (isJust)
import qualified Data.Text as Text (unwords, pack, unpack)
import System.Posix.Process (executeFile)
import System.IO.Silently (silence)
import System.Random (newStdGen, randomRs)
import Data.Text (pack)
import Control.Exception (bracket)

data Command
  = Start Text Int
  | Stop Text
  | Restart Text
  | Repl Text
  deriving (Show, Eq)

run :: Command -> IO ()
run (Start n p) = start n p
run (Stop n)    = stop n
run (Restart n) = restart n
run (Repl n)    = repl n

start name port = do
  existing <- isRunning name "-a"
  if existing then docker "start" name else runMySQL name port

stop name = docker "stop" name >> docker "rm" name

restart name = do
  running <- isRunning name ""
  if running then docker "restart" name else die "mysql is not running"

repl name = do
  running <- isRunning name ""
  if running then runRepl name else die "mysql is not running"

isRunning name flag =
  not <$> fold (grepName dockerPs) Fold.null
  where
    grepName = grep $ has (text name)
    dockerPs       = inshell ("docker ps -f \"label=org.heh.container\"" <> flag) empty

docker command name = silence $ procs "docker" [command, name] empty

runMySQL name port = do
  password <- genPassword
  silence $ shells (command password) empty
  where
    command password =
      Text.unwords
        [ "docker run"
        , "--name " <> name
        , "-p '0.0.0.0:" <> pack (show port) <> ":3306'"
        , "-e MYSQL_ROOT_PASSWORD=" <> password
        , "-l org.heh.container=true"
        , "-d mysql:latest --verbose --server-id=1"
        , "--log-bin --binlog-format='ROW'"
        ]

genPassword = Text.pack <$> fmap (take 16 . randomRs ('a','z')) newStdGen

runRepl name =
  executeFile "docker" True args Nothing
  where
    args =
      [ "run"
      , "-it"
      , "--link"
      , Text.unpack name <> ":mysql"
      , "--rm"
      , "mysql"
      , "sh"
      , "-c"
      , "exec mysql -h\"$MYSQL_PORT_3306_TCP_ADDR\" -P\"$MYSQL_PORT_3306_TCP_PORT\" -uroot -p\"$MYSQL_ENV_MYSQL_ROOT_PASSWORD\""
      ]

withMySQL :: Text -> Int -> (String -> IO a) -> IO a
withMySQL name port = bracket startMySQL stopMySQL
  where
    host       = "localhost:" ++ show port
    startMySQL = Heh.run (Heh.Start name port) >> return host
    stopMySQL  = const (Heh.run $ Heh.Stop name)
