{-# LANGUAGE OverloadedStrings #-}
module Main where

import Turtle 

import qualified Control.Foldl as Fold (head)
import Data.Maybe (isJust)
import qualified Data.Text as Text (unwords)
import System.Posix.Process (executeFile)
import System.IO.Silently (silence)

parser =
      argText "start"  "start mysql container"
  <|> argText "stop"   "stop mysql container"
  <|> argText "resart" "restart mysql container"
  <|> argText "repl"   "start repl to mysql container"

name     = "dev-mysql"
password = "todo"

main = do
  command <- options "heh" parser
  case command of
    "start"   -> start
    "stop"    -> stop
    "restart" -> restart
    "repl"    -> repl
    other     -> die $ "unknown command " <> other

start = do
  existing <- isRunning name "-a"
  if existing then docker "start" name else runMySQL name password

stop = docker "stop" name >> docker "rm" name

restart = do
  running <- isRunning name ""
  if running then stop >> start else start

repl = do
  running <- isRunning name ""
  if running then runRepl else die "mysql is not running"

isRunning name flag = do
  front <- fold (grepName (inshell ("docker ps " <> flag) empty)) Fold.head
  return $ isJust front
  where
    grepName = grep $ has (text name)

docker command name = silence $ procs "docker" [command, name] empty

runMySQL name password =
  silence $ shells command empty
  where
    command =
      Text.unwords
        [ "docker run"
        , "--name " <> name
        , "-p '0.0.0.0:3306:3306'"
        , "-e MYSQL_ROOT_PASSWORD=" <> password
        , "-d mysql:latest --verbose --server-id=1"
        , "--log-bin --binlog-format='ROW'"
        ]

runRepl =
  executeFile "docker" True args Nothing
  where
    args =
      [ "run"
      , "-it"
      , "--link"
      , "dev-mysql:mysql"
      , "--rm"
      , "mysql"
      , "sh"
      , "-c"
      , "exec mysql -h\"$MYSQL_PORT_3306_TCP_ADDR\" -P\"$MYSQL_PORT_3306_TCP_PORT\" -uroot -p\"$MYSQL_ENV_MYSQL_ROOT_PASSWORD\""
      ]
