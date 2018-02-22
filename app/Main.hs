{-# LANGUAGE OverloadedStrings #-}
module Main where

import Turtle 

import qualified Control.Foldl as Fold (head)
import Data.Maybe (isJust)
import qualified Data.Text as Text (unwords, pack, unpack)
import System.Posix.Process (executeFile)
import System.IO.Silently (silence)
import System.Random (newStdGen, randomRs)

parser =
      subcommand "start"  "start mysql container" (pure Start)
  <|> subcommand "stop"   "stop mysql container" (pure Stop)
  <|> subcommand "resart" "restart mysql container" (pure Restart)
  <|> subcommand "repl"   "start repl to mysql container" (pure Repl)

name = "heh-mysql"

data Command
  = Start
  | Stop
  | Restart
  | Repl
  deriving (Show, Eq)


main = do
  command <- options "heh" parser
  case command of
    Start   -> start
    Stop    -> stop
    Restart -> restart
    Repl    -> repl

start = do
  existing <- isRunning name "-a"
  if existing then docker "start" name else runMySQL name

stop = docker "stop" name >> docker "rm" name

restart = do
  running <- isRunning name ""
  if running then stop >> start else start

repl = do
  running <- isRunning name ""
  if running then runRepl name else die "mysql is not running"

isRunning name flag = do
  front <- fold (grepName (inshell ("docker ps " <> flag) empty)) Fold.head
  return $ isJust front
  where
    grepName = grep $ has (text name)

docker command name = silence $ procs "docker" [command, name] empty

runMySQL name = do
  password <- genPassword
  silence $ shells (command password) empty
  where
    command password =
      Text.unwords
        [ "docker run"
        , "--name " <> name
        , "-p '0.0.0.0:3306:3306'"
        , "-e MYSQL_ROOT_PASSWORD=" <> password
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
