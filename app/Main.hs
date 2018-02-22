{-# LANGUAGE OverloadedStrings #-}
module Main where

import Turtle hiding (choice)

import qualified Control.Foldl as Fold (head)
import Data.Maybe (isJust)
import qualified Data.Text as Text (unwords, pack, unpack)
import System.Posix.Process (executeFile)
import System.IO.Silently (silence)
import System.Random (newStdGen, randomRs)
import Control.Monad.Combinators

parser = choice
  [ start
  , stop
  , restart
  , repl
  ]
  where
  defaultedServiceName = argText "service-name" "optional name for the mysql service" <|> pure defaultName

  start :: Parser Command
  start = subcommand "start" "start mysql container" $
    Start <$> defaultedServiceName

  stop :: Parser Command
  stop = subcommand "start" "start mysql container" $
    Stop <$> defaultedServiceName

  restart :: Parser Command
  restart = subcommand "start" "start mysql container" $
    Restart <$> defaultedServiceName

  repl :: Parser Command
  repl = subcommand "start" "start mysql container" $
    Repl <$> defaultedServiceName

defaultName = "heh-mysql"

data Command
  = Start Text
  | Stop Text
  | Restart Text
  | Repl Text
  deriving (Show, Eq)


main = do
  command <- options "heh" parser
  case command of
    Start n -> start n
    Stop  n  -> stop n
    Restart n -> restart n
    Repl  n  -> repl n

start name = do
  existing <- isRunning name "-a"
  if existing then docker "start" name else runMySQL name

stop name = docker "stop" name >> docker "rm" name

restart name = do
  running <- isRunning name ""
  if running then stop name >> start name else start name

repl name = do
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
