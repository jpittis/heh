{-# LANGUAGE OverloadedStrings #-}
module Main where

import Turtle hiding (choice)
import Control.Monad.Combinators (choice)

import Heh

parser = choice
  [ start
  , stop
  , restart
  , repl
  ]
  where
  defaultedServiceName =
    argText "service-name" "optional name for the mysql service" <|> pure defaultName

  start :: Parser Command
  start = subcommand "start" "start a mysql container" $
    Start <$> defaultedServiceName
          <*> (optInt "port" 'p' "port (default: 3306)" <|> pure 3306)
          <*> (optText "pass" 's' "password (default: none)" <|> pure "")

  stop :: Parser Command
  stop = subcommand "stop" "stop a mysql container" $
    Stop <$> defaultedServiceName

  restart :: Parser Command
  restart = subcommand "restart" "restart a mysql container" $
    Restart <$> defaultedServiceName

  repl :: Parser Command
  repl = subcommand "repl" "open a repl into mysql container" $
    Repl <$> defaultedServiceName

defaultName = "heh-mysql"

main = run <$> options "heh" parser
