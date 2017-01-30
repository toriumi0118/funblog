module Main where

import System.Environment
import Web.Api
import Setting.Config

main :: IO ()
main = do
  args <- getArgs
  if "-h" `elem` args || "--help" `elem` args
  then do
    putStrLn "Usage: funblog"
    putStrLn ""
    putStrLn "Configure using the blog.cfg file"
  else do
    cfg <- parseConfig "api.cfg"
    runBlog cfg
