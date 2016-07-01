module Main where

import           System.Process
import           System.Environment
import qualified Data.List          as Lists
import           Data.Char

printErrorMessage :: IO ()
printErrorMessage = do
  putStrLn "Не заданы файлы со списками закачек."

main :: IO ()
main = do
  args <- getArgs
  if null args then
    printErrorMessage
  else
    mapM_ download args

download :: FilePath -> IO ()
download name = do
  contents <- readFile name
  download' contents

download' :: String -> IO ()
download' t = do
  if null ds then
    return ()
  else
     callCommand $ "youtube-dl " ++ ds 
  where
    ds = Lists.intercalate " " . map convertDownload . 
         filter (not . null) . lines $ t

convertDownload :: String -> String
convertDownload d = convertDownload' d'
  where
    d' = Lists.dropWhileEnd isSpace . dropWhile isSpace $ d

convertDownload' :: String -> String
convertDownload' [] = []
convertDownload' d 
  | (fq && lq)     = d
  | (fq && not lq) = d ++ "\""
  | (not fq && lq) = '\"':d
  | otherwise      = ('\"':d) ++ "\""
  where
    fc = head d
    lc = last d
    fq = fc == '\"' 
    lq = lc == '\"'
