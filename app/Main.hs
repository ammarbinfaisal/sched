module Main where

import Control.Monad (replicateM)
import Data.List (sortBy)

process :: [String] -> [(Int, Int)]
process = map (t . map read . words)
  where
    t xs = (head xs, last xs)

firstComeFirstServe ::
  Int ->
  [(Int, Int)] -> -- arrival, burst
  [(Int, Int)] -- waiting time, turnaround time
firstComeFirstServe _ [] = []
firstComeFirstServe consumed ((arrival, burst) : xs) =
  let (wait, turn) =
        ( if consumed >= arrival then consumed - arrival else 0,
          consumed + burst
        )
   in (wait, turn) : firstComeFirstServe (max arrival consumed + burst) xs

shortestJobFirst ::
  Int ->
  [(Int, Int)] -> -- arrival, burst
  [(Int, Int)] -- waiting time, turnaround time
shortestJobFirst _ [] = []
shortestJobFirst consumed xs =
  let (arrival, burst) = head xs
      (wait, turn) =
        ( if consumed >= arrival then consumed - arrival else 0,
          consumed + burst
        )
   in (wait, turn) : shortestJobFirst (max arrival consumed + burst) (tail xs)

average :: [(Int, Int)] -> (Double, Double)
average xs =
  let l = fromIntegral $ length xs
   in (fromIntegral (sum $ map fst xs) / l, fromIntegral (sum $ map snd xs) / l)

separator :: String
separator = "\t"

display :: String -> [(Int, Int)] -> IO ()
display h d =
  putStrLn (h ++ "\n" ++ "waiting" ++ separator ++ "turnaround")
    >> mapM_ (\(x, y) -> putStrLn (show x ++ separator ++ show y)) d

main :: IO ()
main = do
  putStrLn "enter number of jobs: "
  n <- (read <$> getLine) :: IO Int
  ps <- replicateM n getLine
  let p = sortBy (\(a, _) (b, _) -> compare a b) $ process ps
  let res = firstComeFirstServe 0 p
  display "First Come First Serve" res
  let p2 = sortBy (\(_, a) (_, b) -> compare a b) p
  let res2 = shortestJobFirst 0 p2
  display "Shortest Job First" res2
