{-# OPTIONS_GHC -Wall #-}
-- module LogAnalysis 
module Hw02 where

import Log

-- exercise 1
parseMessage :: String -> LogMessage
parseMessage line = case words line of
  ("I":ts:ws) -> LogMessage Info (read ts) (unwords ws)
  ("W":ts:ws) -> LogMessage Warning (read ts) (unwords ws)
  ("E":err:ts:ws) -> LogMessage (Error (read err)) (read ts) (unwords ws)
  s -> Unknown (unwords s)

-- run `testParse parse 10 "src/error.log"` for test
parse :: String -> [LogMessage]
parse txt = fmap parseMessage (lines txt)

example1 :: IO [LogMessage]
example1 = testParse parse 10 "./src/error.log"


-- exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert message Leaf = Node Leaf message Leaf
insert new@(LogMessage _ newts _) (Node left old@(LogMessage _ oldts _) right) =
  if newts <= oldts then (Node (insert new left) old right)
  else (Node left old (insert new right))
insert _ tree = tree


-- exercise 3
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)


-- exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = (inOrder left) ++ [msg] ++ (inOrder right)


-- exercise 5

severe :: LogMessage -> Bool
severe (LogMessage (Error x) _ _)
  | x > 50 = True
severe _ = False

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ s) = s
getMessage (Unknown s) = s

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lms = map getMessage (inOrder (build (filter severe lms)))

example5 :: IO [String]
example5 = testWhatWentWrong parse whatWentWrong "./src/sample.log"
