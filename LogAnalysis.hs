module LogAnalysis where

import Log

-- Parse an individual message
parseMessage :: String -> LogMessage
parseMessage s = case (words s) of
  ("E":c:t:m) -> LogMessage (Error (read c)) (read t) (unwords m)
  ("W":t:m)   -> LogMessage Warning (read t) (unwords m)
  ("I":t:m)   -> LogMessage Info (read t) (unwords m)
  _           -> Unknown "This is not in the right format"

-- Parse an entire logfile
parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

-- Return a message timestamp
time :: LogMessage -> TimeStamp
time (LogMessage _ t _) = t

-- Return a message description
description :: LogMessage -> String
description (LogMessage _ _ d) = d

-- Return a tree with just one node
singleton :: LogMessage -> MessageTree
singleton m = Node Leaf m Leaf

-- Insert a message into a MessageTree
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert m Leaf           = singleton m
insert m (Node left m' right)
  | time m == time m' = Node left m right
  | time m <  time m' = Node (insert m left) m' right
  | time m >  time m' = Node left m' (insert m right)

-- Return an in-order list of messages
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left m right) = inOrder left ++ [m] ++ inOrder right

-- Determine whether a message is an error with a severity of at least n
isError :: Int -> LogMessage -> Bool
isError n (Unknown _)        = False
isError n (LogMessage t _ _) =
  case t of
    (Error severity) -> severity >= n
    _                -> False

-- Build up a MessageTree containing the messages in the list
build :: [LogMessage] -> MessageTree
build list = foldr insert Leaf list

-- Take an unsorted list of LogMessages, and return a list of the
-- messages corresponding to any errors with a severity of 50 or greater,
-- sorted by timestamp
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong list = map description $ filter (isError 50) $ inOrder (build list)
