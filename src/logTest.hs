module LogTest where

import Test.QuickCheck
import Log
import LogParser

-- Test for parseMessage
prop_parseMessage :: MessageType -> TimeStamp -> String -> Bool
prop_parseMessage mt ts msg =
  let logMsg = parseMessage $ show mt ++ " " ++ show ts ++ " " ++ msg
  in case logMsg of
    LogMessage mType timestamp message -> mType == mt && timestamp == ts && message == msg
    _ -> False

-- Test for insert
prop_insert :: LogMessage -> MessageTree -> Bool
prop_insert logMsg tree =
  let newTree = insert logMsg tree
  in case newTree of
    Leaf -> True 
    Node left lgMsg right ->
      if getTime logMsg <= getTime lgMsg
      then case left of
        Leaf -> True
        Node _ l _ -> getTime logMsg <= getTime l
      else case right of
        Leaf -> True
        Node _ l _ -> getTime logMsg > getTime l

-- Test for build
prop_build :: [LogMessage] -> Bool
prop_build logs =
  let tree = build logs
      inOrderLogs = inOrder tree
  in length logs == length inOrderLogs && all (`elem` logs) inOrderLogs

-- Test for whatWentWrong
prop_whatWentWrong :: [LogMessage] -> Bool
prop_whatWentWrong logs =
  let relevantLogs = filter isRelevantError logs
      severeLogs = filter isSevereError logs
      expected = map extractMessage $ filter isRelevantError $ filter isSevereError logs
      result = whatWentWrong logs
  in result == expected

main :: IO ()
main = do
  quickCheck prop_parseMessage
  quickCheck prop_insert
  quickCheck prop_build
  quickCheck prop_whatWentWrong
