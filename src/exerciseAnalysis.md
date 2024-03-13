# <p align="center">Project (Log Message Format)</p>
---

#### Subject: The format of log messages (Focus on log entries)
---
- Starts with a character indicating its type
- Followed by additional information

## Format:
---
- [ ] I .- Informative messages
- [ ] W .- Warning messages
- [ ] E .- Error messages (Error messages include an integer number indicating the level of severity from 1 to 100)

I and W messages do not include the severity level.

*****
*****
### Exercise 1
Parse an individual message (Parses an individual line of the log file)
#### <p align="center">parseMessage :: String -> LogMessage</p>		
Once we can parse a log message we define a function to parse a complete log file:
#### <p align="center">parse :: String -> [LogMessage]</p>	

We need to have the records in order, for that we will use the following data:
	
                                                    data MessageTree =    Leaf 
				                                                    | Node MessageTree LogMessage MessageTree
Clarification:
- MessageTree is a recursive data type, the Node constructor takes two children as arguments representing the left and right subtrees, as well as a LogMessage.  Here, Leaf represents the empty tree.

Restriction:
- A MessageTree must be ordered by timestamp, i.e., the timestamp of a LogMessage in any Node must be greater than all timestamps of any LogMes-sage in the left subtree, and less than all timestamps of any LogMessage in the right child.
- Unknown messages should not be stored in a MessageTree as they lack an untimestamp.

*****
*****
### Exercise 2
Define a function that inserts a new LogMessage into an existing MessageTree, producing a new MessageTree. 
#### <p align="center">insert :: LogMessage -> MessageTree -> MessageTree</p>		

Restriction:
- insert can assume that it is given a sorted MessageTree, and should produce a new sorted MessageTree containing the new LogMessage in addition to the contents of the original MessageTree.
- If the insert receives a LogMessage that is Unknown, it must return the MessageTree unchanged.

*****
*****
### Exercise 3
Define a function that can construct a complete MessageTree from a list of messages. 
#### <p align="center">build :: [LogMessage] -> MessageTree</p>	

Clarification:
- constructs a MessageTree containing the messages in the list by successively inserting the messages into a MessageTree (starting with a Leaf).

*****
*****
### Exercise 4
Define a function that takes a sorted MessageTree and produces a list of all the LogMessages it contains, sorted by timestamp from smallest to largest.
#### <p align="center">inOrder :: Message tree -> [LogMessage]</p>	

Clarification:
- With these functions, we can now eliminate unknown messages and order well-formed messages by using an expression like: inOrder(build tree)

*****
*****
### Exercise 5
Define a function that extracts the relevant messages (means errors with a severity of at least 50).
#### <p align="center">whatWentWrong :: [LogMessage] -> [String]</p>	

Clarification:
It takes an unsorted list of LogMessages, and returns a list of messages corresponding to any error with a severity of 50 or more, sorted by times-tamp.

### Examples

--- 

- I 6 Completed armadillo processing
- I 1 Nothing to report
- I 4 Everything normal
- I 11 Initiating self-destruct sequence
- E 70 3 Way too many pickles
- E 65 8 Bad pickle-flange interaction detected
- W 5 Flange is due for a check-up
- I 7 Out for lunch, back in two time steps
- E 20 2 Too many pickles
- I 9 Back from lunch
- E 99 10 Flange failed!

There are four errors, three of which have a severity greater than 50. the output of whatWentWrong should be:

- "Way too many pickles"
- "Bad pickle-flange interaction detected"
- "Flange failed!" 

You can test your whatWentWrong function with testWhatWentWrong, which is also provided by the Log module. You must provide testWhatWentWrong with your parse function, your whatWentWrong function and the name of the log file to parse.
