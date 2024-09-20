module Main where

import Nfa2dfa
import qualified Data.Map as Map
import qualified Data.Set as Set

test :: DFA
test = constructDFA exampleNFA [epsClosure exampleNFA (Set.fromList [startState exampleNFA])] initDFA
initDFA :: DFA

initDFA = DFA {
    startStates = epsClosure exampleNFA (Set.fromList [startState exampleNFA]),
    alphabt = alphabet exampleNFA,
    collectionSet = Set.singleton (epsClosure exampleNFA (Set.fromList [startState exampleNFA])),
    transitionRules = Map.empty
    }

exampleNFA :: NFA 
exampleNFA = NFA {
    startState = 0,
    stateSet = Set.fromList [0..9],
    acceptSet = Set.fromList [9],
    alphabet = Set.fromList ['a', 'b','c'],
    transition = Map.fromList [
        ((0, Just 'a'), Set.fromList [1]),
        ((1, Nothing),  Set.fromList [2]),
        ((2, Nothing),  Set.fromList [3, 9]),
        ((3, Nothing),  Set.fromList [4, 6]),
        ((4, Just 'b'), Set.fromList [5]),
        ((5, Nothing),  Set.fromList [8]),
        ((6, Just 'c'), Set.fromList [7]),
        ((7, Nothing),  Set.fromList [8]),
        ((8, Nothing),  Set.fromList [3, 9])
    ]
} 

exampleDFA :: DFA 
exampleDFA = nfa2dfa exampleNFA 
exampleDFAr :: DFAr 
exampleDFAr = dfa2dfar exampleNFA exampleDFA


main :: IO ()
main = print exampleDFAr
