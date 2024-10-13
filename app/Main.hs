module Main where

import Automaton
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

exampleNFA1 :: NFA 
exampleNFA1 = NFA {
    startState = 0,
    stateSet = Set.fromList [0..2],
    acceptSet = Set.fromList [2],
    alphabet = Set.fromList ['a','b','c'],
    transition = Map.fromList [
        ((0, Just 'a'), Set.fromList [1,2]),
        ((1, Nothing),  Set.fromList [2])
    ]
} 

exampleNFA2 :: NFA 
exampleNFA2 = NFA {
    startState = 0,
    stateSet = Set.fromList [0..2],
    acceptSet = Set.fromList [2],
    alphabet = Set.fromList ['a','b','c','d'],
    transition = Map.fromList [
        ((0, Nothing), Set.fromList [1]),
        ((1, Just 'd'),  Set.fromList [1,2])
    ]
} 

exampleNFA3 :: NFA 
exampleNFA3 = NFA {
    startState = 0,
    stateSet = Set.fromList [0..1],
    acceptSet = Set.fromList [1],
    alphabet = Set.fromList ['k'],
    transition = Map.fromList [
        ((0, Just 'k'), Set.fromList [1])
    ]
} 

exampleDFA :: DFA 
exampleDFA = nfa2dfa exampleNFA 
exampleDFAr :: DFAr 
exampleDFAr = dfa2dfar exampleNFA exampleDFA

nfa4 :: NFA
nfa4 = thompson1 SGT (Just 'k')
nfa5 :: NFA
nfa5 = thompson1 EPS Nothing 
nfa6 :: NFA
nfa6 = alter2nfa nfa4 nfa5
nfa7 :: NFA
nfa7 = closureNfa nfa6
nfa7' :: DFAr
nfa7' = dfa2dfar nfa7 $ nfa2dfa nfa7



main :: IO ()
-- main = print exampleDFAr
main = print "Hello, cpl-in-hs"
