module Grammar where

import qualified Data.Set as Set 
import qualified Data.Map as Map 

data Symbol x = T x | V x deriving (Show, Eq, Ord)  

type Rule = (Symbol Char, [Symbol Char]) 
--     Rule {
--       lhs :: Symbol Char,     -- must be Variable 
--       rhs :: [Symbol Char]    -- epsilon represented by empty list, thus no condition like 'x ε y' which can be implemented with Maybe
--     }

data Grammar = Grammar {
    gTerminals :: [Symbol Char],
    gVariables :: [Symbol Char],
    gRules :: [Rule],
    gStartVariable :: Symbol Char  -- must be Variable 
}

rulesFor :: Symbol Char -> Grammar -> [Rule]
rulesFor v grammar = foldl (\acc (lhs, rhs) -> if lhs == v then (lhs, rhs):acc else acc ) [] $ gRules grammar

nullable :: Grammar -> Set.Set (Symbol Char)
nullable grammar  = nullable' True (gRules grammar) Set.empty   

nullable' :: Bool -> [Rule] -> Set.Set (Symbol Char) -> Set.Set (Symbol Char)
nullable' False _ s = s 
nullable' True rls s = 
    let 
        len = Set.size s 
        s' = foldl (\acc (lhs, rhs) -> if allInSet acc rhs then Set.insert lhs acc else acc) s rls 
        len' = Set.size s'
        go = len /= len'
    in nullable' go rls s'
         
allInSet :: Ord a => Set.Set a -> [a] -> Bool 
allInSet s = all (`Set.member` s)   


firstMap :: Grammar -> Map.Map (Symbol Char) (Set.Set (Symbol Char))  
firstMap = undefined

firstOfV :: Symbol Char -> Grammar -> Set.Set (Symbol Char)     -- FIRST of Variable V
firstOfV v grammar = 
    let 
        nullVset = nullable grammar 
        rls = rulesFor v grammar 

        traverseRules [] s = s 
        traverseRules ((_, rhs):rs) s = undefined

        traverseRhs [] = undefined
        traverseRhs (V v: syms) = undefined   
        traverseRhs (T t: syms) = undefined

    in undefined





-- dirty test in ghci: [TO BE REMOVED] --

-- g :: Grammar 
-- g = Grammar {
--     gTerminals = [T 's', T 't', T 'g',  T 'w', T 'e', T 'd'],
--     gVariables = [V 'S', V 'N', V 'V'],
--     gRules = [Rule (V 'S') [V 'N', V 'V', V 'N'], 
--              Rule (V 'N') [T 's'],
--              Rule (V 'N') [T 't'],
--              Rule (V 'N') [T 'w'],
--              Rule (V 'N') [T 'w'],
--              Rule (V 'V') [T 'e'],
--              Rule (V 'V') [T 'd']
--             ],
--     gStartVariable = V 'S'
-- }

g :: Grammar
g = Grammar {
    gTerminals = [T 'c', T 'd'],
    gVariables = [V 'Z', V 'Y', V 'X'],
    gRules = [(V 'Z', [V 'X', V 'Y']), 
              (V 'X', []),
              (V 'X', [T 'd']),
              (V 'Y', []),
              (V 'Y', [T 'c'])
             ],
    gStartVariable = V 'S'
}

x :: Set.Set (Symbol Char)
x = nullable g

y :: [Rule]
y = rulesFor (V 'X') g