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

type VarMap = Map.Map (Symbol Char) (Set.Set (Symbol Char))

firstMap :: Grammar -> VarMap 
firstMap grammar = 
    let initMap = Map.fromList $ [(t, Set.empty) | t <- gVariables grammar]
    in firstMap' True grammar initMap  

firstMap' :: Bool -> Grammar -> VarMap -> VarMap
firstMap' False _ m = m
firstMap' True grammar m = 
    let 
        nullVSet = nullable grammar     -- nullable set of grammar
        rls = gRules grammar

        traverseRules [] accm = accm
        traverseRules ((v, rhs): rs) accm = 
            let curSet = unwrappedLookup v accm
                apdSet = traverseRhs rhs curSet accm
                accm' = Map.insert v apdSet accm
            in traverseRules rs accm'

        traverseRhs [] s _ = s
        traverseRhs ((T t): _) s _ = Set.union s (Set.singleton (T t))  
        traverseRhs ((V v): syms) s curM =
            let vFirstSet = unwrappedLookup (V v) curM
                s' = Set.union s vFirstSet
            in if Set.member (V v) nullVSet
               then traverseRhs syms s' curM  
               else s'
        
        m' = traverseRules rls m
        go = any (\v -> Set.size (unwrappedLookup v m') /= Set.size (unwrappedLookup v m)) (gVariables grammar)
        
    in firstMap' go grammar m'


followMap :: Grammar -> VarMap 
followMap grammar = 
    let initMap = Map.fromList $ [(t, Set.empty) | t <- gVariables grammar]
    in followMap' True grammar initMap  

followMap' :: Bool -> Grammar -> VarMap -> VarMap
followMap' False _ m = m 
followMap' True grammar m = 
    let 
        nullVSet = nullable grammar     -- nullable set of grammar
        rls = gRules grammar
        firstSet = firstMap grammar 

        traverseRules [] accm = accm
        traverseRules ((v, rhs): rs) accm = 
            let curFollowTerms = unwrappedLookup v accm
                accm' = traverseRhs (reverse rhs) curFollowTerms accm
            in  traverseRules rs accm'

        traverseRhs [] _ followM = followM 
        traverseRhs ((T t): syms) _ followM = traverseRhs syms (Set.singleton (T t)) followM 
        traverseRhs ((V v): syms) curFollowTerms followM = 
            let oldFollowSet = unwrappedLookup (V v) followM 
                newFollowSet = Set.union curFollowTerms oldFollowSet 
                followM' = Map.insert (V v) newFollowSet followM
                curFollowTerms' = if Set.member (V v) nullVSet
                                then Set.union curFollowTerms (unwrappedLookup (V v) firstSet)
                                else unwrappedLookup (V v) firstSet
            in traverseRhs syms curFollowTerms' followM' 
        
        m' = traverseRules rls m
        go = any (\v -> Set.size (unwrappedLookup v m') /= Set.size (unwrappedLookup v m)) (gVariables grammar)
        
    in followMap' go grammar m'

 
ruleFirstSet :: Rule -> Grammar -> Set.Set (Symbol Char)
ruleFirstSet (lhsv, rhs) grammar = 
    let  
        nullSet = nullable grammar
        firstSet = firstMap grammar 
        followSet = followMap grammar

        traverseRhs [] curSet = curSet  
        traverseRhs ((T t):_) curSet = Set.union curSet (Set.singleton (T t))
        traverseRhs ((V v):syms) curSet = 
            let 
                vfirst = unwrappedLookup (V v) firstSet 
                newSet = Set.union vfirst curSet 
            in 
                if Set.member (V v) nullSet 
                then traverseRhs syms newSet 
                else newSet
        
        rFirstSet = traverseRhs rhs Set.empty
        
    in if allInSet nullSet rhs 
       then Set.union rFirstSet (unwrappedLookup lhsv followSet)
       else rFirstSet

grammarRuleFirstSet :: Grammar ->  [(Rule, Set.Set (Symbol Char))]
grammarRuleFirstSet grammar = 
    let rules = gRules grammar 
    in [(rule, ruleFirstSet rule grammar) | rule <- rules]
        
grammarRuleFirstSetId :: Grammar ->  [(Int, Rule,Set.Set (Symbol Char))]
grammarRuleFirstSetId grammar = 
    let rules = gRules grammar 
        rulesIds = zip rules [0 ..]
    in [(idx, rule, ruleFirstSet rule grammar) | (rule, idx) <- rulesIds]

ll1table :: Grammar -> Map.Map (Symbol Char, Symbol Char) (Set.Set Int)
ll1table grammar = 
    let ruleFirstSetTup = grammarRuleFirstSetId grammar 
        initMap = Map.fromList $ [((v,t), Set.empty) | v <- gVariables grammar, t <- gTerminals grammar]
    in ll1table' ruleFirstSetTup initMap


ll1table' :: [(Int, Rule, Set.Set (Symbol Char))] -> Map.Map (Symbol Char, Symbol Char) (Set.Set Int) -> Map.Map (Symbol Char, Symbol Char) (Set.Set Int)
ll1table' [] m = m 
ll1table' ((idx, (v, _), s):ts) m =
    let 
        traverseTerms [] curM = curM 
        traverseTerms (term:terms) curM = 
            let 
                curSet = unwrappedLookup (v, term) curM
                newSet = Set.insert idx curSet
                newM = Map.insert (v, term) newSet curM  -- Add 'curM' as the third argument
            in traverseTerms terms newM 
        
        m' = traverseTerms (Set.toList s) m
    in ll1table' ts m'  -- Use 'm'' instead of 'newM'


unwrappedLookup :: (Ord k, Show v) => k -> Map.Map k v -> v
unwrappedLookup key mymap = case Map.lookup key mymap of
    Just value  -> value   
    Nothing -> error "Key not found!"  


-- dirty test [TO BE REMOVED] --

g2 :: Grammar 
g2 = Grammar {
    gTerminals = [T 's', T 't', T 'g', T 'w', T 'e', T 'd'],
    gVariables = [V 'S', V 'N', V 'V', V 'M'],
    gRules = [
              (V 'S', [V 'N', V 'V', V 'N']), 
              (V 'N', [T 's']), 
              (V 'N', [T 't']), 
              (V 'N', [T 'g']), 
              (V 'N', [T 'w', V 'M']), 
              (V 'M', []), 
              (V 'M', [V 'N']), 
              (V 'V', [T 'e']), 
              (V 'V', [T 'd'])
             ],
    gStartVariable = V 'S'
}

table :: Map.Map (Symbol Char, Symbol Char) (Set.Set Int)
table = ll1table g2