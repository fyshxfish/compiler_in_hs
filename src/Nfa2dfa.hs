module Nfa2dfa where
  
import qualified Data.Map as Map
import qualified Data.Set as Set

 
type Symbol = Char 
type State = Int
type NfaTransition = Map.Map (State, Maybe Symbol) (Set.Set State) 
type DfarTransition = Map.Map (State, Maybe Symbol) State 

data NFA = NFA {
    startState :: State,
    stateSet   :: Set.Set State,
    acceptSet  :: Set.Set State,
    alphabet   :: Set.Set Symbol,
    transition :: NfaTransition
} deriving Show 

getTransitions :: NFA -> State -> Maybe Symbol -> Set.Set State
getTransitions nfa state symbol = Map.findWithDefault Set.empty (state, symbol) (transition nfa)

singleStateEpsClosure :: NFA -> State -> Set.Set State
singleStateEpsClosure nfa s = epsClosure nfa $ Set.fromList [s]

epsClosure :: NFA -> Set.Set State -> Set.Set State
epsClosure nfa states = search states Set.empty 
    where 
        search pending visited
            | Set.null pending   = Set.union states visited
            | otherwise =       
                let currentState = Set.findMin pending 
                    destinations = getTransitions nfa currentState Nothing 
                    pending' = Set.difference (Set.union destinations (Set.delete currentState pending)) visited 
                    visited' = Set.union visited destinations
                in search pending' visited'

type StatesCollection = Set.Set State
type DfaTransition = Map.Map (StatesCollection, Maybe Symbol) StatesCollection 

data DFA = DFA {
    startStates :: StatesCollection,
    alphabt :: Set.Set Symbol,
    collectionSet :: Set.Set StatesCollection,
    transitionRules :: DfaTransition
    -- acCollectionSet :: Set.Set StatesCollection
} deriving Show

nfa2dfa :: NFA -> DFA
nfa2dfa nfa = 
    let 
        startCollection = singleStateEpsClosure nfa (startState nfa)
        initWorkList = [startCollection]

        initDFA = DFA {
            startStates = startCollection,
            alphabt = alphabet nfa,
            collectionSet = Set.singleton startCollection,
            transitionRules = Map.empty
        }

    in constructDFA nfa initWorkList initDFA 

constructDFA :: NFA -> [Set.Set State] -> DFA -> DFA
constructDFA _ [] dfa = dfa 
constructDFA nfa (q:qs) (DFA st symbols statesSet rules) = 
    let 
        transList' = [(Just c, epsClosure nfa (destStates (Just c)))| c <- Set.toList symbols]
        transList = filter (\(_, s) -> not (Set.null s)) transList' 
        destStates sym = foldl (\acc s -> Set.union (getTransitions nfa s sym) acc) Set.empty q 

        traverseAndUpdate [] rules qs dfa = (rules, qs, dfa)
        traverseAndUpdate ((sym, destClosure):ps) rules qs dfa = 
            let 
                rules' = Map.insert (q, sym) destClosure rules
                qs' = if Set.member destClosure (collectionSet dfa) then qs else destClosure:qs
                collectionSet' = Set.insert destClosure (collectionSet dfa)
                dfa' = DFA st symbols collectionSet' rules'  
            in traverseAndUpdate ps rules' qs' dfa'
        
        (rules', qs', dfa') = traverseAndUpdate transList rules qs (DFA st symbols statesSet rules)

    in constructDFA nfa qs' dfa' 

data DFAr = DFAr {                  -- simplified DFA
    startNode :: State,
    nodeSet   :: Set.Set State,
    acceptNodes  :: Set.Set State,
    alphabat   :: Set.Set Symbol,
    rules :: DfarTransition
} deriving Show 

-- refined DFA
dfa2dfar :: NFA -> DFA -> DFAr
dfa2dfar nfa dfa = 
    let 
        fromSetToId = Map.fromList $ zip (Set.toList (collectionSet dfa)) [0..]  
        symbols = alphabet nfa 
        stNode = unwrappedLookup (startStates dfa) fromSetToId 
        acCollections = [s | 
            let validCollection = 
                    Set.filter (\s -> (not . Set.null) (Set.intersection s (acceptSet nfa))) (collectionSet dfa),
            s <- Set.toList validCollection]
        acNodes = Set.fromList [unwrappedLookup collection fromSetToId | collection <- acCollections]
        allNodes = Set.fromList [unwrappedLookup collection fromSetToId | collection <- Set.toList $ collectionSet dfa]

        
        reformat =
            Map.foldlWithKey
                (\acc key value -> 
                    let (st, sym) = key  
                        ed = value
                        newKey = (unwrappedLookup st fromSetToId, sym)
                        newVal = unwrappedLookup ed fromSetToId
                    in Map.insert newKey newVal acc)
                Map.empty
        
        dfarRules = reformat (transitionRules dfa)

    in DFAr stNode allNodes acNodes symbols dfarRules


unwrappedLookup :: (Ord k, Show v) => k -> Map.Map k v -> v
unwrappedLookup key map = case Map.lookup key map of
    Just x  -> x   
    Nothing -> error "Key not found!"  

