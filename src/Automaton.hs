module Automaton where
  
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

stateNumLayback :: NFA -> Int -> NFA 
stateNumLayback (NFA start1 stateSet1 acceptSet1 alphabet1 rules1) x = 
    let start' = start1 + x 
        stateSet' = Set.map (+ x) stateSet1  
        acceptSet' = Set.map (+ x) acceptSet1 

        rulesList = Map.toList rules1
        rulesList' = map (\((state, sym), set)->((state+x, sym), Set.map (+x) set)) rulesList 
        rules' = Map.fromList rulesList'
    in NFA start' stateSet' acceptSet' alphabet1 rules'


-- Auxiliary Function for Thompson Algorithm
-- convention: the start nfa state is always 0, and the only accept state is always MAX
-- direction: connect from left to right
connect2Nfa :: NFA -> NFA -> NFA 
connect2Nfa (NFA start1 stateSet1 _ alphabet1 rules1) nfa2 =  
                let 
                    lenNfa1 = length stateSet1
                    NFA _ stateSet2 acceptSet2 alphabet2 rules2 = stateNumLayback nfa2 lenNfa1 

                    newStartState = start1
                    newStateSet = Set.union stateSet1 stateSet2 
                    newAccSet = acceptSet2 
                    newAlphabet = Set.union alphabet1 alphabet2
                    newRules = Map.insert (lenNfa1-1, Nothing) (Set.fromList [lenNfa1]) $ Map.union rules1 rules2 
                in NFA newStartState newStateSet newAccSet newAlphabet newRules

connectNfas :: [NFA] -> NFA
connectNfas [] = undefined
connectNfas [nfa] = nfa 
connectNfas (nfa1:nfa2:nfas) = connectNfas $ connect2Nfa nfa1 nfa2: nfas 

alter2nfa :: NFA -> NFA -> NFA 
alter2nfa nfa1 nfa2 = 
    let 
        nfa1' = stateNumLayback nfa1 1 
        nfa2' = stateNumLayback nfa2 nfa2'Start

        nfa1'Start = 1
        nfa1'End = length $ stateSet nfa1 
        nfa2'Start = nfa1'End + 1
        nfa2'End = nfa2'Start + length (stateSet nfa2) - 1

        accStateNum =  nfa2'End + 1

        newStartState = 0 
        newStateSet = Set.fromList [0..accStateNum]
        newAccSet = Set.fromList [accStateNum] 
        newAlphabet = Set.union (alphabet nfa1) (alphabet nfa2)
        newRules = Map.fromList $ 
                    [((0, Nothing), Set.fromList [nfa1'Start, nfa2'Start]), 
                    ((nfa1'End, Nothing), Set.fromList [accStateNum]),
                    ((nfa2'End, Nothing), Set.fromList [accStateNum])
                    ] ++ Map.toList (Map.union (transition nfa1') (transition nfa2'))
    in NFA newStartState newStateSet newAccSet newAlphabet newRules

closureNfa :: NFA -> NFA 
closureNfa nfa = 
    let 
        nfa' = stateNumLayback nfa 1 
        nfa'St = 1 
        nfa'Ed = length $ stateSet nfa 
        accStateNum =  nfa'Ed + 1

        newStartState = 0 
        newStateSet = Set.fromList [0..accStateNum]
        newAccSet = Set.fromList [accStateNum] 
        newAlphabet = alphabet nfa
        newRules = Map.fromList $ 
                    [((0, Nothing), Set.fromList [nfa'St, accStateNum]), 
                    ((nfa'Ed, Nothing), Set.fromList [nfa'St, accStateNum])
                    ] ++ Map.toList (transition nfa') 
    in NFA newStartState newStateSet newAccSet newAlphabet newRules


data TpsTag = EPS | SGT | CON | ALT | CLS -- thompson tag: epsilon / singleton / connection / alternation / closure 

thompson1 :: TpsTag -> Maybe Symbol -> NFA
thompson1 EPS Nothing = 
    NFA 0 (Set.fromList [0,1]) (Set.fromList [1]) (Set.fromList []) (Map.fromList [((0, Nothing), Set.fromList [1])])
thompson1 SGT (Just c)= 
    NFA 0 (Set.fromList [0,1]) (Set.fromList [1]) (Set.fromList ['k']) (Map.fromList [((0, Just c), Set.fromList [1])])
thompson1 _ _  = undefined

thompson2 :: TpsTag -> NFA -> Maybe NFA -> NFA 
thompson2 CON nfa1 (Just nfa2) = connect2Nfa nfa1 nfa2 
thompson2 ALT nfa1 (Just nfa2) = alter2nfa nfa1 nfa2 
thompson2 CLS nfa1 Nothing = closureNfa nfa1
thompson2 _ _ _ = undefined


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
constructDFA nfa (q:qs) (DFA st symbols statesSet rulez) = 
    let 
        transList' = [(Just c, epsClosure nfa (destStates (Just c)))| c <- Set.toList symbols]
        transList = filter (\(_, s) -> not (Set.null s)) transList' 
        destStates sym = foldl (\acc s -> Set.union (getTransitions nfa s sym) acc) Set.empty q 

        traverseAndUpdate [] rules_ qs_ dfa = (rules_, qs_, dfa)
        traverseAndUpdate ((sym, destClosure):ps) rules_ qs_ dfa = 
            let 
                rules' = Map.insert (q, sym) destClosure rules_
                qs_' = if Set.member destClosure (collectionSet dfa) then qs_ else destClosure:qs_
                collectionSet' = Set.insert destClosure (collectionSet dfa)
                dfa_' = DFA st symbols collectionSet' rules'  
            in traverseAndUpdate ps rules' qs_' dfa_'
        
        (_, qs', dfa') = traverseAndUpdate transList rulez qs (DFA st symbols statesSet rulez)

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
unwrappedLookup key mymap = case Map.lookup key mymap of
    Just x  -> x   
    Nothing -> error "Key not found!"  

nfa2dfar :: NFA -> DFAr 
nfa2dfar nfa = dfa2dfar nfa $ nfa2dfa nfa

-- TODO:
minimalDfa :: DFAr -> DFAr 
minimalDfa = undefined
