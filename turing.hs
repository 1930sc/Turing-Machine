import System.IO;
import System.Environment;

type Symbol = String
type State = String


--  Direction
data Direction = Left | Right | Still deriving (Show, Eq, Read)


-- Tape
data Tape = Tape { value :: Symbol
                 , leftPart :: [Symbol]
                 , rightPart :: [Symbol] } deriving Show


-- Rule
data Rule = Rule { prevSymbol :: Symbol
                 , prevState :: State
                 , nextState :: State
                 , nextSymbol :: Symbol
                 , direction :: Direction } deriving Show


-- TuringMachine
data TuringMachine = TuringMachine { currentState :: State
                                   , finalStates :: [State]
                                   , rules :: [Rule] } deriving Show


readSpecificationFile specFile = do
    contents <- readFile specFile
    let result = map (\el -> split el ';') (lines contents)
    let initialState = (result !! 0) !! 0
    let finalStates = result !! 1
    let rulesList = map (\rule -> split rule ' ') (result !! 2)
    let rules = map (\rule -> Rule (rule !! 0) (rule !! 1) (rule !! 2) (rule !! 3) (read (rule !! 4) :: Direction)) rulesList
    let tm = TuringMachine { currentState = initialState
                           , finalStates = finalStates
                           , rules = rules }
    return tm

readTape tapeFile = do
    contents <- readFile tapeFile
    let tapeList = split ((lines contents) !! 0) ';'
    let tape = Tape { value = head tapeList
                    , leftPart = []
                    , rightPart = tail tapeList }
    return tape


saveResult resultFile result = do
    writeFile resultFile result


split :: String -> Char -> [String]
split "" _ = [""]
split (x:xs) ch = 
    if x == ch
        then "" : rest
        else (x : (head rest)) : (tail rest)
    where
        rest = (split xs ch) 


runTM :: (TuringMachine, Tape) -> Tape
runTM (tm, tape) = 
    if currentState tm `elem` finalStates tm
        then tape
        else runTM $ moveTM (tm, tape)


moveTM :: (TuringMachine, Tape) -> (TuringMachine, Tape)
moveTM (tm, tape) = 
    let newTm = TuringMachine (nextState rule) (finalStates tm) (rules tm)
        newTape = moveTape (Tape (nextSymbol rule) (leftPart tape) (rightPart tape)) (direction rule)
        rule = findRule (currentState tm) (value tape) (rules tm)
    in (newTm, newTape)


moveTape :: Tape -> Direction -> Tape
moveTape tape Main.Left = moveLeft tape 
moveTape tape Main.Right = moveRight tape
moveTape tape _ = tape


moveLeft :: Tape -> Tape
moveLeft tape =
    let left = leftPart tape
        right = rightPart tape
        val = value tape
    in if null left
        then Tape "*" [] (val:right)
        else Tape (last left) (init left) (val:right)

    
moveRight :: Tape -> Tape
moveRight tape =
    let left = leftPart tape
        right = rightPart tape
        val = value tape
    in if null right
        then Tape "*" (left ++ [val]) []
        else Tape (head right) (left ++ [val]) (tail right)


findRule :: State -> Symbol -> [Rule] -> Rule 
findRule state symbol rules = 
    head $ filter (\rule -> (prevState rule == state) && (prevSymbol rule == symbol)) rules


printTapeNicely tape = 
    print $ (leftPart tape) ++ ["__" ++ value tape ++ "__"] ++ (rightPart tape)

getNiceTape tape = show $ (leftPart tape) ++ ["__" ++ value tape ++ "__"] ++ (rightPart tape)


listToString list = show list

main = do
    files <- getArgs
    let specFile = files !! 0
        tapeFile = files !! 1
        resultFile = files !! 2

    tm <- readSpecificationFile specFile
    putStrLn $ "\nTURING MACHINE: "
    print $ tm
    
    tape <- readTape tapeFile
    putStrLn $ "\nTAPE: "
    printTapeNicely $ tape

    putStrLn $ "\nRESULT: "
    let resultTape = runTM (tm, tape)
    printTapeNicely $ resultTape

    saveResult resultFile (getNiceTape resultTape)