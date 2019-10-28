import Data.Char
import System.IO
import System.Environment
import Data.List
import Data.List.Split

-- maps labels line numbers and variables to values - uses float for line numbers for simplicity
type SymTable = [(String,Float)]

data Expr = Constant Float |
            Var String |
            Str String |
            ExprError String | 
            Plus Expr Expr |
            Minus Expr Expr |
            Times Expr Expr |
            Div Expr Expr |
            LT_ Expr Expr |
            GT_ Expr Expr |
            GE_ Expr Expr |
            LE_ Expr Expr |
            EQ_ Expr Expr |
            NEQ_ Expr Expr deriving (Show)

data Stmt =
        Let String Expr |
        Print [Expr] |
        If Expr String | 
        Error String |
        Input String deriving (Show) 

-- dummy predicate that is supposed to check if a string is a label which is a string ending with ":"
isLabel :: String -> Bool
isLabel x = if (last x == ':') then True else False

-- takes a list of tokens as strings and returns the parsed expression
parseExpr :: [String] -> Expr
parseExpr (e1:"+":e2:[]) = Plus (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"-":e2:[]) = Minus (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"*":e2:[]) = Times (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"/":e2:[]) = Div (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"<":e2:[]) = LT_ (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:">":e2:[]) = GT_ (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:">=":e2:[]) = GE_ (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"<=":e2:[]) = LE_ (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"==":e2:[]) = EQ_ (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"!=":e2:[]) = NEQ_ (parseExpr [e1]) (parseExpr [e2])
parseExpr [x] = if (isAlpha (head x)) then (Var x) else (if (isInfixOf "\"" x) then (Str x) else Constant (read x))

-- reconstruct the [String] w/o comma
reConstruct :: [String] -> [String]
reConstruct x = splitOn "," (unwords x)

-- take in list of String and retrun a list of Expr
splitExpr :: [String] -> [Expr] -> [Expr]
splitExpr [] output = output
splitExpr (first:rest) output 
    | ((length (words first))/=0) = let expr1 = parseExpr (if (isInfixOf "\"" first) then ([first]) else (words first)) in splitExpr rest (output++[expr1])
    | otherwise = splitExpr rest output

-- takes the first token which should be a keyword and a list of the remaining tokens and returns the parsed Stmt
parseStmt :: String -> [String] -> Stmt
parseStmt "let" (v:"=":expr) = Let v (parseExpr expr)
parseStmt "if" exprexpr = If (parseExpr (init (init (exprexpr)))) (last exprexpr)
parseStmt "input" expr = Input (head expr)
parseStmt "print" exprexpr = Print (splitExpr (reConstruct exprexpr) [])

-- takes a list of tokens and returns the parsed statement - the statement may include a leading label
-- takes a list of tokens, lineNum of labels for ST, and return Stmt and env
-- parseLine :: [String] -> Float -> SymTable -> (Stmt, SymTable)
parseLine :: [String] -> Float -> SymTable -> (Stmt, SymTable)
parseLine (first:rest) pc env = 
    if (isLabel first) then parseLine rest pc ((first, pc):env)
    else (parseStmt first rest, env)

-- takes a variable name and a ST and returns the value of that variable or zero if the variable is not in the ST
lookupVar :: String -> SymTable -> Float
lookupVar name [] = 0
lookupVar name ((id,v):rest) = if (id == name) then v else lookupVar name rest

-- evaluates the given Expr with the variable values found in the given ST
eval :: Expr ->SymTable -> Float
eval (Var v) env = lookupVar v env
eval (Constant v) _ = v
eval (Plus e1 e2) env = (eval e1 env) + (eval e2 env)
eval (Minus e1 e2) env = (eval e1 env) - (eval e2 env)
eval (Times e1 e2) env = (*) (eval e1 env) (eval e2 env)
eval (Div e1 e2) env = (/) (eval e1 env) (eval e2 env)
eval (LT_ e1 e2) env = if ((eval e1 env) < (eval e2 env)) then 1 else 0
eval (GT_ e1 e2) env = if ((eval e1 env) > (eval e2 env)) then 1 else 0
eval (GE_ e1 e2) env = if ((eval e1 env) >= (eval e2 env)) then 1 else 0
eval (LE_ e1 e2) env = if ((eval e1 env) <= (eval e2 env)) then 1 else 0
eval (EQ_ e1 e2) env = if ((eval e1 env) == (eval e2 env)) then 1 else 0
eval (NEQ_ e1 e2) env = if ((eval e1 env) /= (eval e2 env)) then 1 else 0

-- turn 1/0 values to True/False
getBool :: Float -> Bool
getBool a = if a == 0 then False else True

-- check if the output is a Str
isStr :: Expr -> Bool
isStr (Str e) = True
isStr (Constant e) = False
isStr (Var e) = False
isStr (Plus e1 e2) = False
isStr (Minus e1 e2) = False
isStr (Times e1 e2) = False
isStr (Div e1 e2) = False
isStr (LT_ e1 e2) = False
isStr (GT_ e1 e2) = False
isStr (GE_ e1 e2) = False
isStr (LE_ e1 e2) = False
isStr (EQ_ e1 e2) = False
isStr (NEQ_ e1 e2) = False

-- evaluate Str and return a String
showStr :: Expr -> String
showStr (Str e) = e

showOut :: [Expr] -> String -> SymTable -> String
showOut [] output env = output
showOut (first:rest) output env = if (isStr first) then 
    (let output1 = ((tail (init (showStr first)))) in showOut rest (output++output1++" ") env) else 
    (let output1 = eval first env in showOut rest (output++(show output1)++" ") env)
-- splitOn "\"" (showStr first)

-- given a statement, a ST, line number, input and previous output, return an updated ST, input, output, and line number
-- this starter version ignores the input and line number
-- Stmt, SymTable, progCounter, input, output, (SymTable', input', output', progCounter)
perform:: Stmt -> SymTable -> Float -> [String] ->String -> (SymTable, [String], String, Float)
perform (Print e) env lineNum input output = (env, input, (output++(showOut e [] env)++"\n"), lineNum+1)
perform (Let id e) env lineNum input output = ((id,(eval e env)):env, input, output, lineNum+1)
perform (If e id) env lineNum input output = 
    if (getBool (eval e env)) then (env, input, output, lookupVar (id++":") env) 
    else (env, input, output, lineNum+1)
perform (Input id) env lineNum (first:rest) output = ((id,(eval (Constant (read first::Float)) env)):env, rest, output, lineNum+1)
-- perform (Print e) env lineNum input output = (env, input, output++(show (eval (head e) env)++"\n"), lineNum+1)

-- convert a Float to an Int
toInt :: Float -> Int
toInt a = round a

-- given a list of Stmts, a ST, and current output, perform all of the statements in the list and return the updated output String
-- list of stmt, lineNum, SymTable, [input], output
run :: [Stmt] -> Float -> SymTable -> [String] -> String -> String
run [] _ _ _ output = output
run all lineNum env input output = 
    let (env1, input1, output1, lineNum1) = perform (all !! ((toInt lineNum)-1)) env lineNum input output in 
             (if (toInt lineNum1)<=(length all) then (run all lineNum1 env1 input1 output1) else (run [] lineNum1 env1 input1 output1))

-- given list of list of tokens, a ST, and return the list of parsed Stmts and ST storing mapping of labels to line numbers
parseTest :: [[String]] -> SymTable -> ([Stmt], SymTable)
parseTest []  st = ([], st)
parseTest the_map env = parseAll the_map 1 env []

-- Function to make a symTable
makeEnv :: SymTable -> SymTable
makeEnv env = env

-- Function to make a list of Stmt
makeStmt :: [Stmt] -> [Stmt]
makeStmt st = st

-- [[string]] -> program counter -> symtable -> current Stmt -> a list of Stmt
parseAll :: [[String]] -> Float -> SymTable -> [Stmt] -> ([Stmt], SymTable)
parseAll [] _ env output = (output, env)
parseAll (first:rest) pc env output = 
    let (statement, env1) = parseLine first pc env in parseAll rest (pc+1) env1 (output++[statement])

main = do
     -- read random file from command line
     args <- getArgs
     pfile <- openFile (head args) ReadMode
     contents <- hGetContents pfile
     -- input from standard in
     x <- getContents
     let xs = words x

     let the_map = map words (lines contents)
     let the_zip = zip [1..] the_map
     let env = makeEnv []
     let stmtAll = makeStmt []
     let (stmtAll, envAll) = parseAll the_map 1 env []
     let l = length the_map
     putStr (run stmtAll 1 envAll xs "")
     hClose pfile