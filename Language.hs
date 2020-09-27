{-# LANGUAGE OverloadedStrings #-}
import Data.Char
import System.Environment

digitInt :: Char -> Maybe Integer
digitInt '0' = Just 0
digitInt '1' = Just 1
digitInt '2' = Just 2
digitInt '3' = Just 3
digitInt '4' = Just 4
digitInt '5' = Just 5
digitInt '6' = Just 6
digitInt '7' = Just 7
digitInt '8' = Just 8
digitInt '9' = Just 9
digitInt _ = Nothing

digitString :: String -> Maybe Integer --- sorry, we're not doing this polymorphic
digitStringLE [] = Just 0
digitStringLE (x:s) = -- I don't want to hear anything about monads here
  case (digitInt x,digitStringLE s) of
    (Just d,Just n) -> Just (d + n * 10)
    (_,_) -> Nothing

digitString s = digitStringLE (reverse s)

-- do { d <- digitInt x; n <- digitString s; return (d+n*10) }
-- ^ this is not suitable for our purposes, but I do know how to do it

numString "-" = Nothing
numString ('-':s) =
  case digitString s of
    Nothing -> Nothing
    Just n -> Just (-n)
numString s = digitString s

data LData =
  LNil
  | LTrue
  | LFalse
  | LSym String
  | LStr String
  | LInt Integer
  | LCons LData LData
  | LVau (LData -> LData -> LRet)

instance (Eq LData) where
  LNil == LNil = True
  LTrue == LTrue = True
  LFalse == LFalse = True
  LSym s1 == LSym s2 = s1 == s2
  LStr s1 == LStr s2 = s1 == s2
  LInt n1 == LInt n2 = n1 == n2
  LCons a1 b1 == LCons a2 b2 = a1 == a2 && b1 == b2
  _ == _ = False

-- First, the writer, so we can debug:
instance Show LData where
  show LNil = "()"
  show LTrue = "#t"
  show LFalse = "#f"
  show (LSym s) = s
  show (LStr s) = show s
  show (LInt n) = show n
  -- show (LCons a b) = "(" ++ show a ++ " . " ++ show b ++ ")"
  show (LCons a b) = "(" ++ show a ++ showcdr b
  -- show (LLam _) = "<procedure>"
  show (LVau _) = "<vau-combiner>"

data LRet = LRRet LData | LREff LData (LData -> LRet)

instance Show LRet where
  show (LRRet d) = "RETURN " ++ show d
  show (LREff eff cont) = "EFFECT " ++ show eff

showcdr (LNil) = ")"
showcdr (LCons a b) = " " ++ show a ++ showcdr b
showcdr x = " . " ++ show x ++ ")"

nextLine :: String -> String
nextLine [] = []
nextLine ('\n':rest) = rest
nextLine (_:rest) = nextLine rest

filterComments :: String -> String
filterComments [] = []
filterComments (';':rest) = filterComments (nextLine rest)
filterComments (x:rest) = x:filterComments rest

-- skipSpace might be a better idea in a real reader
readForestC :: String -> [LData]
readForestC [] = []
readForestC (x:rest) =
  if isSpace x
  then readForest rest
  else let (exp,rmd) = readExpr (x:rest) in (exp:readForest rmd)

readForest s = readForestC (filterComments s)

-- don't have dotted lists implemented
readLList :: String -> (LData,String)
readLList (')':rest) = (LNil,rest)
readLList (x:rest) =
  if isSpace x
  then readLList rest
  else let (exp,rmd) = readExpr (x:rest)
           (rrest,rrmd) = readLList rmd in (LCons exp rrest, rrmd)
readLList [] = error "readLList ended early"

readSymNumContents :: String -> (String,String)
readSymNumContents [] = ([],[])
readSymNumContents ('(':rest) = ([],'(':rest)
readSymNumContents (')':rest) = ([],')':rest)
readSymNumContents ('#':rest) = ([],'#':rest)
readSymNumContents ('"':rest) = ([],'"':rest)
readSymNumContents (x:rest) =
  if isSpace x
  then ([],x:rest)
  else let (str,rmd) = readSymNumContents rest in (x:str,rmd)

readSymNum s =
  let (rd,rmd) = readSymNumContents s in
    case numString rd of
      Nothing -> (LSym rd,rmd)
      Just x -> (LInt x,rmd)

readExpr :: String -> (LData,String)
readExpr ('#':rest) = readHash rest
readExpr ('(':rest) = readLList rest
readExpr (')':rest) =
  error "readExpr got spurious closing delimiter"
readExpr ('"':rest) =
  let (s,rmd) = readString rest in (LStr s,rmd)
readExpr ('\'':rest) =
  let (s,rmd) = readExpr rest in (LCons (LSym "quote") (LCons s LNil),rmd)
readExpr (x:rest) =
  if isSpace x
  then readExpr rest
  else readSymNum (x:rest)
readExpr [] = error "readExpr got nothing"

readHash ('t':rest) = (LTrue,rest)
readHash ('f':rest) = (LFalse,rest)

-- EXCELLENT CHOICE OF NAMES HERE
readString ('\\':escaped:rest) =
  let (restStr,rmd) = readString rest in
    case escaped of 'n' -> ('\n':restStr,rmd)
                    '"' -> ('"':restStr,rmd)
                    _ -> error "readString unrecognized escape"
readString ('"':rest) = ([],rest)
readString (x:rest) =
  let (s,rmd) = readString rest in (x:s,rmd)

---- ************************ ----
---- *EVALUATION STARTS HERE* ----
---- ************************ ----

-- this is almost certainly actually bind
continue :: LRet -> (LData -> LRet) -> LRet
continue (LREff eff bCont) cont =
  LREff eff (\x -> continue (bCont x) cont)
continue (LRRet d) cont = cont d

combine :: LData -> LData -> LData -> LRet
combine (LVau f) arg env = f arg env
combine f _ _ = error ("Invalid combiner " ++ show f)

combineR :: LRet -> LData -> LData -> LRet
combineR (LREff eff cont) arg env =
  LREff eff (\x -> combineR (cont x) arg env)
combineR (LRRet r) arg env = combine r arg env

-- continue as defined above is "mathematically elegant"
-- but it's not very comprehensible to somebody not familiar with Haskell's monads
-- I think I'll avoid it in lieu of manual recursion, but I might try both

eval :: LData -> LData -> LRet
eval (LCons funExp args) env =
  combineR (eval funExp env) args env
eval (LSym s) env =
  case aref (LSym s) env of
    Nothing -> error ("eval Unbound variable " ++ s ++ " in " ++ show env)
    Just x -> LRRet x
eval x _ = LRRet x

evalIfR (LRRet LFalse) _ e env = eval e env
evalIfR (LRRet _) t _ env = eval t env
evalIfR (LREff eff cont) t e env =
  LREff eff (\x -> evalIfR (cont x) t e env)

evalIf c t e env = evalIfR (eval c env) t e env

evalArgsRR :: LRet -> [LData] -> [LData] -> LData -> LRet
evalArgsRR (LREff eff cont) l prev env =
  LREff eff (\x -> evalArgsRR (cont x) l prev env)
evalArgsRR (LRRet r) l prev env =
  evalArgsR l (r:prev) env

evalArgsR :: [LData] -> [LData] -> LData -> LRet
evalArgsR (a:b) prev env = evalArgsRR (eval a env) b prev env
evalArgsR [] prev _ = LRRet (foldr LCons LNil (reverse prev))

evalArgs :: [LData] -> LData -> LRet
evalArgs l env = evalArgsR l [] env

-- using continue

evalC :: LData -> LData -> LRet
evalC (LCons funExp args) env =
  continue (eval funExp env) (\f -> combine f args env)
evalC (LSym s) env =
  case aref (LSym s) env of
    Nothing -> error ("eval Unbound variable " ++ s ++ " in " ++ show env)
    Just x -> LRRet x
evalC x _ = LRRet x

evalIfC c t e env =
  continue (eval c env) (\x -> if isTrue x then (eval t env) else (eval e env))

evalArgsC :: [LData] -> LData -> LRet
evalArgsC (a:b) env =
  continue (eval a env) (\x -> continue (evalArgs b env) (\y -> LRRet (LCons x y)))

appendEnv :: [(LData,LData)] -> LData -> LData
appendEnv l env =
  foldr LCons env (map (\(x,y) -> LCons x y) l)

makeEnv l = appendEnv l LNil

fromBool False = LFalse
fromBool True = LTrue

acons :: LData -> LData -> LData -> LData
acons k v e = LCons (LCons k v) e

aref k (LCons (LCons ik iv) e) =
  if k == ik
  then Just iv
  else aref k e
aref _ _ = Nothing

-- it's possible
patMatch :: LData -> LData -> Maybe [(LData,LData)]
patMatch (LSym s) arg = Just [(LSym s,arg)]
patMatch (LCons pa pb) (LCons aa ab) =
      case (patMatch pa aa,patMatch pb ab) of
        (Just la,Just lb) -> Just (la ++ lb)
        _ -> Nothing
patMatch (LCons _ _) _ = Nothing
patMatch x y = if x == y then Just [] else Nothing

makeVauProc :: LData -> LData -> LData -> LData -> LData -> LData -> LRet
makeVauProc argPat envArg body env actualArg actualEnv =
  case (envArg,patMatch argPat actualArg) of
    (_,Nothing) -> error ("Invalid argumentl list: got\n" ++ show actualArg ++ "\nfor pattern\n" ++ show argPat)
    (LFalse,Just l) -> eval body (appendEnv l env)
    (_,Just l) -> eval body (appendEnv l (acons envArg actualEnv env))

makeVau :: LData -> LData -> LData -> LData -> LData
makeVau argPat envArg body env =
  LVau (makeVauProc argPat envArg body env)

makeVauRecProc :: LData -> LData -> LData -> LData -> LData -> LData -> LData -> LData -> LRet
makeVauRecProc name argPat envArg body env recur actualArg actualEnv =
  makeVauProc argPat envArg body (acons name recur env) actualArg actualEnv

makeVauRec :: LData -> LData -> LData -> LData -> LData -> LData
makeVauRec name argPat envArg body env =
  let recProc = (makeVauRecProc name argPat envArg body env)
      recComb = LVau (recProc recComb) in
    recComb

-- left in commit for Haskell
-- makeVauRecProcAlt :: LData -> LData -> LData -> LData -> LData -> LData -> LData -> LData
-- makeVauRecProcAlt name argPat envArg body env actualArg actualEnv =
--   (makeVauProc
--    argPat
--    envArg
--    body
--    (acons name (LVau (makeVauRecProcAlt name argPat envArg body env)) env)
--   actualArg
--   actualEnv)

-- makeVauRecAlt :: LData -> LData -> LData -> LData -> LData -> LData
-- makeVauRecAlt name argPat envArg body env =
--   (LVau (makeVauRecProcAlt name argPat envArg body env))

asList :: LData -> Maybe [LData]
asList (LNil) = Just []
asList (LCons a b) =
  case asList b of
    Nothing -> Nothing
    Just t -> Just (a:t)
asList x = error ("asList got " ++ show x)

pairp [LCons _ _] = LRRet LTrue
pairp _ = LRRet LFalse

numberp [LInt _] = LRRet LTrue
numberp _ = LRRet LFalse

symbolp [LSym _] = LRRet LTrue
symbolp _ = LRRet LFalse

stringp [LStr _] = LRRet LTrue
stringp _ = LRRet LFalse

nullp [LNil] = LRRet LTrue
nullp _ = LRRet LFalse

isTrue :: LData -> Bool
isTrue LFalse = False
isTrue _ = True

primCombProc :: String -> Int -> ([LData] -> LData -> LRet) -> LData -> LData -> LRet
primCombProc name nArgs f argl env =
  case asList argl of
    Nothing -> error ("Primitive " ++ name ++ " did not get a proper argument list" ++ show argl)
    Just l ->
      if length l == nArgs
      then f l env
      else error ("Primitive " ++ name ++ " expected " ++ show nArgs ++ " arguments but got " ++ show (length l))

primCombRecord :: String -> Int -> ([LData] -> LData -> LRet) -> (LData,LData)
primCombRecord name nArgs f =
  (LSym name, LVau (primCombProc name nArgs f))

primRecordL f (Just l) = f l
primRecordL f Nothing = error "Impossible Error"

primRecord :: String -> Int -> ([LData] -> LRet) -> (LData,LData)
primRecord name nArgs f =
  primCombRecord name nArgs (\argl env -> continue (evalArgs argl env) (\x -> primRecordL f (asList x)))

contVau cont =
  LVau (\(LCons exp LNil) env -> continue (eval exp env) cont)

withCapture [var,cVar,cap,body] env =
  case eval body env of
    LRRet r -> LRRet r
    LREff eff cont ->
      eval cap (acons var eff (acons cVar (contVau cont) env))

defaultEnv :: LData
defaultEnv =
  makeEnv [primRecord "cons" 2 (\[x,y] -> LRRet (LCons x y))
          ,primRecord "=" 2 (\[x,y] -> LRRet (fromBool (x == y)))
          ,primRecord "eval" 2 (\[exp,env] -> eval exp env)
          ,primCombRecord "vau" 3 (\[argPat,envArg,body] env -> LRRet (makeVau argPat envArg body env))
          ,primCombRecord "vau-rec" 4 (\[name,argPat,envArg,body] env -> LRRet (makeVauRec name argPat envArg body env))
          ,primCombRecord "if" 3 (\[c,t,e] env -> evalIf c t e env)
          -- may need to be changed if eager evaluation occurs
          ,primRecord "effect" 1 (\[eff] -> LREff eff LRRet)
          ,primCombRecord "capture" 4 withCapture
          ,primRecord "pair?" 1 pairp
          ,primRecord "number?" 1 numberp
          ,primRecord "symbol?" 1 symbolp
          ,primRecord "string?" 1 stringp
          ,primRecord "null?" 1 nullp
          ,primRecord "car" 1 (\[LCons a b] -> LRRet a)
          ,primRecord "cdr" 1 (\[LCons a b] -> LRRet b)
          ,primRecord "+" 2 (\[LInt x,LInt y] -> LRRet (LInt (x+y)))
          ,primRecord "-" 2 (\[LInt x,LInt y] -> LRRet (LInt (x - y)))
          ,primRecord "/" 2 (\[LInt x,LInt y] -> LRRet (LInt (div x y)))
          ,primRecord "%" 2 (\[LInt x,LInt y] -> LRRet (LInt (rem x y)))
          ,primRecord "*" 2 (\[LInt x,LInt y] -> LRRet (LInt (x*y)))
          ]

evalEffD :: LData -> (LData -> LRet) -> IO LRet
evalEffD (LSym "read") cont = do {
    ln <- getLine;
    return (cont (fst (readExpr ln)))
  }
evalEffD (LCons (LSym "write") (LCons x LNil)) cont = do {
    print x;
    return (cont LNil);
  }
evalEffD eff cont = error ("Unrecognized effect: " ++ show eff)

evalEff :: LData -> (LData -> LRet) -> IO LData
evalEff eff cont = do {
    result <- evalEffD eff cont;
    case result of
      LRRet r -> return r
      LREff eff cont -> evalEff eff cont
  }

-- evalTop :: LData -> LData -> IO LDat
-- evalTop exp env = return (eval exp env)

evalTop :: [LData] -> LData -> IO LData
evalTop [] env = return env
evalTop (a:b) env = do {
  -- print a;
  case eval a env of
    LRRet val -> evalTop b val
    LREff eff cont -> evalEff eff cont
  }

mainArgs :: [String] -> LData -> IO ()
mainArgs [] env = return ()
mainArgs ("-e":expString:rest) env = do {
    result <- (evalTop (readForest expString) env);
    print result;
    mainArgs rest env;
  }
mainArgs ("-f":fileName:rest) env = do {
    file <- readFile fileName;
    result <- evalTop (readForest file) env;
    mainArgs rest result
  }

main :: IO ()
main = do {
    args <- getArgs;
    mainArgs args defaultEnv;
  }
