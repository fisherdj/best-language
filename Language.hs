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
  | LVau (LCont -> LData -> LData -> LRet)

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

data LRet = LRRet LData | LREff LData LCont
type LCont = LData -> LRet

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
readHash [] = error "readHash got nothing"
readHash _ = error "readHash got invalid"

-- EXCELLENT CHOICE OF NAMES HERE
readString ('\\':escaped:rest) =
  let (restStr,rmd) = readString rest in
    case escaped of 'n' -> ('\n':restStr,rmd)
                    '"' -> ('"':restStr,rmd)
                    _ -> error "readString unrecognized escape"
readString ('"':rest) = ([],rest)
readString (x:rest) =
  let (s,rmd) = readString rest in (x:s,rmd)
readString [] =
  error "readString got nothing"

---- ************************ ----
---- *EVALUATION STARTS HERE* ----
---- ************************ ----
primError :: (LData -> LRet) -> String -> LRet
primError k s = LREff (LCons (LSym "prim-error") (LStr s)) k

-- this is almost certainly actually bind
continue :: LRet -> (LData -> LRet) -> LRet
continue (LREff eff bCont) cont =
  LREff eff (\x -> continue (bCont x) cont)
continue (LRRet d) cont = cont d

combine :: LCont -> LData -> LData -> LData -> LRet
combine k (LVau f) arg env = f k arg env
combine k f _ _ = primError k ("Invalid combiner " ++ show f)

eval :: LCont -> LData -> LData -> LRet
eval k (LCons funExp args) env =
  eval (\f -> combine k f args env) funExp env
eval k (LSym s) env =
  case aref (LSym s) env of
    Nothing -> primError k ("eval Unbound variable " ++ s ++ " in " ++ show env)
    Just x -> k x
eval k x _ = k x

evalIf k c t e env =
  eval (\x -> if isTrue x then eval k t env else eval k e env) c env

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

-- it's possible to do this in the language itself
patMatch :: LData -> LData -> Maybe [(LData,LData)]
patMatch (LSym s) arg = Just [(LSym s,arg)]
patMatch (LCons pa pb) (LCons aa ab) =
      case (patMatch pa aa,patMatch pb ab) of
        (Just la,Just lb) -> Just (la ++ lb)
        _ -> Nothing
patMatch (LCons _ _) _ = Nothing
patMatch x y = if x == y then Just [] else Nothing

makeVauProc :: LData -> LData -> LData -> LData -> LCont -> LData -> LData -> LRet
makeVauProc argPat envArg body env k actualArg actualEnv =
  case (envArg,patMatch argPat actualArg) of
    (_,Nothing) -> primError k ("Invalid argumentl list: got\n" ++ show actualArg ++ "\nfor pattern\n" ++ show argPat)
    (LFalse,Just l) -> eval k body (appendEnv l env)
    (_,Just l) -> eval k body (appendEnv l (acons envArg actualEnv env))

makeVau :: LData -> LData -> LData -> LData -> LData
makeVau argPat envArg body env =
  LVau (makeVauProc argPat envArg body env)

makeVauRecProc :: LData -> LData -> LData -> LData -> LData -> LData -> LCont -> LData -> LData -> LRet
makeVauRecProc name argPat envArg body env recur k actualArg actualEnv =
  makeVauProc argPat envArg body (acons name recur env) k actualArg actualEnv

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

pairp [LCons _ _] = LTrue
pairp _ = LFalse

numberp [LInt _] = LTrue
numberp _ = LFalse

symbolp [LSym _] = LTrue
symbolp _ = LFalse

stringp [LStr _] = LTrue
stringp _ = LFalse

nullp [LNil] = LTrue
nullp _ = LFalse

isTrue :: LData -> Bool
isTrue LFalse = False
isTrue _ = True

asList :: LData -> Maybe [LData]
asList (LNil) = Just []
asList (LCons a b) =
  case asList b of
    Just l -> Just (a:l)
    Nothing -> Nothing
asList _ = Nothing

primCombProc :: String -> Int -> (LCont -> [LData] -> LData -> LRet) -> LCont -> LData -> LData -> LRet
primCombProc name nArgs f k argl env =
  case asList argl of
    Nothing -> primError k ("Primitive " ++ name ++ " did not get a proper argument list" ++ show argl)
    Just l ->
      if length l == nArgs
      then f k l env
      else primError k ("Primitive " ++ name ++ " expected " ++ show nArgs ++ " arguments but got " ++ show (length l))

primCombRecord :: String -> Int -> (LCont -> [LData] -> LData -> LRet) -> (LData,LData)
primCombRecord name nArgs f =
  (LSym name, LVau (primCombProc name nArgs f))

evalArgs :: ([LData] -> LRet) -> [LData] -> LData -> LRet
evalArgs k [] env = k []
evalArgs k (x:xs) env =
  eval (\r -> evalArgs (\rs -> k (r:rs)) xs env) x env

primRecord :: String -> Int -> ([LData] -> LData) -> (LData,LData)
primRecord name nArgs f =
  primCombRecord name nArgs (\k argl env -> evalArgs (\x -> (k (f x))) argl env)

composeCont :: LCont -> LCont -> LCont
composeCont k1 k2 ret =
  case k2 ret of
    LRRet x -> k1 x
    LREff eff k3 -> LREff eff (composeCont k1 k3)

contVau k1 =
  LVau (\k2 (LCons exp LNil) env -> (eval (composeCont k2 k1) exp env))

-- withCapture k [var,cVar,cap,body] env =
--   case eval LRRet body env of
--     LRRet r -> k r
--     LREff eff cont ->
--       eval k cap (acons var eff (acons cVar (contVau cont) env))

evalCapture [exp,env] =
  case eval LRRet exp env of
    LRRet r -> LCons (LSym "value") r
    LREff eff cont -> LCons (LSym "effect") (LCons eff (contVau cont))

evalPrim k [expArg,envArg] outerEnv =
  eval (\exp -> eval (\env -> eval k exp env) envArg outerEnv) expArg outerEnv

defaultEnv :: LData
defaultEnv =
  makeEnv [primRecord "cons" 2 (\[x,y] -> LCons x y)
          ,primRecord "=" 2 (\[x,y] -> fromBool (x == y))
          ,primCombRecord "eval" 2 evalPrim
          ,primCombRecord "vau" 3 (\k [argPat,envArg,body] env -> k (makeVau argPat envArg body env))
          ,primCombRecord "vau-rec" 4 (\k [name,argPat,envArg,body] env -> k (makeVauRec name argPat envArg body env))
          ,primCombRecord "if" 3 (\k [c,t,e] env -> evalIf k c t e env)
          -- may need to be changed if eager evaluation occurs
          ,primCombRecord "effect" 1 (\k [eff] env -> eval (\x -> LREff x k) eff env)
          ,primRecord "eval-capture" 2 evalCapture
          ,primRecord "pair?" 1 pairp
          ,primRecord "number?" 1 numberp
          ,primRecord "symbol?" 1 symbolp
          ,primRecord "string?" 1 stringp
          ,primRecord "null?" 1 nullp
          ,primRecord "car" 1 (\[LCons a b] -> a)
          ,primRecord "cdr" 1 (\[LCons a b] -> b)
          ,primRecord "+" 2 (\[LInt x,LInt y] -> LInt (x+y))
          ,primRecord "-" 2 (\[LInt x,LInt y] -> LInt (x - y))
          ,primRecord "/" 2 (\[LInt x,LInt y] -> LInt (div x y))
          ,primRecord "%" 2 (\[LInt x,LInt y] -> LInt (rem x y))
          ,primRecord "*" 2 (\[LInt x,LInt y] -> LInt (x*y))
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
  case eval LRRet a env of
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
