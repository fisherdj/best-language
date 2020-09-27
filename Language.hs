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
  | LLam (LData -> LData)

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
  show (LLam _) = "<procedure>"

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

appendEnv :: [(LData,LData)] -> LData -> LData
appendEnv l env =
  foldr LCons env (map (\(x,y) -> LCons x y) l)

makeEnv l = appendEnv l LNil

apply :: LData -> LData -> LData
apply (LLam f) arg = f arg
apply x _ = error ("Cannot apply " ++ show x)

isTrue :: LData -> Bool
isTrue LFalse = False
isTrue _ = True

fromBool False = LFalse
fromBool True = LTrue

acons :: LData -> LData -> LData -> LData
acons k v e = LCons (LCons k v) e

aref k (LCons (LCons ik iv) e) =
  if k == ik
  then Just iv
  else aref k e
aref _ _ = Nothing

patMatch :: LData -> LData -> Maybe [(LData,LData)]
patMatch (LSym s) arg = Just [(LSym s,arg)]
patMatch (LCons pa pb) (LCons aa ab) =
      case (patMatch pa aa,patMatch pb ab) of
        (Just la,Just lb) -> Just (la ++ lb)
        _ -> Nothing
patMatch (LCons _ _) _ = Nothing
patMatch x y = if x == y then Just [] else Nothing

makeLambdaProc :: LData -> LData -> LData -> LData -> LData
makeLambdaProc argPat body env actualArg =
  case patMatch argPat actualArg of
    Just l -> eval body (appendEnv l env)
    Nothing -> error ("Invalid argumentl list: got\n" ++ show actualArg ++ "\nfor pattern\n" ++ show argPat)

makeLambdaRecProc :: LData -> LData -> LData -> LData -> LData -> LData
makeLambdaRecProc name argPat body env actualArg =
  (makeLambdaProc
   argPat
   body
   (acons name (LLam (makeLambdaRecProc name argPat body env)) env)
  actualArg)

makeLambdaRec :: LData -> LData -> LData -> LData -> LData
makeLambdaRec name argPat body env  =
  (LLam (makeLambdaRecProc name argPat body env))

evalArgs :: LData -> LData -> LData
evalArgs (LCons a b) env = LCons (eval a env) (evalArgs b env)
evalArgs LNil env = LNil

eval :: LData -> LData -> LData
eval (LCons (LSym "if") args) env =
  case args of
    (LCons c (LCons t (LCons e LNil))) ->
      if isTrue (eval c env)
      then eval t env
      else eval e env
    _ -> error ("eval: if got invalid argument list" ++ show args)
eval (LCons (LSym "quote") args) env =
  case args of
    (LCons x LNil) -> x
    _ -> error ("eval: quote got invalid argument list" ++ show args)
eval (LCons (LSym "define") args) env =
  case args of
    (LCons name (LCons valExp LNil)) -> acons name (eval valExp env) env
    _ -> error ("eval: define got invalid argument list " ++ show args)
eval (LCons (LSym "lambda") args) env =
  case args of
    (LCons argPat (LCons body LNil)) ->
      (LLam (makeLambdaProc argPat body env))
eval (LCons (LSym "lambda-rec") args) env =
  case args of
    (LCons name (LCons argPat (LCons body LNil))) ->
      (makeLambdaRec name argPat body env)
eval (LCons funExp args) env =
  apply (eval funExp env) (evalArgs args env)
eval (LSym s) env =
  case aref (LSym s) env of
    Nothing -> error ("eval Unbound variable " ++ s ++ " in " ++ show env)
    Just x -> x
eval x _ = x

asList :: LData -> Maybe [LData]
asList (LNil) = Just []
asList (LCons a b) =
  case asList b of
    Nothing -> Nothing
    Just t -> Just (a:t)

primRecordProc :: String -> Int -> ([LData] -> LData) -> LData -> LData
primRecordProc name nArgs f =
  \argl -> case asList argl of
            Nothing -> error ("Primitive " ++ name ++ " did not get a proper argument list" ++ show argl)
            Just l ->
              if length l == nArgs
              then f l
              else error ("Primitive " ++ name ++ " expected " ++ show nArgs ++ " arguments but got " ++ show (length l))

primRecord :: String -> Int -> ([LData] -> LData) -> (LData,LData)
primRecord name nArgs f =
  (LSym name, LLam (primRecordProc name nArgs f))

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

defaultEnv :: LData
defaultEnv =
  makeEnv [primRecord "cons" 2 (\[x,y] -> LCons x y)
          ,primRecord "=" 2 (\[x,y] -> fromBool (x == y))
          ,primRecord "pair?" 1 pairp
          ,primRecord "number?" 1 numberp
          ,primRecord "symbol?" 1 symbolp
          ,primRecord "string?" 1 stringp
          ,primRecord "null?" 1 nullp
          ,primRecord "car" 1 (\[LCons a b] -> a)
          ,primRecord "cdr" 1 (\[LCons a b] -> a)
          ,primRecord "+" 2 (\[LInt x,LInt y] -> LInt (x+y))
          ,primRecord "-" 2 (\[LInt x,LInt y] -> LInt (x - y))
          ,primRecord "/" 2 (\[LInt x,LInt y] -> LInt (div x y))
          ,primRecord "%" 2 (\[LInt x,LInt y] -> LInt (rem x y))
          ,primRecord "*" 2 (\[LInt x,LInt y] -> LInt (x*y))
          ]

evalTop :: [LData] -> LData -> LData
evalTop l env = foldl (\ienv iexp -> eval iexp ienv) env l

mainArgs :: [String] -> LData -> IO ()
mainArgs [] env = return ()
mainArgs ("-e":expString:rest) env = do {
    print (evalTop (readForest expString) env);
    mainArgs rest env;
  }
mainArgs ("-f":fileName:rest) env = do {
    file <- readFile fileName;
    mainArgs rest (evalTop (readForest file) env);
  }

main :: IO ()
main = do {
    args <- getArgs;
    mainArgs args defaultEnv;
  }
