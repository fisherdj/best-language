{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
import Data.Char
import System.Environment
import Control.Monad

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

digitString :: String -> Maybe Integer
digitStringLE [] = Just 0
digitStringLE (x:s) =
  case (digitInt x,digitStringLE s) of
    (Just d,Just n) -> Just (d + n * 10)
    (_,_) -> Nothing

digitString s = digitStringLE (reverse s)

numString "-" = Nothing
numString ('-':s) =
  case digitString s of
    Nothing -> Nothing
    Just n -> Just (-n)
numString s = digitString s

---- ******************* ----
---- *THE MAIN DATATYPE* ----
---- ******************* ----
data LData =
  LNil
  | LTrue
  | LFalse
  | LSym String
  | LStr String
  | LInt Integer
  | LCons LData LData
  | LVau LData LData LData LData LData
  | LPrim (LData -> LData -> LRet LData)
  | LCont (LData -> LRet LData)

instance Eq LData where
  LNil == LNil = True
  LTrue == LTrue = True
  LFalse == LFalse = True
  LSym s1 == LSym s2 = s1 == s2
  LStr s1 == LStr s2 = s1 == s2
  LInt n1 == LInt n2 = n1 == n2
  LCons a1 b1 == LCons a2 b2 = a1 == a2 && b1 == b2
  LVau a1 b1 c1 d1 e1 == LVau a2 b2 c2 d2 e2 =
    [a1,b1,c1,d1,e1] == [a2,b2,c2,d2,e2]
  _ == _ = False

instance Show LData where
  show LNil = "()"
  show LTrue = "#t"
  show LFalse = "#f"
  show (LSym s) = s
  show (LStr s) = show s
  show (LInt n) = show n
  show (LCons a b) = "(" ++ show a ++ showcdr b
  show (LVau _ _ _ _ _) = "<combiner>"
  show (LPrim _) = "<primitive>"
  show (LCont _) = "<continuation>"

data LRet a = LRRet a | LREff LData (LData -> LRet a)

instance Functor LRet where
  fmap = liftM

instance Applicative LRet where
  pure = LRRet
  (<*>) = ap

instance Monad LRet where
  return = pure
  LRRet x >>= f = f x
  LREff e c >>= f = LREff e (\x -> c x >>= f)

instance Show a => Show (LRet a) where
  show (LRRet x) = "RETURN " ++ show x
  show (LREff eff cont) = "EFFECT " ++ show eff

showcdr (LNil) = ")"
showcdr (LCons a b) = " " ++ show a ++ showcdr b
showcdr x = " . " ++ show x ++ ")"

---- ************ ----
---- *THE PARSER* ----
---- ************ ----
filterComments :: String -> String
filterComments [] = []
filterComments (';':rest) = filterComments (nextLine rest)
filterComments (x:rest) = x:filterComments rest

nextLine :: String -> String
nextLine [] = []
nextLine ('\n':rest) = rest
nextLine (_:rest) = nextLine rest

readForestC :: String -> [LData]
readForestC [] = []
readForestC (x:rest) =
  if isSpace x
  then readForest rest
  else let (exp,rmd) = readExpr (x:rest) in (exp:readForest rmd)

readForest s = readForestC (filterComments s)

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

readHash ('t':rest) = (LTrue,rest)
readHash ('f':rest) = (LFalse,rest)

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
acons :: LData -> LData -> LData -> LData
acons k v e = LCons (LCons k v) e

aref k (LCons (LCons ik iv) e) =
  if k == ik
  then Just iv
  else aref k e
aref _ _ = Nothing

patMatch :: LData -> LData -> LData -> Maybe LData
patMatch (LSym s) arg env =
  Just (acons (LSym s) arg env)
patMatch (LCons pa pb) (LCons aa ab) env =
  case patMatch pa aa env of
    Just ea -> patMatch pb ab ea
    Nothing -> Nothing
patMatch x y env =
  if x == y then Just env else Nothing

combine :: LData -> LData -> LData -> LRet LData
combine comb argl env =
  case comb of
    LPrim f -> f argl env
    LCont f ->
      case argl of
        LCons arg LNil -> eval arg env >>= f
        _ -> error ("Invalid argument list to continuation: " ++ show argl)
    LVau rarg argpat earg body venv ->
      case patMatch argpat argl (acons earg env (acons rarg comb venv)) of
        Nothing ->
          error ("Invalid argument list:" ++ show argl ++ "\n"
                 ++ "Argument pattern is:" ++ show argpat)
        Just e -> eval body e
    _ -> error ("Invalid combiner " ++ show comb)

eval :: LData -> LData -> LRet LData
eval (LCons funExp args) env =
  eval funExp env >>= (\f -> combine f args env)
eval (LSym s) env =
  case aref (LSym s) env of
    Nothing -> error ("eval Unbound variable " ++ s ++ " in " ++ show env)
    Just x -> LRRet x
eval x _ = LRRet x

class PrimOp a where
  getPrim :: String -> a -> LData -> LData -> LRet LData

instance PrimOp (LRet LData) where
  getPrim _ x LNil _ = x
  getPrim name _ y _ =
    error ("Primitive " ++ name ++ " got extra arguments: " ++ show y)

instance PrimOp LData where
  getPrim name x = getPrim name (LRRet x)

instance PrimOp Integer where
  getPrim name n = getPrim name (LInt n)

instance PrimOp Bool where
  getPrim name True = getPrim name LTrue
  getPrim name False = getPrim name LFalse

instance PrimOp a => PrimOp (LData -> a) where
  getPrim name f (LCons x xs) env =
    eval x env >>= \x -> getPrim name (f x) xs env
  getPrim name f LNil _ =
    error ("Primitive " ++ name  ++ " got too few arguments")

instance PrimOp a => PrimOp (Integer -> a) where
  getPrim name f =
    getPrim name (\x ->
                    case x of
                      LInt n -> f n
                      _ -> error ("Primitive " ++ name
                                  ++ " got non-numeric argument " ++ show x))

primRecord name f = (name,getPrim name f)

makePrimEnv :: [(String,LData -> LData -> LRet LData)] -> LData
makePrimEnv ((name,fun):xs) = acons (LSym name) (LPrim fun) (makePrimEnv xs)
makePrimEnv [] = LNil

primIf (LCons c (LCons t (LCons e LNil))) env = do
  x <- eval c env;
  case x of
    LFalse -> eval e env
    LTrue -> eval t env

primIf x _ =
  error ("Primitive if got invalid argument list: " ++ show x)

primVauRec (LCons name (LCons argPat (LCons envArg (LCons body LNil)))) env =
  LRRet (LVau name argPat envArg body env)
primVauRec x _ =
  error ("Primitive primVauRec got invalid argument list: " ++ show x)

initEnv :: LData
initEnv =
  makePrimEnv
    [primRecord "cons" LCons
    ,primRecord "=" ((==) :: LData -> LData -> Bool)
    ,primRecord "eval" eval
    ,("vau",primVauRec . LCons LFalse)
    ,("vau-rec",primVauRec)
    ,("if",primIf)
    ,primRecord "effect" (\x -> LREff x LRRet)
    ,primRecord "eval-capture" (\exp env -> capture (eval exp env))
    ,primRecord "pair?" (\x -> case x of {LCons _ _ -> True;_ -> False})
    ,primRecord "number?" (\x -> case x of {LInt _ -> True;_ -> False})
    ,primRecord "symbol?" (\x -> case x of {LSym _ -> True;_ -> False})
    ,primRecord "string?" (\x -> case x of {LStr _ -> True;_ -> False})
    ,primRecord "null?" (\x -> case x of {LNil -> True;_ -> False})
    ,primRecord "car" (\(LCons a _) -> a)
    ,primRecord "cdr" (\(LCons _ b) -> b)
    ,primRecord "+" ((+) :: Integer -> Integer -> Integer)
    ,primRecord "-" ((-) :: Integer -> Integer -> Integer)
    ,primRecord "/" (div :: Integer -> Integer -> Integer)
    ,primRecord "%" (mod :: Integer -> Integer -> Integer)
    ,primRecord "*" ((*) :: Integer -> Integer -> Integer)
    ]

capture (LRRet r) = LCons (LSym "value") r
capture (LREff eff cont) = LCons (LSym "effect") (LCons eff (LCont cont))

evalTop :: LData -> [LData] -> LRet LData
evalTop = foldM eval

mainHandle :: LRet LData -> IO LData
mainHandle (LRRet r) = return r
mainHandle (LREff (LSym "read") cont) =
    getLine >>= mainHandle . cont . fst . readExpr
mainHandle (LREff (LCons (LSym "write") (LCons x LNil)) cont) =
    print x >> mainHandle (cont LNil)

mainArgs :: LData -> [String] -> IO ()
mainArgs env [] = return ()
mainArgs env ("-e":expString:rest) = do {
    mainHandle (evalTop env (readForest expString)) >>= print;
    mainArgs env rest
  }
mainArgs env ("-f":fileName:rest) = do {
    file <- readFile fileName;
    result <- mainHandle (evalTop env (readForest file));
    mainArgs result rest
  }

main :: IO ()
main = getArgs >>= mainArgs initEnv
