{-# LANGUAGE GADTs,FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- types
--T ::= int 
--    | bool 
--    | id 
--    | T + T 
--    | T - T 
--    | T * T 
--    | T / T 
--    | T ^ T 
--    | between T T T 
--    | lambda (id:TY) in T 
--    | T T 
--    | bind id T T 
--    | if T then T else T 
--    | T && T 
--    | T || T 
--    | T <= T 
--    | isZero T
--
--TY ::= Num 
--     | Boolean 
--     | TY -> TY

-- Abstract Syntax Definitions
data T where -- operators
  Int :: Int -> T
  Bool :: Bool -> T
  Id :: String -> T
  Add :: T -> T -> T
  Sub :: T -> T -> T
  Mult :: T -> T -> T
  Div :: T -> T -> T
  Pow :: T -> T -> T
  Between :: T -> T -> T -> T
  Lambda :: String -> TY -> T -> T
  App :: T -> T -> T
  Bind :: String -> T -> T -> T
  If :: T -> T -> T -> T
  And :: T -> T -> T
  Or :: T -> T -> T
  Leq :: T -> T -> T
  IsZero :: T -> T
  Fix :: T -> T
  deriving (Show, Eq)

data TY where -- types
  Num :: TY
  Boolean :: TY
  Arrow :: TY -> TY -> TY
  deriving (Show, Eq)

data TV where
  NumV :: Int -> TV
  BoolV :: Bool -> TV
  ClosureV :: String -> T -> EnvV -> TV
  deriving (Show, Eq)

-- Context
type Cont = [(String, TY)]

-- Environment
type Env = [(String, T)]
type EnvV = [(String, TV)]

-- Helper Functions
lookupVar :: String -> Cont -> Maybe TY
lookupVar _ [] = Nothing
lookupVar x ((y, t):cont) = if x == y then Just t else lookupVar x cont

useClosure :: String -> TV -> EnvV -> EnvV -> EnvV
useClosure i v e _ = (i,v):e 

subst :: String -> T -> T -> T
subst _ _ (Int i) = Int i
subst _ _ (Bool b) = Bool b
subst i t (Id x) = if i == x then t else Id x
subst i t (Add x y) = Add (subst i t x) (subst i t y)
subst i t (Sub x y) = Sub (subst i t x) (subst i t y)
subst i t (Mult x y) = Mult (subst i t x) (subst i t y)
subst i t (Div x y) = Div (subst i t x) (subst i t y)
subst i t (Pow x y) = Pow (subst i t x) (subst i t y)
subst i t (Between x y z) = Between (subst i t x) (subst i t y) (subst i t z)
subst i t (Lambda x y z) = if i == x then Lambda x y z else Lambda x y (subst i t z)
subst i t (App x y) = App (subst i t x) (subst i t y)
subst i t (Bind x y z) = if i == x then Bind x (subst i t y) z else Bind x (subst i t y) (subst i t z)
subst i t (If x y z) = If (subst i t x) (subst i t y) (subst i t z)
subst i t (And x y) = And (subst i t x) (subst i t y)
subst i t (Or x y) = Or (subst i t x) (subst i t y)
subst i t (Leq x y) = Leq (subst i t x) (subst i t y)
subst i t (IsZero x) = IsZero (subst i t x)
subst i t (Fix x) = Fix (subst i t x)

--------------------------------
-- Part 1: Type Checking ------- -- Decide on a language name
--------------------------------
-- implement a method that takes an expression from your language and returns its type given a context. If no type is found your method should return Nothing. 
typeOf :: Cont -> T -> (Maybe TY)
typeOf _ (Int _) = Just Num
typeOf _ (Bool _) = Just Boolean
typeOf cont (Id x) = lookupVar x cont

typeOf cont (Add x y) = do  
  t1 <- typeOf cont x;
  t2 <- typeOf cont y;
  case (t1, t2) of
    (Num, Num) -> Just Num
    _ -> Nothing
 

typeOf cont (Sub x y) = do  
  t1 <- typeOf cont x;
  t2 <- typeOf cont y;
  case (t1, t2) of
    (Num, Num) -> Just Num
    _ -> Nothing
 

typeOf cont (Mult x y) = do  
  t1 <- typeOf cont x;
  t2 <- typeOf cont y;
  case (t1, t2) of
    (Num, Num) -> Just Num
    _ -> Nothing
 

typeOf cont (Div x y) = do  
  t1 <- typeOf cont x;
  t2 <- typeOf cont y;
  case (t1, t2) of
    (Num, Num) -> Just Num
    _ -> Nothing
 

typeOf cont (Pow x y) = do  
  t1 <- typeOf cont x;
  t2 <- typeOf cont y;
  case (t1, t2) of
    (Num, Num) -> Just Num
    _ -> Nothing
 

typeOf cont (Between x y z) = do  
  t1 <- typeOf cont x;
  t2 <- typeOf cont y;
  t3 <- typeOf cont z;
  case (t1, t2, t3) of
    (Num, Num, Num) -> Just Boolean
    _ -> Nothing
 

typeOf cont (Lambda x y z) = do       --where x is an identifier, y is a type, and b is a body
  t1 <- typeOf ((x,y):cont) z;
  -- type-check the body expression with the new variable in the context
  -- t2 <- typeOf ((x, t1):cont) y
  -- return the Arrow type from t1 to t2
  return (Arrow y t1)

typeOf cont (App x y) = do  
  -- type-check the function expression
  (Arrow t1 t2) <- typeOf cont x;
  -- type-check the argument expression
  t1' <- typeOf cont y;
  if t1 == t1' -- if the type of the argument matches the domain of the function
    then return t2
    else Nothing
 

typeOf cont (Bind x y z) = do  
  -- type-check the bound expression
  t1 <- typeOf cont y;
  -- type-check the body expression with the new variable in the context
  t2 <- typeOf ((x, t1):cont) z;
  return t2
 

typeOf cont (If x y z) = do  
  -- type-check the condition expression
  t1 <- typeOf cont x;
  -- type-check the then expression
  t2 <- typeOf cont y;
  -- type-check the else expression
  t3 <- typeOf cont z;
  if t1 == Boolean && t2 == t3 -- if the condition is boolean and the then and else expressions have the same type
    then return t2
    else Nothing
 

typeOf cont (And x y) = do  
  t1 <- typeOf cont x;
  t2 <- typeOf cont y;
  case (t1, t2) of
    (Boolean, Boolean) -> Just Boolean
    _ -> Nothing
 

typeOf cont (Or x y) = do  
  t1 <- typeOf cont x;
  t2 <- typeOf cont y;
  case (t1, t2) of
    (Boolean, Boolean) -> Just Boolean
    _ -> Nothing
 

typeOf cont (Leq x y) = do  
  t1 <- typeOf cont x;
  t2 <- typeOf cont y;
  case (t1, t2) of
    (Num, Num) -> Just Boolean
    _ -> Nothing
 

typeOf cont (IsZero x) = do  
  t1 <- typeOf cont x;
  case t1 of
    Num -> Just Boolean
    _ -> Nothing

typeOf cont (Fix x) = do
  (Arrow t1 t2) <- typeOf cont x;
  if t1 == t2
    then return t1
    else Nothing
 
--------------------------------
-- Part 2: Evaluation ----------
--------------------------------

eval :: EnvV -> T -> (Maybe TV) --call-by-value and static scooping
eval eV (Int x) = Just (NumV x)
eval eV (Bool x) = Just (BoolV x)
eval eV (Id x) = lookup x eV

eval eV (Add x y) = do  
  x' <- eval eV x;
  y' <- eval eV y;
  case (x', y') of
    (NumV x'', NumV y'') -> Just (NumV (x'' + y''))
    _ -> Nothing
 

eval eV (Sub x y) = do  
  x' <- eval eV x;
  y' <- eval eV y;
  case (x', y') of
    (NumV x'', NumV y'') -> Just (NumV (x'' - y''))
    _ -> Nothing
 

eval eV (Mult x y) = do  
  x' <- eval eV x;
  y' <- eval eV y;
  case (x', y') of
    (NumV x'', NumV y'') -> Just (NumV (x'' * y''))
    _ -> Nothing
 

eval eV (Div x y) = do  
  x' <- eval eV x;
  y' <- eval eV y;
  case (x', y') of
    (NumV x'', NumV y'') -> Just (NumV (x'' `div` y''))
    _ -> Nothing
 

eval eV (Pow x y) = do  
  x' <- eval eV x;
  y' <- eval eV y;
  case (x', y') of
    (NumV x'', NumV y'') -> Just (NumV (x'' ^ y''))
    _ -> Nothing
 

eval eV (Between x y z) = do  
  x' <- eval eV x;
  y' <- eval eV y;
  z' <- eval eV z;
  case (x', y', z') of
    (NumV x'', NumV y'', NumV z'') -> Just (BoolV (x'' <= y'' && y'' <= z''))
    _ -> Nothing
 

eval eV (Lambda x y z) = Just (ClosureV x z eV)

eval eV (App x y) = do  
  x' <- eval eV x;
  y' <- eval eV y;
  case x' of
    ClosureV i b e -> eval (useClosure i y' e eV) b
 

eval eV (Bind x y z) = do  
  y' <- eval eV y;
  eval ((x, y'):eV) z
 

eval eV (If x y z) = do  
  x' <- eval eV x;
  case x' of
    (BoolV True) -> eval eV y
    (BoolV False) -> eval eV z
    _ -> Nothing
 

eval eV (And x y) = do  
  x' <- eval eV x;
  y' <- eval eV y;
  case (x', y') of
    (BoolV x'', BoolV y'') -> Just (BoolV (x'' && y''))
    _ -> Nothing
 

eval eV (Or x y) = do  
  x' <- eval eV x;
  y' <- eval eV y;
  case (x', y') of
    (BoolV x'', BoolV y'') -> Just (BoolV (x'' || y''))
    _ -> Nothing
 

eval eV (Leq x y) = do  
  x' <- eval eV x;
  y' <- eval eV y;
  case (x', y') of
    (NumV x'', NumV y'') -> Just (BoolV (x'' <= y''))
    _ -> Nothing
 

eval eV (IsZero x) = do  
  x' <- eval eV x;
  case x' of
    (NumV x'') -> Just (BoolV (x'' == 0))
    _ -> Nothing
 
eval eV (Fix f) = do
  ClosureV x y j <- eval eV f;
  t <- Just Num
  eval j (subst x (Fix (Lambda x t y)) y)


--------------------------------
-- Part 3: Fixed Point Operator
--------------------------------
-- AST UPDATED
-- TYPE CHECKER UPDATED
-- TODO: update eval

--------------------------------
-- Part 4: New Language Feature -- TODO: Decide on a new feature
--------------------------------

-- TODO: update ast
-- TODO: update type checker
-- TODO: update eval

--------------------------------
-- Part 5: Interpretation ------
--------------------------------

-- interpret :: String -> Maybe T
-- if input type is String, we need a parser
interpret :: T -> (Maybe TV)
interpret x = do {typeOf [] x;
                  eval [] x;}


-- Testing
-- To test quickly, you can use the following main function
-- There's also a cabal file
      -- 'cabal build' to compile
      -- 'cabal clean' to clean up .exe and .o files

test1 = interpret (
                  Bind "f"
                    (Fix (Lambda "g" (Arrow Num Num)
                      (Lambda "x" Num
                        (If (Leq (Id "x") (Int 1))
                          (Id "x")
                          (Add
                            (App (Id "g") (Sub (Id "x") (Int 1)))
                            (App (Id "g") (Sub (Id "x") (Int 2)))
                          )
                        )
                      )
                    ))
                    (App (Id "f") (Int 2))) == Just (NumV 1)


main :: IO ()
main = do {
  -- print("Testing...")
  putStrLn $ show $ test1
  }

