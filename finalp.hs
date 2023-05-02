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
  Num :: Int -> T
  Boolean :: Bool -> T
  Id :: String -> T
  Plus :: T -> T -> T
  Minus :: T -> T -> T
  Mult :: T -> T -> T
  Div :: T -> T -> T
  Exp :: T -> T -> T
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
  deriving (Show,Eq)

-- Types
data TY where
  TNum :: TY
  TBool :: TY
  (:->:) :: TY -> TY -> TY
  deriving (Show,Eq)

-- Values
data TA where
  NumA :: Int -> TA
  BooleanA :: Bool -> TA
  ClosureV :: String -> T -> MyTA -> TA
  deriving (Show, Eq,Eq)

-- Context
-- type Cont = [(String, TY)]
type MyTY = [(String, TY)]

-- Environment
-- type Env = [(String, T)]
-- type EnvV = [(String, TV)]
type MyTA = [(String, TA)]

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
subst i t (Lambda x y) = if i == x then Lambda x y else Lambda x (subst i t y)
subst i t (App x y) = App (subst i t x) (subst i t y)
subst i t (Bind x y z) = if i == x then Bind x (subst i t y) z else Bind x (subst i t y) (subst i t z)
subst i t (If x y z) = If (subst i t x) (subst i t y) (subst i t z)
subst i t (And x y) = And (subst i t x) (subst i t y)
subst i t (Or x y) = Or (subst i t x) (subst i t y)
subst i t (Leq x y) = Leq (subst i t x) (subst i t y)
subst i t (IsZero x) = IsZero (subst i t x)
subst i t (Fix x) = Fix (subst i t x)

--------------------------------
-- Part 1: Type Checking ------- 
--------------------------------
typeofMonad :: MyTY -> T -> (Maybe TY)
typeofMonad g (Num x ) = if x >=0 then Just TNum else Nothing
typeofMonad g (Boolean x) = return TBool
typeofMonad g (Id i) = (lookup i g)
typeofMonad g (Plus x y) = do {
    TNum <- typeofMonad g x;
    TNum <- typeofMonad g y;
    return TNum
}
typeofMonad g (Minus x y) = do {
    TNum <- typeofMonad g x;
    TNum <- typeofMonad g y;
    return TNum
}
typeofMonad g (Mult x y) = do {
    TNum <- typeofMonad g x;
    TNum <- typeofMonad g y;
    return TNum
}
typeofMonad g (Div x y) = do {
    TNum <- typeofMonad g x;
    TNum <- typeofMonad g y;
    return TNum
}
typeofMonad g (Exp x y) = do {
    TNum <- typeofMonad g x;
    TNum <- typeofMonad g y;
    return TNum
}
typeofMonad g (Between x y z) = do {
    TNum <- typeofMonad g x;
    TNum <- typeofMonad g y;
    TNum <- typeofMonad g z;
    return TBool
}
typeofMonad g (Lambda x d z) = do {
    m <- typeofMonad ((x,d):g) z;
        return (d :->: m)
}
typeofMonad g (App x y) = do {
    y' <- typeofMonad g y;
    z :->: r <- typeofMonad g x;
        if y' == z
        then return r
        else Nothing
}
typeofMonad g (Bind i v b) = do {
    tov <- typeofMonad g v;
    typeofMonad ((i,tov):g) b
}
typeofMonad g (If x y z) = do {
    TBool <- typeofMonad g x;
    if ((typeofMonad g y) == (typeofMonad g z)) then (typeofMonad g y) else Nothing
}
typeofMonad g (And x y) = do {
    TBool <- typeofMonad g x;
    TBool <- typeofMonad g y;
    return TBool
}
typeofMonad g (Or x y) = do {
    TBool <- typeofMonad g x;
    TBool <- typeofMonad g y;
    return TBool
}
typeofMonad g (Leq x y) = do {
    TNum <- typeofMonad g x;
    TNum <- typeofMonad g y;
    return TBool
}
typeofMonad g (IsZero x) = do {
    TNum <- typeofMonad g x;
    return TBool
}
--------------------------------
-- Part 2: Evaluation ----------
--------------------------------

evalMonad :: MyTA -> T -> (Maybe TA)
evalMonad e (Num x) = if x < 0 then Nothing else Just (NumA x)
evalMonad e (Boolean x) = Just (BooleanA x)
evalMonad e (Id i) = (lookup i e)
evalMonad e (Plus x y) = do {
    (NumA x') <- (evalMonad e x);
    (NumA y') <- (evalMonad e y);
    Just (NumA (x' + y'))
}
evalMonad e (Minus x y) = do {
    (NumA x') <- (evalMonad e x);
    (NumA y') <- (evalMonad e y);
    if (x' < y') then Nothing else Just (NumA (x' - y'))
}
evalMonad e (Mult x y) = do {
    (NumA x') <- (evalMonad e x);
    (NumA y') <- (evalMonad e y);
    return (NumA (x' * y'))
}
evalMonad e (Div x y) = do{
    (NumA x') <- (evalMonad e x);
    (NumA y') <- (evalMonad e y);
    if (y' == 0) then Nothing else return (NumA (x' `div` y'))
}
evalMonad e (Exp x y) = do{
    (NumA x') <- (evalMonad e x);
    (NumA y') <- (evalMonad e y);
    return (NumA (x' ^ y'))
}
evalMonad e (Between x y z) = do{
    (NumA x') <- (evalMonad e x);
    (NumA y') <- (evalMonad e y);
    (NumA z') <- (evalMonad e z);
    if (x' < y' && y' < z') then Just (BooleanA True) else Just (BooleanA False) 
}
evalMonad e (Lambda x y z) = Just (ClosureV x z e)
evalMonad e (App f a) = do{
    (ClosureV i b j)<- evalMonad e f;
    v <- evalMonad e a;
    evalMonad ((i,v):j) b
}
evalMonad e (Bind i v b) = do {
    v' <- evalMonad e v;
    evalMonad ((i, v'):e) b
}
evalMonad e (If x y z) = do{
    (BooleanA x') <- (evalMonad e x);
    if x' then (evalMonad e y) else (evalMonad e z)
}
evalMonad e (And x y) = do{
    (BooleanA x') <- (evalMonad e x);
    (BooleanA y') <- (evalMonad e y);
    return (BooleanA (x' && y'))
}
evalMonad e (Or x y) = do{
    (BooleanA x') <- (evalMonad e x);
    (BooleanA y') <- (evalMonad e y);
    return (BooleanA (x' || y'))
}
evalMonad e (Leq x y) = do{
    (NumA x') <- (evalMonad e x);
    (NumA y') <- (evalMonad e y);
    if (x' <= y') then Just (BooleanA True) else Just (BooleanA False)
}
evalMonad e (IsZero x) = do{
    (NumA x') <- (evalMonad e x);
    if (x' == 0) then Just (BooleanA True) else Just (BooleanA False) 
}
--------------------------------
-- Part 3: Fixed Point Operator
--------------------------------
-- AST UPDATED
-- TYPE CHECKER UPDATED
-- TODO: update eval

--------------------------------
-- Part 4: New Language Feature (Lists)
--------------------------------

-- TODO: update ast
-- TODO: update type checker
-- TODO: update eval

--------------------------------
-- Part 5: Interpretation ------
--------------------------------

-- interpret :: String -> Maybe T


-- Testing
-- To test quickly, you can use the following main function
-- There's also a cabal file
      -- 'cabal build' to compile
      -- 'cabal clean' to clean up .exe and .o files


main :: IO ()
main = do {
  print("Testing...")
  }

