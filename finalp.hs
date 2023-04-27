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
  deriving (Show,Eq)

data TY where
  TNum :: TY
  TBool :: TY
  (:->:) :: TY -> TY -> TY
  deriving (Show,Eq)

data TA where
  NumA :: Int -> TA
  BooleanA :: Bool -> TA
  ClosureV :: String -> T -> MyTA -> TA
  deriving (Show,Eq)

type MyTY = [(String,TY)]
type MyTA = [(String,TA)]

--------------------------------
-- Part 1: Type Checking ------- TODO: -- Decide on a language name
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


--------------------------------
-- Part 4: New Language Feature TODO: -- Decide on a new feature
--------------------------------



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
main = do 
  print("This is the project")
