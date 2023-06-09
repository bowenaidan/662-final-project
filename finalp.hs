{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- types
-- T ::= int
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
-- TY ::= Num
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
    --List AST--
  List :: T -> T -> T
  Head :: T -> T
  Tail :: T -> T
  Prepend :: T -> T -> T
  EmptyList :: T
  deriving (Show,Eq)

-- Types
data TY where
  TNum :: TY
  TBool :: TY
  (:->:) :: TY -> TY -> TY
  --List type--
  TList :: TY -> TY
  TEmptyList :: TY
  deriving (Show,Eq)

-- Values
data TA where
  NumA :: Int -> TA
  BooleanA :: Bool -> TA
  ClosureV :: String -> T -> [(String, TA)] -> TA  -- Function value
  ListV :: TA -> TA -> TA     -- List value
  EmptyListA :: TA -- List Empty        

  deriving (Show, Eq)


-- Context
-- type Cont = [(String, TY)]
type MyTY = [(String, TY)]

-- Environment
-- type Env = [(String, T)]
-- type EnvV = [(String, TV)]
type MyTA = [(String, TA)]

-- Helper Functions
lookupVar :: String -> MyTA -> TA
lookupVar x e = case lookup x e of
  Just v -> v
  _ -> error "Variable not found"


useClosure :: String -> TA -> MyTA -> MyTA -> MyTA
useClosure i v e _ = (i,v):e
-- useClosure x t e = case lookup x e of
--   Just (ClosureV x' t' e') -> evalMonad (Bind x t e') e
--   _ -> error "Not a closure"

subst :: String -> T -> T -> T
subst _ _ (Num i) = Num i
subst _ _ (Boolean b) = Boolean b
subst i t (Id x) = if i == x then t else Id x
subst i t (Plus x y) = Plus (subst i t x) (subst i t y)
subst i t (Minus x y) = Minus (subst i t x) (subst i t y)
subst i t (Mult x y) = Mult (subst i t x) (subst i t y)
subst i t (Div x y) = Div (subst i t x) (subst i t y)
subst i t (Exp x y) = Exp (subst i t x) (subst i t y)
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
-- Part 1: Type Checking ------- 
--------------------------------

typeofMonad :: MyTY -> T -> (Maybe TY)
typeofMonad g (Num x) = if x >= 0 then Just TNum else Nothing
typeofMonad g (Boolean x) = return TBool
typeofMonad g (Id i) = (lookup i g)
typeofMonad g (Plus x y) = do
  TNum <- typeofMonad g x
  TNum <- typeofMonad g y
  return TNum
typeofMonad g (Minus x y) = do
  TNum <- typeofMonad g x
  TNum <- typeofMonad g y
  return TNum
typeofMonad g (Mult x y) = do
  TNum <- typeofMonad g x
  TNum <- typeofMonad g y
  return TNum
typeofMonad g (Div x y) = do
  TNum <- typeofMonad g x
  TNum <- typeofMonad g y
  return TNum
typeofMonad g (Exp x y) = do
  TNum <- typeofMonad g x
  TNum <- typeofMonad g y
  return TNum
typeofMonad g (Between x y z) = do
  TNum <- typeofMonad g x
  TNum <- typeofMonad g y
  TNum <- typeofMonad g z
  return TBool
typeofMonad g (Lambda x d z) = do
  m <- typeofMonad ((x, d) : g) z
  return (d :->: m)
typeofMonad g (App x y) = do
  y' <- typeofMonad g y
  z :->: r <- typeofMonad g x
  if y' == z then return r else Nothing
typeofMonad g (Bind i v b) = do
  tov <- typeofMonad g v
  typeofMonad ((i, tov) : g) b
typeofMonad g (If x y z) = do
  TBool <- typeofMonad g x
  y' <- typeofMonad g y
  z' <- typeofMonad g z
  if y' == z' then return y' else Nothing
typeofMonad g (And x y) = do
  TBool <- typeofMonad g x
  TBool <- typeofMonad g y
  return TBool
typeofMonad g (Or x y) = do
  TBool <- typeofMonad g x
  TBool <- typeofMonad g y
  return TBool
typeofMonad g (Leq x y) = do
  TNum <- typeofMonad g x
  TNum <- typeofMonad g y
  return TBool
typeofMonad g (IsZero x) = do
  TNum <- typeofMonad g x
  return TBool
typeofMonad g (Fix x) = do
  (d :->: r) <- typeofMonad g x
  return r

  
 --List begin--
typeofMonad g (List x y) = do
  tx <- typeofMonad g x
  ty <- typeofMonad g y
  if tx == ty then return (TList tx) else Nothing
typeofMonad g (Head x) = do
  TList tx <- typeofMonad g x
  return tx
typeofMonad g (Tail x) = do
  tlist <- typeofMonad g x
  return tlist

typeofMonad g (Prepend x y) = do
  tx <- typeofMonad g x
  TList ty <- typeofMonad g y
  if tx == ty then return (TList tx) else Nothing

typeofMonad _ EmptyList = Just (TList TEmptyList)

--List end

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
evalMonad e (IsZero x) = do {
  (NumA x') <- evalMonad e x;
  if (x' == 0) then Just (BooleanA True) else Just (BooleanA False)
}
evalMonad e (Fix f) = do {
  ClosureV x y j <- evalMonad e f;
  t <- Just TNum;
  evalMonad j (subst x (Fix (Lambda x t y)) y)
}

--List begin
evalMonad e (List x y) = do {
  x' <- evalMonad e x;
  y' <- evalMonad e y;
  Just (ListV x' y')
}
evalMonad e (Head x) = do {
  (ListV h _) <- evalMonad e x;
  Just h
}
evalMonad e (Tail x) = do {
  (ListV _ t) <- evalMonad e x;
  Just t
}
evalMonad e (Prepend x y) = do {
  x' <- evalMonad e x;
  y' <- evalMonad e y;
  Just (ListV x' y')
}

evalMonad _ EmptyList = Just EmptyListA

--List end

--------------------------------
-- Part 3: Fixed Point Operator
--------------------------------
-- AST UPDATED
-- TYPE CHECKER UPDATED
-- EVAL UPDATED

--------------------------------
-- Part 4: New Language Feature (Lists)
--------------------------------

-- AST UPDTAED
-- TYPE CHECKER UPDATED
-- EVAL UPDATED

--------------------------------
-- Part 5: Interpretation ------
--------------------------------

-- interpret :: String -> Maybe T
-- if input type is String, we need a parser
interpret :: T -> (Maybe TA)
interpret x = do {typeofMonad [] x;
                  evalMonad [] x;}


-- Testing
-- To test quickly, you can use the following main function
-- There's also a cabal file
-- 'cabal build' to compile
-- 'cabal clean' to clean up .exe and .o files


-- --three different recursive case functions for testing
-- --fibonacci
-- test0 = interpret (
--                   Bind "f"
--                     (Fix (Lambda "g" (TNum :->: TNum)
--                       (Lambda "x" TNum
--                         (If (Leq (Id "x") (Num 1))
--                           (Id "x")
--                           (Plus
--                             (App (Id "g") (Minus (Id "x") (Num 1)))
--                             (App (Id "g") (Minus (Id "x") (Num 2)))
--                           )
--                         )
--                       )
--                     ))
--                     (App (Id "f") (Num 15))) == Just (NumA 610)

-- --factorial
-- test1 = interpret (Bind "f"
--                   (Lambda "g" ((:->:) TNum TNum)
--                     (Lambda "x" TNum
--                       (If (IsZero (Id "x"))(Num 1)(Mult (Id "x")(App (Id "g")(Minus (Id "x")(Num 1)))))))
--                     (App (Fix (Id "f")) (Num 8))) == Just (NumA 40320)
-- --factorial again
-- test2 = interpret (
--                   Bind "f" (Lambda "g" ((:->:) TNum TNum)
--                   (Lambda "x" TNum (If (IsZero (Id "x")) (Num 1)
--                                         (Mult (Id "x")
--                                               (App (Id "g")
--                                                   (Minus (Id "x") (Num 1)))))))
--                   (App (Fix (Id "f")) (Num 6))) == Just (NumA 720)


-- main :: IO ()
-- main = do {
--   print(if test0 then "Pass" else "Fail");
--   print(if test1 then "Pass" else "Fail");
--   print(if test2 then "Pass" else "Fail");
--   }


runTest :: String -> T -> IO ()
runTest testName expr = do
  putStrLn $ testName ++ ":"
  putStrLn $ " Expression: " ++ show expr
  putStrLn $ " Result: " ++ show (evalMonad [] (Bind testName expr (Id testName)))
  putStrLn ""

test1 = List EmptyList EmptyList
test2 = Prepend (Num 1) EmptyList
test3 = Prepend (Num 3) (Prepend (Num 2) (Prepend (Num 1) EmptyList))
test4 = Head (Prepend (Num 5) (Prepend (Num 4) EmptyList))
test5 = Tail (Prepend (Num 7) (Prepend (Num 6) EmptyList))
test6 = Prepend (Num 9) (Prepend (Num 8) (Prepend (Num 7) (Prepend (Num 6) EmptyList)))




main :: IO ()
main = do
  runTest "Test Case 1: Creating an empty list" test1
  runTest "Test Case 2: Creating a list with one element" test2
  runTest "Test Case 3: Creating a list with multiple elements" test3
  runTest "Test Case 4: Accessing the head of a list" test4
  runTest "Test Case 5: Accessing the tail of a list" test5
  runTest "Test Case 6: Prepending an element to a list" test6


