{-# LANGUAGE GADTs,FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}


-- Abstract Syntax Definitions
data T where
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

  --List AST--
  List :: T -> T -> T
  Head :: T -> T
  Tail :: T -> T
  Prepend :: T -> T -> T
  EmptyList :: T 
  deriving (Show, Eq)

-- Type Definitions


data TY where
  TNum :: TY
  TBool :: TY
  (:->:) :: TY -> TY -> TY
      --List type--
  TList :: TY -> TY
  TEmptyList :: TY

  -- TVar :: String -> TY --
  deriving (Show, Eq)


data TA where
  NumA :: Int -> TA
  BooleanA :: Bool -> TA
  ClosureV :: String -> T -> [(String, TA)] -> TA  -- Function value
  ListV :: TA -> TA -> TA     -- List value
  EmptyListA :: TA -- List Empty        

  deriving (Show, Eq)


--------------------------------
-- Part 1: Type Checking ------- TODO: -- Decide on a language name
--------------------------------

typeofMonad :: [(String, TY)] -> T -> Maybe TY
typeofMonad g (Num x) = if x >= 0 then Just TNum else Nothing
-- typeofMonad g EmptyList = Just (TList (TVar "a"))
-- typeofMonad g EmptyList = Just EmptyListA

-- typeofMonad g EmptyList = Just TEmptyList 
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
evalMonad :: [(String, TA)] -> T -> Maybe TA
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
-- Part 4: New Language Feature TODO: -- List is implemented above and 
--lines corresponding is commented list
--------------------------------

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


