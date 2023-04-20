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
  Lambda :: String -> T -> T
  App :: T -> T -> T
  Bind :: String -> T -> T -> T
  If :: T -> T -> T -> T
  And :: T -> T -> T
  Or :: T -> T -> T
  Leq :: T -> T -> T
  IsZero :: T -> T
  deriving (Show)

data TY where -- types
  Num :: TY
  Boolean :: TY
  Arrow :: TY -> TY -> TY
  deriving (Show)

--------------------------------
-- Part 1: Type Checking ------- TODO: -- Decide on a language name
--------------------------------
-- implement a method that takes an expression from your language and returns its type given a context. If no type is found your method should return Nothing. 
typeOf :: Cont -> T -> (Maybe TY)
typeOf _ (Int _) = Just Num
typeOf _ (Bool _) = Just Boolean
typeOf cont (Id x) = lookup x cont

typeOf cont (Add x y) = do {
  t1 <- typeOf cont x;
  t2 <- typeOf cont y;
  case (t1, t2) of
    (Num, Num) -> Just Num
    _ -> Nothing
}

typeOf cont (Sub x y) = do {
  t1 <- typeOf cont x;
  t2 <- typeOf cont y;
  case (t1, t2) of
    (Num, Num) -> Just Num
    _ -> Nothing
}

typeOf cont (Mult x y) = do {
  t1 <- typeOf cont x;
  t2 <- typeOf cont y;
  case (t1, t2) of
    (Num, Num) -> Just Num
    _ -> Nothing
}

typeOf cont (Div x y) = do {
  t1 <- typeOf cont x;
  t2 <- typeOf cont y;
  case (t1, t2) of
    (Num, Num) -> Just Num
    _ -> Nothing
}

typeOf cont (Pow x y) = do {
  t1 <- typeOf cont x;
  t2 <- typeOf cont y;
  case (t1, t2) of
    (Num, Num) -> Just Num
    _ -> Nothing
}

typeOf cont (Between x y z) = do {
  t1 <- typeOf cont x;
  t2 <- typeOf cont y;
  t3 <- typeOf cont z;
  case (t1, t2, t3) of
    (Num, Num, Num) -> Just Boolean
    _ -> Nothing
}

typeOf cont (Lambda x y) = do {
  t1 <- lookupVar x cont
  -- type-check the body expression with the new variable in the context
  t2 <- typeOf ((x, t1):cont) y
  -- return the arrow type from t1 to t2
  return (Arrow t1 t2)
}

typeOf cont (App x y) = do {
  -- type-check the function expression
  (Arrow t1 t2) <- typeOf cont x;
  -- type-check the argument expression
  t1' <- typeOf cont y;
  if t1 == t1' -- if the type of the argument matches the domain of the function
    then return t2
    else Nothing
}

typeOf cont (Bind x y z) = do {
  -- type-check the bound expression
  t1 <- typeOf cont y;
  -- type-check the body expression with the new variable in the context
  t2 <- typeOf ((x, t1):cont) z;
  return t2
}

typeOf cont (If x y z) = do {
  -- type-check the condition expression
  t1 <- typeOf cont x;
  -- type-check the then expression
  t2 <- typeOf cont y;
  -- type-check the else expression
  t3 <- typeOf cont z;
  if t1 == Boolean && t2 == t3 -- if the condition is boolean and the then and else expressions have the same type
    then return t2
    else Nothing
}

typeOf cont (And x y) = do {
  t1 <- typeOf cont x;
  t2 <- typeOf cont y;
  case (t1, t2) of
    (Boolean, Boolean) -> Just Boolean
    _ -> Nothing
}

typeOf cont (Or x y) = do {
  t1 <- typeOf cont x;
  t2 <- typeOf cont y;
  case (t1, t2) of
    (Boolean, Boolean) -> Just Boolean
    _ -> Nothing
}

typeOf cont (Leq x y) = do {
  t1 <- typeOf cont x;
  t2 <- typeOf cont y;
  case (t1, t2) of
    (Num, Num) -> Just Boolean
    _ -> Nothing
}

typeOf cont (IsZero x) = do {
  t1 <- typeOf cont x;
  case t1 of
    Num -> Just Boolean
    _ -> Nothing
}

--------------------------------
-- Part 2: Evaluation ----------
--------------------------------

eval :: T -> Maybe T --call-by-value and static scooping
eval (Int _) = Just (Int _)

--------------------------------
-- Part 3: Fixed Point Operator
--------------------------------
TODO: -- update ast
TODO: -- update type checker
TODO: -- update eval

--------------------------------
-- Part 4: New Language Feature TODO: -- Decide on a new feature
--------------------------------

TODO: -- update ast
TODO: -- update type checker
TODO: -- update eval

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