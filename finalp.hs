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

typeOf :: T -> Maybe TY
typeOf (Int _) = Just Num

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