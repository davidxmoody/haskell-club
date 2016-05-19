module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend state key val lookup
    | key == lookup  = val
    | otherwise      = state lookup

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

evalE :: State -> Expression -> Int
evalE state (Var str) = state str
evalE state (Val int) = int
evalE state (Op a Plus b) = (evalE state a) + (evalE state b)
evalE state (Op a Times b) = (evalE state a) * (evalE state b)
evalE state (Op a Minus b) = (evalE state a) - (evalE state b)
evalE state (Op a Divide b) = (evalE state a) `div` (evalE state b)
evalE state (Op a Gt b) = boolToInt $ (evalE state a) > (evalE state b)
evalE state (Op a Ge b) = boolToInt $ (evalE state a) >= (evalE state b)
evalE state (Op a Lt b) = boolToInt $ (evalE state a) < (evalE state b)
evalE state (Op a Le b) = boolToInt $ (evalE state a) <= (evalE state b)
evalE state (Op a Eql b) = boolToInt $ (evalE state a) == (evalE state b)

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign var e) = DAssign var e
desugar (Incr var) = DAssign var (Op (Val 1) Plus (Var var))
desugar (If e s1 s2) = DIf e (desugar s1) (desugar s2)
desugar (While e s) = DWhile e (desugar s)
desugar (For s1 e s2 s3) = DSequence (desugar s1) (DWhile e (DSequence (desugar s3) (desugar s2)))
desugar (Sequence s1 s2) = DSequence (desugar s1) (desugar s2)
desugar (Skip) = DSkip


-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple state (DAssign var ex) = extend state var (evalE state ex)
evalSimple state (DIf ex st1 st2)
  | (evalE state ex) == 0 = evalSimple state st2
  | otherwise             = evalSimple state st1
evalSimple state (DWhile ex st)
  | (evalE state ex) == 0 = state
  | otherwise             = evalSimple (evalSimple state st) (DWhile ex st)
evalSimple state (DSequence st1 st2) = evalSimple (evalSimple state st1) st2
evalSimple state (DSkip) = state

run :: State -> Statement -> State
run state statement = evalSimple state $ desugar statement

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
