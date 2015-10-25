data Term = T
	| F
	| If Term Term Term
	| Zero
	| Succ Term
	| Pred Term
	| IsZero Term deriving (Eq, Show)

isNumeric :: Term -> Bool
isNumeric Zero = True
isNumeric (Succ a) = isNumeric a
isNumeric (Pred a) = isNumeric a
isNumeric _ = False

eval' :: Term -> Term
eval' T = T
eval' F = F
eval' Zero = Zero
eval' (IsZero Zero) = T
eval' (IsZero (Succ a)) | isNumeric a = F 
eval' (If T a b) = a
eval' (If F a b) = b
eval' (If a b c) = If (eval' a) b c
eval' (IsZero a) = IsZero $ eval' a
eval' (Pred Zero) = Zero
eval' (Pred (Succ a)) | isNumeric a = a
eval' (Succ a) = Succ $ eval' a

eval :: Term -> Term 
eval a | a /= b = eval (eval' a)
	| otherwise = a
	where b = eval' a

bigStepEval :: Term -> Term

bigStepEval T = T
bigStepEval F = F
bigStepEval Zero = Zero

bigStepEval (If a b c) | e == T = bigStepEval b
	| e == F = bigStepEval c
	where e = bigStepEval a

bigStepEval (IsZero a) | e == Zero = T
	where e = bigStepEval a

bigStepEval (Pred a) | e == Zero = Zero
	| isNumeric e = case e of Succ n -> n  
	where e = bigStepEval a

bigStepEval (Succ a) | isNumeric e = Succ e
	where e = bigStepEval a 
