data Term = Var String
	| Abs String Term
	| App Term Term deriving (Eq, Show)

isAbs :: Term -> Bool
isAbs (Abs _ _) = True
isAbs _ = False

{- return a term with a replacement of variable(s) of name n in term (pattern) by term r -}
replace :: String -> Term -> Term -> Term
replace n (Var s) r = if s == n 
	then r
	else (Var s)
replace n (App a b) r = (App (replace n a r) (replace n b r))
replace n (Abs v t) r = (Abs v (replace n t r))

substitute :: Term -> Term -> Term
substitute (Abs a aTerm) (Abs b bTerm) = replace a aTerm (Abs b bTerm)

eval :: Term -> Term
eval (App a b) | isAbs a && isAbs b = substitute a b
eval (App a b) | isAbs a = (App a (eval b))
eval (App a b) = (App (eval a) b)
eval a = a

fullEval :: Term -> Term
fullEval a | a /= b = fullEval $ eval a
	| otherwise = a
	where b = eval a
