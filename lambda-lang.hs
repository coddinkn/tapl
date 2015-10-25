data Term = Variable String
	| Lambda String Term
	| Term Term deriving (Eq, Show)
