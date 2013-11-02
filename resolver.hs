data Formula = Atom String | Or Formula Formula | And Formula Formula | Not Formula deriving (Eq, Show, Read)

data Literal = PositiveLiteral String | NegativeLiteral String deriving (Eq, Show, Read)
type Clause = [Literal]
type ClauseSet = [Clause]

formulaToLiteral :: Formula -> Literal
formulaToLiteral (Atom name) = PositiveLiteral name
formulaToLiteral (Not (Atom name)) = NegativeLiteral name

formulaToClause :: Formula -> Clause
formulaToClause (Or left right) = (formulaToClause left) ++ (formulaToClause right)
formulaToClause formula = [formulaToLiteral formula]

formulaToClauseSet :: Formula -> ClauseSet
formulaToClauseSet (And left right) = (formulaToClauseSet left) ++ (formulaToClauseSet right)
formulaToClauseSet formula = [formulaToClause formula]

shiftNegations :: Formula -> Formula
shiftNegations (Not (And left right)) = Or (shiftNegations (Not left)) (shiftNegations (Not right))
shiftNegations (Not (Or left right)) = And (shiftNegations (Not left)) (shiftNegations (Not right))
shiftNegations (Not (Not f)) = shiftNegations f
shiftNegations (Not (Atom name)) = Not $ Atom name
shiftNegations (And left right) = And (shiftNegations left) (shiftNegations right)
shiftNegations (Or left right) = Or (shiftNegations left) (shiftNegations right)
shiftNegations (Atom name) = (Atom name)

-- tools for testing
a = Atom "a"
b = Atom "b"
c = Atom "c"
f = Not $ And a b
f2 = And f (Not f)

