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

-- formulaToClauseSet (Not (And left right)) = Or $ formulaToClauseSet (Not left) formulaToClauseSet (Not right)
-- formulaToClauseSet (Not (Or left right)) = And $ formulaToClauseSet (Not left) formulaToClauseSet (Not right)

