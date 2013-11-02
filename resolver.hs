data Formula = Atom String | Or Formula Formula | And Formula Formula | Not Formula deriving (Eq, Show, Read)

data Literal = PositiveLiteral String | NegativeLiteral String deriving (Eq, Show, Read)
type Clause = [Literal]
type ClauseSet = [Clause]
