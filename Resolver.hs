module Resolver
( satisfiable
) where 

import Data.List

-- Type declarations  

data Formula = Atom String | Or Formula Formula | And Formula Formula | Not Formula deriving (Eq, Show, Read)

data Literal = NegativeLiteral String | PositiveLiteral String deriving (Eq, Ord, Show, Read)
type Clause = [Literal]
type ClauseSet = [Clause]

-- Helpers

negatedLiteral :: Literal -> Literal
negatedLiteral (NegativeLiteral name) = PositiveLiteral name
negatedLiteral (PositiveLiteral name) = NegativeLiteral name

-- CNF formula to clause set

cnfToLiteral :: Formula -> Literal
cnfToLiteral (Atom name) = PositiveLiteral name
cnfToLiteral (Not (Atom name)) = NegativeLiteral name

cnfToClause :: Formula -> Clause
cnfToClause (Or left right) = (cnfToClause left) ++ (cnfToClause right)
cnfToClause formula = [cnfToLiteral formula]

cnfToClauseSet :: Formula -> ClauseSet
cnfToClauseSet (And left right) = (cnfToClauseSet left) ++ (cnfToClauseSet right)
cnfToClauseSet formula = [cnfToClause formula]

-- Any formula to CNF formula

shiftNegations :: Formula -> Formula
shiftNegations (Not (And left right)) = Or (shiftNegations (Not left)) (shiftNegations (Not right))
shiftNegations (Not (Or left right)) = And (shiftNegations (Not left)) (shiftNegations (Not right))
shiftNegations (Not (Not f)) = shiftNegations f
shiftNegations (Not (Atom name)) = Not $ Atom name
shiftNegations (And left right) = And (shiftNegations left) (shiftNegations right)
shiftNegations (Or left right) = Or (shiftNegations left) (shiftNegations right)
shiftNegations (Atom name) = (Atom name)

shiftAndOr :: Formula -> Formula
-- a | (b & c) === (a | b) & (a | c)

shiftAndOr (Or l r) =
  let args = (shiftAndOr l, shiftAndOr r)
  in case args of ((And left right), other) -> And (shiftAndOr (Or left other)) (shiftAndOr (Or right other))
                  (other, (And left right)) -> And (shiftAndOr (Or other left)) (shiftAndOr (Or other right))
                  (left, right)             -> Or left right

shiftAndOr (And left right) = (And (shiftAndOr left) (shiftAndOr right))
shiftAndOr f = f

formulaToCNF :: Formula -> Formula
formulaToCNF = shiftAndOr . shiftNegations

-- Normalize clause sets

normalizeClause :: Clause -> Clause
normalizeClause literals
  | overlaps = []
  | otherwise =  [PositiveLiteral a | a <- positive] ++ [NegativeLiteral a | a <- negative]
  where positive = sort . nub $ [ a | PositiveLiteral a <- literals]
        negative = sort . nub $ [a | NegativeLiteral a <- literals]
        overlaps = not . null $ intersect positive negative

normalizeClauseSet :: ClauseSet -> ClauseSet
normalizeClauseSet clauses = sort . nub . removeEmpty $ map normalizeClause clauses
  where removeEmpty = filter (not . null)

-- Lexer

data Symbol = OpenParenthesis | ClosingParenthesis | AndOperator | OrOperator | NotOperator | Identifier String | EndOfFile deriving (Eq, Show, Read)

scan :: String -> (Symbol, String)
scan ('(':tail) = (OpenParenthesis, tail)
scan (')':tail) = (ClosingParenthesis, tail)
scan ('&':tail) = (AndOperator, tail)
scan ('|':tail) = (OrOperator, tail)
scan ('!':tail) = (NotOperator, tail)
scan "" = (EndOfFile, "")
scan all
       | not $ null ident = (Identifier ident, tail)
       | otherwise        = error $ "invalid char at " ++ tail
  where ident = takeWhile isLetter all
        tail  = dropWhile isLetter all
        isLetter = (`elem` ['a'..'z'])

-- Parser

parseAtomic :: String -> (Formula, String)
parseAtomic all =
  case symbol of (Identifier name) -> (Atom name, tail)
                 OpenParenthesis   -> let (formula, tail2) = parseOr tail;
                                          (nextSymbol, realTail) = scan tail2 in case nextSymbol of ClosingParenthesis -> (formula, realTail)
                                                                                                    _ -> error ("')' expected, but " ++ (show nextSymbol) ++ " found")
                 s -> error("Identifier or '(' expected, but " ++ (show s) ++ " found")
  where (symbol, tail) = scan all

parseNot :: String -> (Formula, String)
parseNot all = 
  case symbol of NotOperator -> let (formula, realTail) = parseNot tail in (Not formula, realTail)
                 _ -> parseAtomic all
  where (symbol, tail) = scan all

parseAnd :: String -> (Formula, String)
parseAnd all = case middleSymbol of AndOperator -> (And first second, realTail)
                                    _ -> (first, firstTail)
  where (first, firstTail) = parseNot(all)
        (middleSymbol, secondTail) = scan(firstTail)
        (second, realTail) = parseAnd(secondTail)

parseOr :: String -> (Formula, String)
parseOr all = case middleSymbol of OrOperator -> (Or first second, realTail)
                                   _ -> (first, firstTail)
  where (first, firstTail) = parseAnd(all)
        (middleSymbol, secondTail) = scan(firstTail)
        (second, realTail) = parseOr(secondTail)

parse :: String -> Formula
parse str = case nextSymbol of EndOfFile -> formula
                               _  -> error $ "End of input expected, but " ++ (show nextSymbol) ++ " found"
 where (formula, tail) = parseOr str
       (nextSymbol, _) = scan tail
       
-- Resolution

resolveClause :: Clause -> Clause -> Maybe Clause
resolveClause leftClause rightClause =
  case leftLiteral of Just lit -> Just $ (delete lit leftClause) ++ (delete (negatedLiteral lit) rightClause)
                      Nothing  -> Nothing
  where leftLiteral = find (\lit -> (negatedLiteral lit) `elem` rightClause) leftClause

resolveClauseSet :: ClauseSet -> ClauseSet
resolveClauseSet clauses = nub $ clauses ++ [a | Just a <- resolvents]
  where resolvents = [resolveClause left right | left <- clauses, right <- clauses, left /= right]

resolve :: ClauseSet -> ClauseSet
resolve clauses =
  if changed then resolve resolution else resolution
  where resolution = resolveClauseSet clauses
        changed = (length resolution) /= (length clauses)

-- High-level functions

formulaToClauseSet :: Formula -> ClauseSet
formulaToClauseSet = normalizeClauseSet . cnfToClauseSet . formulaToCNF

satisfiable :: String -> Bool
satisfiable str = not $ [] `elem` resolution
  where resolution = resolve . formulaToClauseSet . parse $ str
