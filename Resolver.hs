module Resolver
( satisfiable
) where 

import Data.List
import Data.Maybe

-- Type declarations  

data Formula = Atom String | Or Formula Formula | And Formula Formula | Not Formula deriving (Eq, Show, Read)

data Literal = NegativeLiteral String | PositiveLiteral String deriving (Eq, Ord, Show, Read)
type Clause = [Literal]
type ClauseSet = [Clause]

-- Helpers

-- | Converts positive to negative and negative to positive literal.
negatedLiteral :: Literal -> Literal
negatedLiteral (NegativeLiteral name) = PositiveLiteral name
negatedLiteral (PositiveLiteral name) = NegativeLiteral name

-- Any formula to CNF formula

-- !(a & b) === (!a | !B)    !(a | b) === (!a & !b)    !!a === a
-- | Moves negations inwards using deMorgan and eliminates duplicate negations
shiftNegations :: Formula -> Formula
shiftNegations (Not (And left right)) = Or (shiftNegations (Not left)) (shiftNegations (Not right))
shiftNegations (Not (Or left right)) = And (shiftNegations (Not left)) (shiftNegations (Not right))
shiftNegations (Not (Not f)) = shiftNegations f
shiftNegations (Not (Atom name)) = Not $ Atom name
shiftNegations (And left right) = And (shiftNegations left) (shiftNegations right)
shiftNegations (Or left right) = Or (shiftNegations left) (shiftNegations right)
shiftNegations (Atom name) = (Atom name)

-- a | (b & c) === (a | b) & (a | c)
-- | Moves 'and's outwards using the distributive law
shiftAndOr :: Formula -> Formula
shiftAndOr (Or l r) =
  let args = (shiftAndOr l, shiftAndOr r)
  in case args of ((And left right), other) -> And (shiftAndOr (Or left other)) (shiftAndOr (Or right other))
                  (other, (And left right)) -> And (shiftAndOr (Or other left)) (shiftAndOr (Or other right))
                  (left, right)             -> Or left right
shiftAndOr (And left right) = (And (shiftAndOr left) (shiftAndOr right))
shiftAndOr f = f

-- | Converts a formula to Conjunctive Normal Form
formulaToCNF :: Formula -> Formula
formulaToCNF = shiftAndOr . shiftNegations

-- CNF formula to clause set

-- | Converts an atomic formula or a negation of an atomic formula to a literal
cnfToLiteral :: Formula -> Literal
cnfToLiteral (Atom name) = PositiveLiteral name
cnfToLiteral (Not (Atom name)) = NegativeLiteral name

-- | Converts a disjunction of atomic formulas / negations thereof to a set of literals
cnfToClause :: Formula -> Clause
cnfToClause (Or left right) = (cnfToClause left) ++ (cnfToClause right)
cnfToClause formula = [cnfToLiteral formula]

-- | Converts a conjunction of disjunction of atomic formulas / negations thereof to a set
-- | of set of literals
cnfToClauseSet :: Formula -> ClauseSet
cnfToClauseSet (And left right) = (cnfToClauseSet left) ++ (cnfToClauseSet right)
cnfToClauseSet formula = [cnfToClause formula]

-- Normalize clause sets

-- | Removes duplicate literals, sorts literals and makes tautologies Nothing
normalizeClause :: Clause -> Maybe Clause
normalizeClause literals
  | overlaps = Nothing
  | otherwise =  Just $ [PositiveLiteral a | a <- positive] ++ [NegativeLiteral a | a <- negative]
  where positive = sort . nub $ [ a | PositiveLiteral a <- literals]
        negative = sort . nub $ [a | NegativeLiteral a <- literals]
        overlaps = not . null $ intersect positive negative

-- | Removes duplicate and clauses, sorts clauses and removes tautologies
normalizeClauseSet :: ClauseSet -> ClauseSet
normalizeClauseSet clauses = sort . nub $ mapMaybe normalizeClause clauses

-- Resolution

-- | Resolves a new clause of two old clauses
resolveClause :: Clause -> Clause -> Maybe Clause
resolveClause leftClause rightClause =
  case leftLiteral of Just lit -> Just . nub $ (delete lit leftClause) ++ (delete (negatedLiteral lit) rightClause)
                      Nothing  -> Nothing
  where leftLiteral = find (\lit -> (negatedLiteral lit) `elem` rightClause) leftClause

-- | Gets all possible resolvents of a clause set
resolveClauseSet :: ClauseSet -> ClauseSet
resolveClauseSet clauses = normalizeClauseSet $ clauses ++ [a | Just a <- resolvents]
  where resolvents = [resolveClause left right | left <- clauses, right <- clauses, left /= right]

-- | Resolves until no new clauses can be resolved
resolve :: ClauseSet -> ClauseSet
resolve clauses =
  if changed then resolve resolution else resolution
  where resolution = resolveClauseSet clauses
        changed = (length resolution) /= (length clauses)

-- Lexer

data Symbol = OpenParenthesis | ClosingParenthesis | AndOperator | OrOperator | NotOperator | Identifier String | EndOfFile deriving (Eq, Show, Read)

-- | Gets the next symbol and the remaining string
scan :: String -> (Symbol, String)
scan ('(':tail) = (OpenParenthesis, tail)
scan (')':tail) = (ClosingParenthesis, tail)
scan ('&':tail) = (AndOperator, tail)
scan ('|':tail) = (OrOperator, tail)
scan ('!':tail) = (NotOperator, tail)
scan (' ':tail) = scan tail
scan "" = (EndOfFile, "")
scan all
       | not $ null ident = (Identifier ident, tail)
       | otherwise        = error $ "invalid char at " ++ tail
  where ident = takeWhile isLetter all
        tail  = dropWhile isLetter all
        isLetter a = (a `elem` ['a'..'z']) || (a `elem` ['A'..'Z'])

-- Parser

-- | Parses the string as an atomic formula or as a grouped formula
parseAtomic :: String -> (Formula, String)
parseAtomic all =
  case symbol of (Identifier name) -> (Atom name, tail)
                 OpenParenthesis   -> let (formula, tail2) = parseOr tail;
                                          (nextSymbol, realTail) = scan tail2 in case nextSymbol of ClosingParenthesis -> (formula, realTail)
                                                                                                    _ -> error ("')' expected, but " ++ (show nextSymbol) ++ " found")
                 s -> error("Identifier or '(' expected, but " ++ (show s) ++ " found")
  where (symbol, tail) = scan all

-- | Parses the string as a possibly negated atomic formula
parseNot :: String -> (Formula, String)
parseNot all = 
  case symbol of NotOperator -> let (formula, realTail) = parseNot tail in (Not formula, realTail)
                 _ -> parseAtomic all
  where (symbol, tail) = scan all

-- | Parses the string as a conjunction of formulas
parseAnd :: String -> (Formula, String)
parseAnd all = case middleSymbol of AndOperator -> (And first second, realTail)
                                    _ -> (first, firstTail)
  where (first, firstTail) = parseNot(all)
        (middleSymbol, secondTail) = scan(firstTail)
        (second, realTail) = parseAnd(secondTail)

-- | Parses the string as a disjunction of formulas
parseOr :: String -> (Formula, String)
parseOr all = case middleSymbol of OrOperator -> (Or first second, realTail)
                                   _ -> (first, firstTail)
  where (first, firstTail) = parseAnd(all)
        (middleSymbol, secondTail) = scan(firstTail)
        (second, realTail) = parseOr(secondTail)

-- | Parses the string as a formula
parse :: String -> Formula
parse str = case nextSymbol of EndOfFile -> formula
                               _  -> error $ "End of input expected, but " ++ (show nextSymbol) ++ " found"
 where (formula, tail) = parseOr str
       (nextSymbol, _) = scan tail

-- High-level functions

-- | Converts any formula to a normalized clause set that can be resolved
formulaToClauseSet :: Formula -> ClauseSet
formulaToClauseSet = normalizeClauseSet . cnfToClauseSet . formulaToCNF

-- | Checks whether the formula given as string is satisfiable
satisfiable :: String -> Bool
satisfiable str = not $ [] `elem` resolution
  where resolution = resolve . formulaToClauseSet . parse $ str
