Resolution tool for propositional logic
=======================================

This is my first little project in haskell. It accepts a string, interprets it as a propositional formula, converts it to a set of clauses and then applies the resolution algorithm to find out whether the formula is satisfiable or not.

Usage
-----

    satisfiable "(A | !B) & (!A | B) & A & !B"
    
results in `False`, where

    satisfiable "(A | !B) & (!A | B) & A & B"
    
results in `True`.
