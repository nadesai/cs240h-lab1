module Globber (matchGlob) where

type GlobPattern = String

type AtomSequence = [Atom]
data Atom = Any | AnyString | AnyOf String | Literal Char

matchGlob :: GlobPattern -> String -> Bool
matchGlob = matchAtomSequence . toAtomSequence

toAtomSequence :: GlobPattern -> AtomSequence
toAtomSequence []   = []
toAtomSequence (x:xs) = (Literal x):toAtomSequence xs

matchAtomSequence :: AtomSequence -> String -> Bool
matchAtomSequence a@(ah:as) s@(sh:ss) = case ah of
                              Any       -> matchAtomSequence as ss 
                              AnyString -> matchAtomSequence as s || matchAtomSequence a ss
                              AnyOf str -> (sh `elem` str) && matchAtomSequence as ss
                              Literal c -> (sh == c) && matchAtomSequence as ss
matchAtomSequence (ah:as) [] = case ah of
                              AnyString -> matchAtomSequence as []
                              _         -> False
matchAtomSequence [] [] = True
matchAtomSequence [] _ = False 
