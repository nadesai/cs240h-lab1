module Globber (matchGlob) where

type GlobPattern = String

type AtomSequence = [Atom]
data Atom = Any | AnyString | AnyOf String | AnyIn (Char,Char)

matchGlob :: GlobPattern -> String -> Bool
matchGlob = matchAtomSequence . toAtomSequence

toAtomSequence :: GlobPattern -> AtomSequence
toAtomSequence []   = []
toAtomSequence (x:xs) = toAtomSequence xs

matchAtomSequence :: AtomSequence -> String -> Bool
matchAtomSequence [] = null -- if our sequence is null, our string must be as well
matchAtomSequence (a:as) = case a of
                              Any         -> matchAtomSequence as
                              AnyString   -> matchAtomSequence as
                              AnyOf s     -> matchAtomSequence as
                              AnyIn (c,d) -> matchAtomSequence as
