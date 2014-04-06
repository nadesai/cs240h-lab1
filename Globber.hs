module Globber (matchGlob) where

type GlobPattern = String

type AtomSequence = [Atom]
data Atom = Any | AnyString | AnyOf String | Literal Char deriving (Show)

matchGlob :: GlobPattern -> String -> Bool
matchGlob = matchAtomSequence . toAtomSequence

toAtomSequence :: GlobPattern -> AtomSequence
toAtomSequence []   = []
toAtomSequence (x:xs) = case x of
                             '*' -> AnyString:toAtomSequence xs
                             '?' -> Any:toAtomSequence xs
                             '\\' -> nextLiteral xs
                             '[' -> toRangeAtomSequence [] xs
                             ']' -> error "Range closure without corresponding range opener"
                             _   -> (Literal x):toAtomSequence xs
                        where
                             nextLiteral (y:ys) = (Literal y):toAtomSequence ys
                             nextLiteral [] = error "Terminal escape character"

toRangeAtomSequence :: String -> GlobPattern -> AtomSequence
toRangeAtomSequence l [] = error "Range ended without closure!" 
toRangeAtomSequence l s@(x:xs) = case x of
                                    ']' -> (AnyOf l):toAtomSequence xs
                                    '\\' -> nextLiteral xs
                                    _   -> toRangeAtomSequence (x:l) xs
                                 where
                                    nextLiteral (y:ys) = toRangeAtomSequence (y:l) ys
                                    nextLiteral [] = error "Range ended without closure!"

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
