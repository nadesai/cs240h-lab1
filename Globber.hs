module Globber (matchGlob) where

type GlobPattern = String

type AtomSequence = [Atom]
data Atom = AnyChar | AnyString | AnyOf [RangeAtom] | Literal Char deriving (Show)
data RangeAtom =  Lit Char | Range Char Char deriving (Show)

matchGlob :: GlobPattern -> String -> Bool
matchGlob = matchAtomSequence . toAtomSequence

toAtomSequence :: GlobPattern -> AtomSequence
toAtomSequence []   = []
toAtomSequence (x:xs) = case x of
                          '*'  -> AnyString:toAtomSequence xs
                          '?'  -> AnyChar:toAtomSequence xs
                          '\\' -> case xs of
                                    [] -> error "Terminal escape character"
                                    (y:ys) -> (Literal y):toAtomSequence ys
                          '['  -> toRangeAtomSequence [] xs
                          ']'  -> error "Range closure without corresponding range opener"
                          _    -> (Literal x):toAtomSequence xs

toRangeAtomSequence :: [RangeAtom] -> GlobPattern -> AtomSequence
toRangeAtomSequence _ [] = error "Range ended without closure" 
toRangeAtomSequence l (x:xs) = case x of
                                 ']'  -> (AnyOf l):toAtomSequence xs
                                 '\\' -> case xs of
                                           [] -> error "Range ended without closure"
                                           (y:ys) -> toRangeAtomSequence ((Lit y):l) ys
                                 _    -> case xs of
                                           ('-':z:zs) | z /= ']' -> toRangeAtomSequence ((Range x z):l) zs
                                           ('-':'\\':z:zs) -> toRangeAtomSequence ((Range x z):l) zs
                                           _  -> toRangeAtomSequence ((Lit x):l) xs

matchAtomSequence :: AtomSequence -> String -> Bool
matchAtomSequence a@(ah:as) s@(sh:ss) = case ah of
                                          AnyChar   -> matchAtomSequence as ss 
                                          AnyString -> matchAtomSequence as s || matchAtomSequence a ss
                                          AnyOf list -> any (sh `inRange`) list && matchAtomSequence as ss
                                          Literal c -> (sh == c) && matchAtomSequence as ss
                                        where
                                          inRange :: Char -> RangeAtom -> Bool
                                          inRange c (Lit d) = c == d
                                          inRange c (Range a b) = c >= a && c <= b
matchAtomSequence (ah:as) [] = case ah of
                                 AnyString -> matchAtomSequence as []
                                 _         -> False
matchAtomSequence [] [] = True
matchAtomSequence [] _ = False 
