module Globber (matchGlob) where

type GlobPattern = String

type AtomSequence = [Atom]
data Atom = AnyChar | AnyString | AnyOf [Char] | Literal Char deriving (Show)

-- Matches the given string against the provided glob pattern.
matchGlob :: GlobPattern -> String -> Bool
matchGlob = matchAtomSequence . toAtomSequence

-- Converts the given glob pattern into a sequence of Atoms.
toAtomSequence :: GlobPattern -> AtomSequence
toAtomSequence []   = []
toAtomSequence (x:xs) = atom : (toAtomSequence remainder) where
                        (atom,remainder) = case x of
                                             '*'  -> (AnyString, xs)
                                             '?'  -> (AnyChar, xs)
                                             '\\' -> case xs of
                                                       (y:ys) -> (Literal y, ys)
                                                       [] -> error "Terminal escape character"
                                             '['  -> let (l,ys) = parseLeadingRange xs in 
                                                     (AnyOf l, ys)
                                             ']'  -> error "Range closure without corresponding opener"
                                             _    -> (Literal x, xs)

-- Parses the leading range of the GlobPattern and returns a tuple of a string containing all characters in the range,
-- and the remaining characters in the pattern.
parseLeadingRange :: GlobPattern -> ([Char], GlobPattern)
parseLeadingRange = parseLeadingRange' [] 
  where
  parseLeadingRange' :: [Char] -> GlobPattern -> ([Char],GlobPattern)
  parseLeadingRange' _ [] = error "Range ended without closure" 
  parseLeadingRange' l (x:xs) | x == ']'  = (l, xs)
                              | otherwise = parseLeadingRange' l' remainder where
                                            (l',remainder) = case x of
                                                               '\\' -> case xs of
                                                                         (y:'-':'\\':z:zs) -> ([y..z] ++ l, zs)
                                                                         (y:'-':z:zs) | z /= ']' -> ([y..z] ++ l, zs)
                                                                         (y:ys) -> (y:l, ys)
                                                                         [] -> error "Range ended without closure"
                                                               _    -> case xs of
                                                                         ('-':'\\':z:zs) -> ([x..z] ++ l, zs)
                                                                         ('-':z:zs) | z /= ']' -> ([x..z] ++ l, zs)
                                                                         _  -> (x:l, xs)

-- Matches the given string against the given AtomSequence.
matchAtomSequence :: AtomSequence -> String -> Bool
matchAtomSequence a@(ah:as) s@(sh:ss) = case ah of
                                          AnyChar   -> matchAtomSequence as ss 
                                          AnyString -> matchAtomSequence as s || matchAtomSequence a ss
                                          AnyOf str -> (sh `elem` str) && matchAtomSequence as ss
                                          Literal c -> (sh == c) && matchAtomSequence as ss
matchAtomSequence (ah:as) [] = case ah of
                                 AnyString -> matchAtomSequence as []
                                 _         -> False
matchAtomSequence [] [] = True
matchAtomSequence [] _ = False 
