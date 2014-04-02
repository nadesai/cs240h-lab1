module Globber (matchGlob) where

type GlobPattern = String

type AtomSequence = [Atom]
data Atom = AnyCharacter | AnyString | AnyOf String | InRange (Char,Char)

matchGlob :: GlobPattern -> String -> Bool
matchGlob pattern str = pattern == str

toAtomSequence :: GlobPattern -> AtomSequence
toAtomSequence = undefined

matchAtomSequence :: AtomSequence -> String -> Bool
matchAtomSequence = undefined
