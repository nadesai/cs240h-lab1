module Globber (matchGlob) where

type GlobPattern = String

type GlobAtomList = [GlobAtom]
data GlobAtom = AnyCharacter | AnyString | AnyOf String | InRange (Char,Char)

matchGlob :: GlobPattern -> String -> Bool
matchGlob pattern str = pattern == str

toAtomList :: GlobPattern -> GlobAtomList
toAtomList = undefined

matchAtomList :: GlobAtomList -> String -> Bool
matchAtomList = undefined
