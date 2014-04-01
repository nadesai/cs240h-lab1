module Globber (matchGlob) where

type GlobPattern = String

matchGlob :: GlobPattern -> String -> Bool
matchGlob pattern str = pattern == str

