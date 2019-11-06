{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Config where

import Data.Either
import Data.List
import Text.Printf
import qualified Data.Map.Strict as Map
import Text.Megaparsec.Char
import Text.Megaparsec
import Data.Void
import Data.Maybe
import Safe

-- AST
data Step =
      SetPropertyFromValue { prop :: String, value :: String }
    | ShellCmd             { cmd :: [String] }
data Builder = Builder { name :: String, steps :: [Step] }
data Config = Config { builders :: [Builder], subst :: Subst }

-- types
type Subst = Map.Map String String

class Substable a where
    -- The result of applying a substitution (Right) or an error message (Left)
    substitute :: Subst -> a -> Either String a

-- Substitution of leaves

data Chunk = Text String | Var String
  deriving (Eq, Show)

type Leaf = [Chunk]

parseLeaf :: String -> String -> String -> Leaf
parseLeaf open close str =
  fromJustNote "leaf should always succeed" (parseMaybe @Void leaf str)
 where
  leaf = do
    prefix <- many (try textThenVar)
    suffix <- many anySingle
    return (concat prefix ++ [Text suffix])

  textThenVar = do
    before <- manyTill anySingle (string open)
    inside <- manyTill anySingle (string close)
    return [Text before, Var inside]

validateLeaf :: Subst -> Leaf -> [String]
validateLeaf subst leaf = mapMaybe validateChunk leaf
 where
  validateChunk (Var x) | not (x `Map.member` subst) = Just (x ++ " not found in substitution")
  validateChunk _ = Nothing

substituteLeaf :: Subst -> Leaf -> String
substituteLeaf subst leaf = concatMap substituteChunk leaf
 where
  substituteChunk (Text str) = str
  substituteChunk (Var x) = fromJustNote "precondition violated" (x `Map.lookup` subst)

-- Replace $[key] by subst[key] for every $[key] found
-- Fail (return Left) if some key is not mapped by the substitution
applySubstitution :: Subst -> String -> Either String String
applySubstitution subst text
  | null validationErrors = Right (substituteLeaf subst leaf)
  | otherwise = Left (unlines validationErrors)
 where
  leaf = parseLeaf "$[" "]" text
  validationErrors = validateLeaf subst leaf

attrValueString :: String -> String -> String
attrValueString attr value = printf "%s=\"%s\"" attr value

----------------------------
-- Implementation of Show --
----------------------------

instance Show Step where
    show (SetPropertyFromValue prop value)= printf "<set_property %s/>" (attrValueString prop value)
    show (ShellCmd cmdList)= "<shell command=\"" ++ (unwords cmdList) ++ "\"/>"

instance Show Builder where
    show (Builder name steps) = 
        let shownSteps :: [String] = map show steps
            indentedSteps :: [String] = map (\x -> " " ++ x) shownSteps
            linedSteps :: String = intercalate "\n" indentedSteps
         in "<builder>\n" ++ linedSteps ++ "\n</builder>"

---------------------------------
-- Implementation of Substable --
---------------------------------

-- Lift substitute to lists
instance Substable a => Substable [a] where
    substitute subst list = 
        let base :: [Either String a] = map (substitute subst) list
            (failures, images) = partitionEithers base in
                if length(failures) > 0
                then Left(intercalate "\n" failures) -- report all errors at once, not only the first one
                else Right images

instance Substable Step where
    substitute subst (SetPropertyFromValue prop value) = do
        valueImage <- applySubstitution subst value
        Right (SetPropertyFromValue prop valueImage)
    substitute subst (ShellCmd cmdList) =
        let cmdListImage :: [Either String String] = map (applySubstitution subst) cmdList
            (failures, images) = partitionEithers cmdListImage in
                if length(failures) > 0
                then Left(intercalate "\n" failures)
                else Right (ShellCmd images)

instance Substable Builder where
    substitute subst (Builder name steps) = do
        substedName <- applySubstitution subst name
        substedSteps <- substitute subst steps
        return (Builder substedName substedSteps)
