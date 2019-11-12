{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.Either
import Data.List
import Data.Maybe
import Text.Printf
import Text.XML

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

-- AST
data Step =
      SetPropertyFromValue { prop :: String, value :: String }
    | ShellCmd             { cmd :: [String] }
data Builder = Builder { name :: String, steps :: [Step] }
data Config = Config { builders :: [Builder], subst :: Subst }

-- types
type Subst = Map.Map String String

class Substable a where
    -- The result of applying a substitution (Right) or errors (Left), using the delimiters given as first argument
    substitute :: (String, String) -> Subst -> a -> Either [String] a

-- split_around 'b' 'foobar' = Just(("foo", "ar"))
-- splitAround 'f' 'foobar' = Just(("", "oobar"))
-- splitAround 'r' 'foobar' = Just(("fooba", "")
-- splitAround 'c' 'foobar' = Nothing
splitAround :: String -> String -> Maybe (String, String)
splitAround sep text =
  if sep `isPrefixOf` text
  then Just ("", drop (length sep) text)
  else let fail = (length sep > length text || null text) in
    if fail then Nothing
    else let fst = head text
             tailResult = splitAround sep (tail text)
          in case tailResult of
                Nothing             -> Nothing
                Just(before, after) -> Just(fst : before, after)

-- splitDelimiters ("(", ")") "foo(var)bar" returns Just("foo", "var", "bar")
-- splitDelimiters ("(", ")") "(var)bar" returns Just("", "var", "bar")
-- splitDelimiters ("(", ")") "foo(var)" returns Just("foo", "var")
-- splitDelimiters ("(", ")") "foobar" returns Nothing
splitDelimiters :: (String, String) -> String -> Maybe (String, String, String)
splitDelimiters delimiters text = do
  (beforeOpen, afterOpen) <- splitAround (fst delimiters) text
  (beforeClose, afterClose) <- splitAround (snd delimiters) afterOpen
  return (beforeOpen, beforeClose, afterClose)

type VarName = String

-- First argument is the pair of opening and closing delimiters
-- Second argument is the string to parse
-- Returns the string parsed, concatening String content and variable names
-- For example: parseVars ("(", ")") "foo(bar)chose" =
--                [Left "foo", Right "bar", Left "chose"]
parseVars :: (String, String) -> String -> [Either String VarName]
parseVars delimiters text =
    case splitDelimiters delimiters text of
        Nothing -> [Left text | not (null text)]
        Just (before, var, after) -> [Left before, Right var] ++ parseVars delimiters after

-- delimiters -> substitution -> the text to substitute -> A list of errors
validateVars :: (String, String) -> Subst -> String -> [String]
validateVars delimiters subst text =
    map (\key -> printf "key not mapped by substitution: %s. Substitution's domain is: %s" key domain) missingVars
    where allVars = rights (parseVars delimiters text)
          missingVars = filter (`Map.notMember` subst) allVars
          domain = unwords $ Map.keys subst

-- Replace variables enclosed in delimiters and return the resulting string (Right)
-- or a list of errors (Left) if some keys are not mapped by the substitution
applySubstitution :: (String, String) -> Subst -> String -> Either [String] String
applySubstitution delimiters subst text =
    if not (null errors) then Left errors else
    Right (intercalate "" $ catMaybes pieces')
    where errors :: [String] = validateVars delimiters subst text
          pieces :: [Either String VarName] = parseVars delimiters text
          substApplier (Left text) = Just text
          substApplier (Right varName) = Map.lookup varName subst
          pieces' = map substApplier pieces

---------
-- XML --
---------

class ToXml a where
    toXml :: a -> Element

simpleName :: String -> Name
simpleName name = Name (T.pack name) Nothing Nothing

(=:) :: String -> String -> Map.Map Name T.Text
attr =: value = Map.singleton (simpleName attr) (T.pack value)

instance ToXml Step where
    toXml (SetPropertyFromValue prop value) = Element "set_property" (prop =: value) []
    toXml (ShellCmd cmdList) = Element "shell" ("command" =: unwords cmdList) []

instance ToXml Builder where
    toXml (Builder name steps) = Element "builder" Map.empty (map (NodeElement . toXml) steps)

renderAsXml :: ToXml a => a -> LT.Text
renderAsXml x = renderText settings doc
 where
  settings = def {rsPretty=True, rsXMLDeclaration=False}
  doc = Document (Prologue [] Nothing []) (toXml x) []

---------------------------------
-- Implementation of Substable --
---------------------------------

-- Creates an instance overlapping with the one for lists, because hey
-- String is [Char]!
-- instance Substable String where
--    substitute delimiters subst text = applySubstitution delimiters subst text

-- Lift substitute to lists
instance Substable a => Substable [a] where
    substitute delimiters subst list =
        let base :: [Either [String] a] = map (substitute delimiters subst) list
            (failures :: [[String]], images :: [a]) = partitionEithers base in
                if not (null failures)
                then Left $ concat failures
                else Right images

instance Substable Step where
    substitute delimiters subst (SetPropertyFromValue prop value) = do
        valueImage <- applySubstitution delimiters subst value
        return (SetPropertyFromValue prop valueImage)
    substitute delimiters subst (ShellCmd (cmdList :: [String])) =
        if not (null failures)
        then Left $ concat failures
        else Right $ ShellCmd images
        where cmdListMapped :: [Either [String] String] = map (applySubstitution delimiters subst) cmdList
              (failures :: [[String]], images) = partitionEithers cmdListMapped

instance Substable Builder where
    substitute delimiters subst (Builder name steps) = do
        substedName <- applySubstitution delimiters subst name
        substedSteps <- substitute delimiters subst steps
        return (Builder substedName substedSteps)
