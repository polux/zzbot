{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Config
import Control.Applicative
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import Data.Validation
import Exec
import System.Environment
import System.Exit
import XmlParse

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Options.Applicative as Opt

process :: Bool -- ^ Whether to print (True) or execute the builder (False)
        -> String -- ^ The file containing the builder's description
        -> IO ExitCode
process printOrExec filepath  = do
  mbuilders :: XmlValidation (NonEmpty Builder) <- parseXmlFile filepath
  case mbuilders of
      Failure (err :: Set.Set XmlParsingError) -> do
        putStrLn $ unlines $ map show $ Set.toList err
        return (ExitFailure 1)
      Success (builders :: NonEmpty Builder) ->
        if printOrExec
        then do
          let btexts :: NonEmpty LT.Text = NE.map renderAsXml builders
          LT.putStrLn $ LT.unlines $ NE.toList btexts
          return ExitSuccess
        else andExitCodes <$> traverse runBuild builders

andExitCode :: ExitCode -> ExitCode -> ExitCode
andExitCode (ExitFailure i) (ExitFailure j) = ExitFailure (max i j)
andExitCode (ExitFailure i) _ = ExitFailure i
andExitCode _ (ExitFailure j) = ExitFailure j
andExitCode c1 c2 = c1

andExitCodes
  :: NonEmpty ExitCode -- ^ The list of return codes to combine
  -> ExitCode
andExitCodes = foldr1 andExitCode

{- HLINT ignore Options -}
data Options = Options { optFilenames :: NonEmpty String, optPrint :: Bool }

optionsParser :: Opt.Parser Options
optionsParser = Options <$> NE.some1 (Opt.strArgument (Opt.metavar "FILE"))
                        <*> Opt.switch (Opt.long "print" <> Opt.short 'p' <> Opt.help "print the builder's interpretation, but do not execute it")

optionsParserInfo :: Opt.ParserInfo Options
optionsParserInfo = Opt.info (optionsParser <**> Opt.helper) Opt.fullDesc

main :: IO ()
main = do
  options <- Opt.execParser optionsParserInfo
  let print :: Bool = optPrint options
  codes <- traverse (process print) (optFilenames options)  
  exitWith $ andExitCodes codes

-- kept for future reference:
runSubstedBuild :: Validation (Set.Set ValidationError) Builder -> IO ExitCode
runSubstedBuild eBuilder =
  case eBuilder of
     Failure _       -> return $ ExitFailure 1
     Success builder -> runBuild builder

renderErrors :: Set.Set ValidationError -> LT.Text
renderErrors = LT.pack . unlines . map show . Set.toList

main0 :: IO ()
main0 = do
  LT.putStrLn (renderAsXml shellCmd0)
  LT.putStrLn (renderAsXml builder)
  LT.putStrLn (validation renderErrors renderAsXml substedBuilder)
  rc :: ExitCode <- runSubstedBuild substedBuilder
  putStrLn $ "Builder finished with " ++ show rc
  where shellCmd0 :: Step = ShellCmd ["ls", "$[mydir]"]
        shellCmd1 = ShellCmd ["tree", "-L", "1", "$[mydir]"]
        shellCmd2 = ShellCmd ["wrongcmd"]
        builder :: Builder = Builder "bname" [shellCmd0, shellCmd1, shellCmd2]
        subst :: Subst = Map.fromList [("mydir", "/")]
        substedBuilder = substitute ("$[", "]") subst builder