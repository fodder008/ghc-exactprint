{-# LANGUAGE CPP #-}
-- | Use "runhaskell Setup.hs test" or "cabal test" to run these tests.
module Main where

import Language.Haskell.GHC.ExactPrint
-- import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils

import GHC.Paths ( libdir )

import qualified DynFlags      as GHC
import qualified FastString    as GHC
import qualified GHC           as GHC
import qualified MonadUtils    as GHC
import qualified Outputable    as GHC

import qualified GHC.SYB.Utils as SYB

import Control.Monad
import System.Directory
import System.FilePath
import System.IO

import Test.HUnit

-- import qualified Data.Map as Map

-- ---------------------------------------------------------------------

main :: IO Counts
main = runTestTT tests

-- tests = TestCase (do r <- manipulateAstTest "tests/examples/LetStmt.hs" "Layout.LetStmt"
--                      assertBool "test" r )

tests = TestList
  [
    mkTestMod "tests/examples/LetStmt.hs"               "Layout.LetStmt"
  , mkTestMod "tests/examples/LetExpr.hs"               "LetExpr"
  , mkTestMod "tests/examples/ExprPragmas.hs"           "ExprPragmas"
  , mkTestMod "tests/examples/ListComprehensions.hs"    "Main"
  , mkTestMod "tests/examples/MonadComprehensions.hs"   "Main"
  , mkTestMod "tests/examples/FunDeps.hs"               "Main"
  , mkTestMod "tests/examples/ImplicitParams.hs"        "Main"
  , mkTestMod "tests/examples/RecursiveDo.hs"           "Main"
  , mkTestMod "tests/examples/TypeFamilies.hs"          "Main"
  , mkTestMod "tests/examples/MultiParamTypeClasses.hs" "Main"
  , mkTestMod "tests/examples/DataFamilies.hs"          "DataFamilies"
  , mkTestMod "tests/examples/Deriving.hs"              "Main"
  , mkTestMod "tests/examples/Default.hs"               "Main"
  , mkTestMod "tests/examples/ForeignDecl.hs"           "ForeignDecl"
  , mkTestMod "tests/examples/Warning.hs"               "Warning"
  , mkTestMod "tests/examples/Annotations.hs"           "Annotations"
  , mkTestMod "tests/examples/DocDecls.hs"              "DocDecls"
  , mkTestModTH "tests/examples/QuasiQuote.hs"          "QuasiQuote"
  , mkTestMod "tests/examples/Roles.hs"                 "Roles"
  , mkTestMod "tests/examples/Splice.hs"                "Splice"
  , mkTestMod "tests/examples/ImportsSemi.hs"           "ImportsSemi"
  , mkTestMod "tests/examples/Stmts.hs"                 "Stmts"
  , mkTestMod "tests/examples/Mixed.hs"                 "Main"
  , mkTestMod "tests/examples/Arrow.hs"                 "Arrow"
  , mkTestMod "tests/examples/PatSynBind.hs"            "Main"
  , mkTestMod "tests/examples/HsDo.hs"                  "HsDo"
  , mkTestMod "tests/examples/ForAll.hs"                "ForAll"
  , mkTestMod "tests/examples/PArr.hs"                  "PArr"
  , mkTestMod "tests/examples/ViewPatterns.hs"          "Main"
  , mkTestMod "tests/examples/BangPatterns.hs"          "Main"
  , mkTestMod "tests/examples/Associated.hs"            "Main"
  , mkTestMod "tests/examples/Move1.hs"                 "Move1"
  , mkTestMod "tests/examples/Rules.hs"                 "Rules"
  , mkTestMod "tests/examples/TypeOperators.hs"         "Main"
  , mkTestMod "tests/examples/NullaryTypeClasses.hs"    "Main"
  , mkTestMod "tests/examples/FunctionalDeps.hs"        "Main"
  , mkTestMod "tests/examples/DerivingOC.hs"            "Main"
  , mkTestMod "tests/examples/GenericDeriving.hs"       "Main"
  , mkTestMod "tests/examples/OverloadedStrings.hs"     "Main"
  , mkTestMod "tests/examples/RankNTypes.hs"            "Main"
  , mkTestMod "tests/examples/Existential.hs"           "Main"
  , mkTestMod "tests/examples/ScopedTypeVariables.hs"   "Main"
  , mkTestMod "tests/examples/Arrows.hs"                "Main"
  , mkTestMod "tests/examples/TH.hs"                    "Main"
  , mkTestMod "tests/examples/StaticPointers.hs"        "Main"
  , mkTestMod "tests/examples/DataDecl.hs"              "Main"
  , mkTestMod "tests/examples/Guards.hs"                "Main"
  , mkTestMod "tests/examples/RebindableSyntax.hs"      "Main"
  , mkTestMod "tests/examples/RdrNames.hs"              "RdrNames"
  , mkTestMod "tests/examples/Vect.hs"                  "Vect"
  , mkTestMod "tests/examples/Tuple.hs"                 "Main"
  , mkTestMod "tests/examples/ExtraConstraints1.hs"     "ExtraConstraints1"
  , mkTestMod "tests/examples/AddAndOr3.hs"             "AddAndOr3"
  , mkTestMod "tests/examples/Ann01.hs"                 "Ann01"
  , mkTestMod "tests/examples/StrictLet.hs"             "Main"
  , mkTestMod "tests/examples/Cg008.hs"                 "Cg008"
  , mkTestMod "tests/examples/T2388.hs"                 "T2388"
  , mkTestMod "tests/examples/T3132.hs"                 "T3132"
  , mkTestMod "tests/examples/Stream.hs"                "Stream"
  , mkTestMod "tests/examples/Trit.hs"                  "Trit"
  , mkTestMod "tests/examples/DataDecl.hs"              "Main"
  , mkTestMod "tests/examples/Zipper.hs"                "Zipper"
  , mkTestMod "tests/examples/Sigs.hs"                  "Sigs"
  , mkTestMod "tests/examples/Utils2.hs"                "Utils2"
  , mkTestMod "tests/examples/EmptyMostlyInst.hs"       "EmptyMostlyInst"
  , mkTestMod "tests/examples/EmptyMostlyNoSemis.hs"    "EmptyMostlyNoSemis"
  , mkTestMod "tests/examples/Dead1.hs"                 "Dead1"
  , mkTestMod "tests/examples/EmptyMostly.hs"           "EmptyMostly"
  , mkTestMod "tests/examples/FromUtils.hs"             "Main"
  , mkTestMod "tests/examples/DocDecls.hs"              "DocDecls"
  , mkTestMod "tests/examples/RecordUpdate.hs"          "Main"
  -- , mkTestMod "tests/examples/Unicode.hs"               "Main"
  , mkTestMod "tests/examples/B.hs"                     "Main"
  , mkTestMod "tests/examples/LayoutWhere.hs"           "Main"
  , mkTestMod "tests/examples/LayoutLet.hs"             "Main"
  , mkTestMod "tests/examples/Deprecation.hs"           "Deprecation"
  , mkTestMod "tests/examples/Infix.hs"                 "Main"
  , mkTestMod "tests/examples/BCase.hs"                 "Main"
  , mkTestMod "tests/examples/AltsSemis.hs"             "Main"

  , mkTestMod "tests/examples/LetExprSemi.hs"           "LetExprSemi"
  ]

mkTestMain :: FilePath -> Test
mkTestMain fileName = TestCase (do r <- manipulateAstTest fileName "Main"
                                   assertBool fileName r )

mkTestMod :: FilePath -> String -> Test
mkTestMod fileName modName
  = TestCase (do r <- manipulateAstTest fileName modName
                 assertBool fileName r )

mkTestModTH :: FilePath -> String -> Test
mkTestModTH fileName modName
  = TestCase (do r <- manipulateAstTestTH fileName modName
                 assertBool fileName r )

-- ---------------------------------------------------------------------

tt :: IO Bool
tt = do
{-
    manipulateAstTest "tests/examples/LetStmt.hs"               "Layout.LetStmt"
    manipulateAstTest "tests/examples/LetExpr.hs"               "LetExpr"
    manipulateAstTest "tests/examples/ExprPragmas.hs"           "ExprPragmas"
    manipulateAstTest "tests/examples/ListComprehensions.hs"    "Main"
    manipulateAstTest "tests/examples/MonadComprehensions.hs"   "Main"
    manipulateAstTest "tests/examples/FunDeps.hs"               "Main"
    manipulateAstTest "tests/examples/ImplicitParams.hs"        "Main"
    manipulateAstTest "tests/examples/RecursiveDo.hs"           "Main"
    manipulateAstTest "tests/examples/TypeFamilies.hs"          "Main"
    manipulateAstTest "tests/examples/MultiParamTypeClasses.hs" "Main"
    manipulateAstTest "tests/examples/DataFamilies.hs"          "DataFamilies"
    manipulateAstTest "tests/examples/Deriving.hs"              "Main"
    manipulateAstTest "tests/examples/Default.hs"               "Main"
    manipulateAstTest "tests/examples/ForeignDecl.hs"           "ForeignDecl"
    manipulateAstTest "tests/examples/Warning.hs"               "Warning"
    manipulateAstTest "tests/examples/Annotations.hs"           "Annotations"
    manipulateAstTest "tests/examples/DocDecls.hs"              "DocDecls"
    manipulateAstTestTH "tests/examples/QuasiQuote.hs"          "QuasiQuote"
    manipulateAstTest "tests/examples/Roles.hs"                 "Roles"
    manipulateAstTest "tests/examples/Splice.hs"                "Splice"
    manipulateAstTest "tests/examples/ImportsSemi.hs"           "ImportsSemi"
    manipulateAstTest "tests/examples/Stmts.hs"                 "Stmts"
    manipulateAstTest "tests/examples/Mixed.hs"                 "Main"
    manipulateAstTest "tests/examples/Arrow.hs"                 "Arrow"
    manipulateAstTest "tests/examples/PatSynBind.hs"            "Main"
    manipulateAstTest "tests/examples/HsDo.hs"                  "HsDo"
    manipulateAstTest "tests/examples/ForAll.hs"                "ForAll"
    manipulateAstTest "tests/examples/PArr.hs"                  "PArr"
    manipulateAstTest "tests/examples/ViewPatterns.hs"          "Main"
    manipulateAstTest "tests/examples/BangPatterns.hs"          "Main"
    manipulateAstTest "tests/examples/Associated.hs"            "Main"
    manipulateAstTest "tests/examples/Move1.hs"                 "Move1"
    manipulateAstTest "tests/examples/Rules.hs"                 "Rules"
    manipulateAstTest "tests/examples/TypeOperators.hs"         "Main"
    manipulateAstTest "tests/examples/NullaryTypeClasses.hs"    "Main"
    manipulateAstTest "tests/examples/FunctionalDeps.hs"        "Main"
    manipulateAstTest "tests/examples/DerivingOC.hs"            "Main"
    manipulateAstTest "tests/examples/GenericDeriving.hs"       "Main"
    manipulateAstTest "tests/examples/OverloadedStrings.hs"     "Main"
    manipulateAstTest "tests/examples/RankNTypes.hs"            "Main"
    manipulateAstTest "tests/examples/Existential.hs"           "Main"
    manipulateAstTest "tests/examples/ScopedTypeVariables.hs"   "Main"
    manipulateAstTest "tests/examples/Arrows.hs"                "Main"
    manipulateAstTest "tests/examples/TH.hs"                    "Main"
    manipulateAstTest "tests/examples/StaticPointers.hs"        "Main"
    manipulateAstTest "tests/examples/DataDecl.hs"              "Main"
    manipulateAstTest "tests/examples/Guards.hs"                "Main"
    manipulateAstTest "tests/examples/RebindableSyntax.hs"      "Main"
    manipulateAstTest "tests/examples/RdrNames.hs"              "RdrNames"
    manipulateAstTest "tests/examples/Vect.hs"                  "Vect"
    manipulateAstTest "tests/examples/Tuple.hs"                 "Main"
    manipulateAstTest "tests/examples/ExtraConstraints1.hs"     "ExtraConstraints1"
    manipulateAstTest "tests/examples/AddAndOr3.hs"             "AddAndOr3"
    manipulateAstTest "tests/examples/Ann01.hs"                 "Ann01"
    manipulateAstTest "tests/examples/StrictLet.hs"             "Main"
    manipulateAstTest "tests/examples/Cg008.hs"                 "Cg008"
    manipulateAstTest "tests/examples/T2388.hs"                 "T2388"
    manipulateAstTest "tests/examples/T3132.hs"                 "T3132"
    manipulateAstTest "tests/examples/Stream.hs"                "Stream"
    manipulateAstTest "tests/examples/Trit.hs"                  "Trit"
    manipulateAstTest "tests/examples/DataDecl.hs"              "Main"
    manipulateAstTest "tests/examples/Zipper.hs"                "Zipper"
    manipulateAstTest "tests/examples/Sigs.hs"                  "Sigs"
    manipulateAstTest "tests/examples/Utils2.hs"                "Utils2"
    manipulateAstTest "tests/examples/EmptyMostlyInst.hs"       "EmptyMostlyInst"
    manipulateAstTest "tests/examples/EmptyMostlyNoSemis.hs"    "EmptyMostlyNoSemis"
    manipulateAstTest "tests/examples/Dead1.hs"                 "Dead1"
    manipulateAstTest "tests/examples/EmptyMostly.hs"           "EmptyMostly"
    manipulateAstTest "tests/examples/FromUtils.hs"             "Main"
    manipulateAstTest "tests/examples/DocDecls.hs"              "DocDecls"
    manipulateAstTest "tests/examples/RecordUpdate.hs"          "Main"
    -- manipulateAstTest "tests/examples/Unicode.hs"               "Main"
    manipulateAstTest "tests/examples/B.hs"                     "Main"
    manipulateAstTest "tests/examples/LayoutWhere.hs"           "Main"
    manipulateAstTest "tests/examples/Deprecation.hs"           "Deprecation"
    manipulateAstTest "tests/examples/Infix.hs"                 "Main"
    manipulateAstTest "tests/examples/BCase.hs"                 "Main"
    manipulateAstTest "tests/examples/AltsSemis.hs"             "Main"
    manipulateAstTest "tests/examples/LetExprSemi.hs"           "LetExprSemi"
-}
    manipulateAstTest "tests/examples/LayoutLet.hs"             "Main"
{-
    manipulateAstTest "tests/examples/Cpp.hs"                   "Main"
    manipulateAstTest "tests/examples/Lhs.lhs"                  "Main"
    manipulateAstTest "tests/examples/ParensAroundContext.hs"   "ParensAroundContext"
    manipulateAstTest "tests/examples/EmptyMostly2.hs"          "EmptyMostly2"
    manipulateAstTest "tests/examples/Foo.hs"                   "Main"
-}

-- | Where all the tests are to be found
examplesDir :: FilePath
examplesDir = "tests" </> "examples"

examplesDir2 :: FilePath
examplesDir2 = "examples"

manipulateAstTest :: FilePath -> String -> IO Bool
manipulateAstTest file modname = manipulateAstTest' False True file modname

manipulateAstTestTH :: FilePath -> String -> IO Bool
manipulateAstTestTH file modname = manipulateAstTest' True True file modname

manipulateAstTest' :: Bool -> Bool -> FilePath -> String -> IO Bool
manipulateAstTest' useTH useRenamed file modname = do
  let out    = file <.> "out"

  contents <- readUTF8File file
  (ghcAnns,t) <- parsedFileGhc file modname useTH
  let
    parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t
    Just renamed = GHC.tm_renamed_source t
    parsedAST = SYB.showData SYB.Parser 0 parsed

    -- try to pretty-print; summarize the test result
    ann = annotateAST parsed ghcAnns
      `debug` ("ghcAnns:" ++ showGhc ghcAnns)

    printed = if useRenamed
                then exactPrintRenamed    renamed parsed ann
                else exactPrintAnnotation parsed  []     ann
    result =
            if printed == contents
              then "Match\n"
              else printed ++ "\n==============\n"
                    ++ "lengths:" ++ show (length printed,length contents) ++ "\n"
                    ++ parsedAST
  -- putStrLn $ "Test:parsed=" ++ parsedAST
  writeFile out $ result
  -- putStrLn $ "Test:ann organised:" ++ showGhc (organiseAnns ann)
  putStrLn $ "Test:showdata:" ++ showAnnData (organiseAnns ann) 0 parsed
  return ("Match\n"  == result)



-- ---------------------------------------------------------------------
-- |Result of parsing a Haskell source file. It is simply the
-- TypeCheckedModule produced by GHC.
type ParseResult = GHC.TypecheckedModule

parsedFileGhc :: String -> String -> Bool -> IO (GHC.ApiAnns,ParseResult)
parsedFileGhc fileName modname useTH = do
    putStrLn $ "parsedFileGhc:" ++ show fileName
#if __GLASGOW_HASKELL__ > 704
    GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $ do
#else
    GHC.defaultErrorHandler GHC.defaultLogAction $ do
#endif
      GHC.runGhc (Just libdir) $ do
        dflags <- GHC.getSessionDynFlags
        let dflags' = foldl GHC.xopt_set dflags
                           [GHC.Opt_Cpp, GHC.Opt_ImplicitPrelude, GHC.Opt_MagicHash]

            dflags'' = dflags' { GHC.importPaths = ["./tests/examples/","../tests/examples/",
                                                    "./src/","../src/"] }

            tgt = if useTH then GHC.HscInterpreted
                           else GHC.HscNothing -- allows FFI
            dflags''' = dflags'' { GHC.hscTarget = tgt,
                                   GHC.ghcLink =  GHC.LinkInMemory
                                  , GHC.packageFlags = [GHC.ExposePackage (GHC.PackageArg "ghc") (GHC.ModRenaming False [])]
                                 }

            dflags4 = if False -- useHaddock
                        then GHC.gopt_set (GHC.gopt_set dflags''' GHC.Opt_Haddock)
                                       GHC.Opt_KeepRawTokenStream
                        else GHC.gopt_set dflags'''
                                       GHC.Opt_KeepRawTokenStream
                        -- else GHC.gopt_set (GHC.gopt_unset dflags''' GHC.Opt_Haddock)
                        --               GHC.Opt_KeepRawTokenStream

        (dflags5,args,warns) <- GHC.parseDynamicFlagsCmdLine dflags4 [GHC.noLoc "-package ghc"]
        GHC.liftIO $ putStrLn $ "dflags set:(args,warns)" ++ show (map GHC.unLoc args,map GHC.unLoc warns)
        void $ GHC.setSessionDynFlags dflags5
        -- GHC.liftIO $ putStrLn $ "dflags set"

        target <- GHC.guessTarget fileName Nothing
        GHC.setTargets [target]
        GHC.liftIO $ putStrLn $ "target set:" ++ showGhc (GHC.targetId target)
        void $ GHC.load GHC.LoadAllTargets -- Loads and compiles, much as calling make
        -- GHC.liftIO $ putStrLn $ "targets loaded"
        g <- GHC.getModuleGraph
        let showStuff ms = show (GHC.moduleNameString $ GHC.moduleName $ GHC.ms_mod ms,GHC.ms_location ms)
        -- GHC.liftIO $ putStrLn $ "module graph:" ++ (intercalate "," (map showStuff g))

        modSum <- GHC.getModSummary $ GHC.mkModuleName modname
        -- GHC.liftIO $ putStrLn $ "got modSum"
        -- let modSum = head g
        p <- GHC.parseModule modSum
        -- GHC.liftIO $ putStrLn $ "got parsedModule"
        t <- GHC.typecheckModule p
        GHC.liftIO $ putStrLn $ "typechecked"
        -- toks <- GHC.getRichTokenStream (GHC.ms_mod modSum)
        -- GHC.liftIO $ putStrLn $ "toks"
        let anns = GHC.pm_annotations p
        GHC.liftIO $ putStrLn $ "anns"
        return (anns,t)

readUTF8File :: FilePath -> IO String
readUTF8File fp = openFile fp ReadMode >>= \h -> do
        hSetEncoding h utf8
        hGetContents h

-- ---------------------------------------------------------------------

pwd :: IO FilePath
pwd = getCurrentDirectory

cd :: FilePath -> IO ()
cd = setCurrentDirectory

-- ---------------------------------------------------------------------

