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
main = runTestTT $ TestList [ -- TestLabel "ParsedSource" (tests False)
                              TestLabel "RenamedSource" (tests True)
                            ]

tests :: Bool -> Test
tests useRn = TestList
  [
    mkTestMod useRn "tests/examples/LetStmt.hs"               "Layout.LetStmt"
  , mkTestMod useRn "tests/examples/LetExpr.hs"               "LetExpr"
  , mkTestMod useRn "tests/examples/ExprPragmas.hs"           "ExprPragmas"
  , mkTestMod useRn "tests/examples/ListComprehensions.hs"    "Main"
  , mkTestMod useRn "tests/examples/MonadComprehensions.hs"   "Main"
  , mkTestMod useRn "tests/examples/FunDeps.hs"               "Main"
  , mkTestMod useRn "tests/examples/ImplicitParams.hs"        "Main"
  , mkTestMod useRn "tests/examples/RecursiveDo.hs"           "Main"
  , mkTestMod useRn "tests/examples/TypeFamilies.hs"          "Main"
  , mkTestMod useRn "tests/examples/MultiParamTypeClasses.hs" "Main"
  , mkTestMod useRn "tests/examples/DataFamilies.hs"          "DataFamilies"
  , mkTestMod useRn "tests/examples/Deriving.hs"              "Main"
  , mkTestMod useRn "tests/examples/Default.hs"               "Main"
  , mkTestMod useRn "tests/examples/ForeignDecl.hs"           "ForeignDecl"
  , mkTestMod useRn "tests/examples/Warning.hs"               "Warning"
  , mkTestMod useRn "tests/examples/Annotations.hs"           "Annotations"
  , mkTestMod useRn "tests/examples/DocDecls.hs"              "DocDecls"
  , mkTestModTH useRn "tests/examples/QuasiQuote.hs"          "QuasiQuote"
  , mkTestMod useRn "tests/examples/Roles.hs"                 "Roles"
  , mkTestMod useRn "tests/examples/Splice.hs"                "Splice"
  , mkTestMod useRn "tests/examples/ImportsSemi.hs"           "ImportsSemi"
  , mkTestMod useRn "tests/examples/Stmts.hs"                 "Stmts"
  , mkTestMod useRn "tests/examples/Mixed.hs"                 "Main"
  , mkTestMod useRn "tests/examples/Arrow.hs"                 "Arrow"
  , mkTestMod useRn "tests/examples/PatSynBind.hs"            "Main"
  , mkTestMod useRn "tests/examples/HsDo.hs"                  "HsDo"
  , mkTestMod useRn "tests/examples/ForAll.hs"                "ForAll"
  , mkTestMod useRn "tests/examples/PArr.hs"                  "PArr"
  , mkTestMod useRn "tests/examples/ViewPatterns.hs"          "Main"
  , mkTestMod useRn "tests/examples/BangPatterns.hs"          "Main"
  , mkTestMod useRn "tests/examples/Associated.hs"            "Main"
  , mkTestMod useRn "tests/examples/Move1.hs"                 "Move1"
  , mkTestMod useRn "tests/examples/Rules.hs"                 "Rules"
  , mkTestMod useRn "tests/examples/TypeOperators.hs"         "Main"
  , mkTestMod useRn "tests/examples/NullaryTypeClasses.hs"    "Main"
  , mkTestMod useRn "tests/examples/FunctionalDeps.hs"        "Main"
  , mkTestMod useRn "tests/examples/DerivingOC.hs"            "Main"
  , mkTestMod useRn "tests/examples/GenericDeriving.hs"       "Main"
  , mkTestMod useRn "tests/examples/OverloadedStrings.hs"     "Main"
  , mkTestMod useRn "tests/examples/RankNTypes.hs"            "Main"
  , mkTestMod useRn "tests/examples/Existential.hs"           "Main"
  , mkTestMod useRn "tests/examples/ScopedTypeVariables.hs"   "Main"
  , mkTestMod useRn "tests/examples/Arrows.hs"                "Main"
  , mkTestMod useRn "tests/examples/TH.hs"                    "Main"
  , mkTestMod useRn "tests/examples/StaticPointers.hs"        "Main"
  , mkTestMod useRn "tests/examples/DataDecl.hs"              "Main"
  , mkTestMod useRn "tests/examples/Guards.hs"                "Main"
  , mkTestMod useRn "tests/examples/RebindableSyntax.hs"      "Main"
  , mkTestMod useRn "tests/examples/RdrNames.hs"              "RdrNames"
  , mkTestMod useRn "tests/examples/Vect.hs"                  "Vect"
  , mkTestMod useRn "tests/examples/Tuple.hs"                 "Main"
  , mkTestMod useRn "tests/examples/ExtraConstraints1.hs"     "ExtraConstraints1"
  , mkTestMod useRn "tests/examples/AddAndOr3.hs"             "AddAndOr3"
  , mkTestMod useRn "tests/examples/Ann01.hs"                 "Ann01"
  , mkTestMod useRn "tests/examples/StrictLet.hs"             "Main"
  , mkTestMod useRn "tests/examples/Cg008.hs"                 "Cg008"
  , mkTestMod useRn "tests/examples/T2388.hs"                 "T2388"
  , mkTestMod useRn "tests/examples/T3132.hs"                 "T3132"
  , mkTestMod useRn "tests/examples/Stream.hs"                "Stream"
  , mkTestMod useRn "tests/examples/Trit.hs"                  "Trit"
  , mkTestMod useRn "tests/examples/DataDecl.hs"              "Main"
  , mkTestMod useRn "tests/examples/Zipper.hs"                "Zipper"
  , mkTestMod useRn "tests/examples/Sigs.hs"                  "Sigs"
  , mkTestMod useRn "tests/examples/Utils2.hs"                "Utils2"
  , mkTestMod useRn "tests/examples/EmptyMostlyInst.hs"       "EmptyMostlyInst"
  , mkTestMod useRn "tests/examples/EmptyMostlyNoSemis.hs"    "EmptyMostlyNoSemis"
  , mkTestMod useRn "tests/examples/Dead1.hs"                 "Dead1"
  , mkTestMod useRn "tests/examples/EmptyMostly.hs"           "EmptyMostly"
  , mkTestMod useRn "tests/examples/FromUtils.hs"             "Main"
  , mkTestMod useRn "tests/examples/DocDecls.hs"              "DocDecls"
  , mkTestMod useRn "tests/examples/RecordUpdate.hs"          "Main"
  -- , mkTestMod useRn "tests/examples/Unicode.hs"               "Main"
  , mkTestMod useRn "tests/examples/B.hs"                     "Main"
  , mkTestMod useRn "tests/examples/LayoutWhere.hs"           "Main"
  , mkTestMod useRn "tests/examples/LayoutLet.hs"             "Main"
  , mkTestMod useRn "tests/examples/Deprecation.hs"           "Deprecation"
  , mkTestMod useRn "tests/examples/Infix.hs"                 "Main"
  , mkTestMod useRn "tests/examples/BCase.hs"                 "Main"
  , mkTestMod useRn "tests/examples/AltsSemis.hs"             "Main"

  , mkTestMod useRn "tests/examples/LetExprSemi.hs"           "LetExprSemi"
  ]

mkTestMain :: FilePath -> Test
mkTestMain fileName = TestCase (do r <- manipulateAstTest False fileName "Main"
                                   assertBool fileName r )

mkTestMod :: Bool -> FilePath -> String -> Test
mkTestMod useRn fileName modName
  = TestCase (do r <- manipulateAstTest useRn fileName modName
                 assertBool fileName r )

mkTestModTH :: Bool -> FilePath -> String -> Test
mkTestModTH useRn fileName modName
  = TestCase (do r <- manipulateAstTestTH useRn fileName modName
                 assertBool fileName r )

-- ---------------------------------------------------------------------

tt :: IO Bool
tt = do
{-
    manipulateAstTest False "tests/examples/LetStmt.hs"               "Layout.LetStmt"
    manipulateAstTest False "tests/examples/LetExpr.hs"               "LetExpr"
    manipulateAstTest False "tests/examples/ExprPragmas.hs"           "ExprPragmas"
    manipulateAstTest False "tests/examples/ListComprehensions.hs"    "Main"
    manipulateAstTest False "tests/examples/MonadComprehensions.hs"   "Main"
    manipulateAstTest False "tests/examples/FunDeps.hs"               "Main"
    manipulateAstTest False "tests/examples/ImplicitParams.hs"        "Main"
    manipulateAstTest False "tests/examples/RecursiveDo.hs"           "Main"
    manipulateAstTest False "tests/examples/TypeFamilies.hs"          "Main"
    manipulateAstTest False "tests/examples/MultiParamTypeClasses.hs" "Main"
    manipulateAstTest False "tests/examples/DataFamilies.hs"          "DataFamilies"
    manipulateAstTest False "tests/examples/Deriving.hs"              "Main"
    manipulateAstTest False "tests/examples/Default.hs"               "Main"
    manipulateAstTest False "tests/examples/ForeignDecl.hs"           "ForeignDecl"
    manipulateAstTest False "tests/examples/Warning.hs"               "Warning"
    manipulateAstTest False "tests/examples/Annotations.hs"           "Annotations"
    manipulateAstTestTH False "tests/examples/QuasiQuote.hs"          "QuasiQuote"
    manipulateAstTest False "tests/examples/Roles.hs"                 "Roles"
    manipulateAstTest False "tests/examples/Splice.hs"                "Splice"
    manipulateAstTest False "tests/examples/ImportsSemi.hs"           "ImportsSemi"
    manipulateAstTest False "tests/examples/Stmts.hs"                 "Stmts"
    manipulateAstTest False "tests/examples/Mixed.hs"                 "Main"
    manipulateAstTest False "tests/examples/PatSynBind.hs"            "Main"
    manipulateAstTest False "tests/examples/HsDo.hs"                  "HsDo"
    manipulateAstTest False "tests/examples/ForAll.hs"                "ForAll"
    manipulateAstTest False "tests/examples/PArr.hs"                  "PArr"
    manipulateAstTest False "tests/examples/ViewPatterns.hs"          "Main"
    manipulateAstTest False "tests/examples/BangPatterns.hs"          "Main"
    manipulateAstTest False "tests/examples/Associated.hs"            "Main"
    manipulateAstTest False "tests/examples/Move1.hs"                 "Move1"
    manipulateAstTest False "tests/examples/Rules.hs"                 "Rules"
    manipulateAstTest False "tests/examples/TypeOperators.hs"         "Main"
    manipulateAstTest False "tests/examples/NullaryTypeClasses.hs"    "Main"
    manipulateAstTest False "tests/examples/FunctionalDeps.hs"        "Main"
    manipulateAstTest False "tests/examples/DerivingOC.hs"            "Main"
    manipulateAstTest False "tests/examples/GenericDeriving.hs"       "Main"
    manipulateAstTest False "tests/examples/OverloadedStrings.hs"     "Main"
    manipulateAstTest False "tests/examples/RankNTypes.hs"            "Main"
    manipulateAstTest False "tests/examples/Existential.hs"           "Main"
    manipulateAstTest False "tests/examples/ScopedTypeVariables.hs"   "Main"
    manipulateAstTest False "tests/examples/TH.hs"                    "Main"
    manipulateAstTest False "tests/examples/StaticPointers.hs"        "Main"
    manipulateAstTest False "tests/examples/DataDecl.hs"              "Main"
    manipulateAstTest False "tests/examples/Guards.hs"                "Main"
    manipulateAstTest False "tests/examples/RebindableSyntax.hs"      "Main"
    manipulateAstTest False "tests/examples/RdrNames.hs"              "RdrNames"
    manipulateAstTest False "tests/examples/Vect.hs"                  "Vect"
    manipulateAstTest False "tests/examples/Tuple.hs"                 "Main"
    manipulateAstTest False "tests/examples/ExtraConstraints1.hs"     "ExtraConstraints1"
    manipulateAstTest False "tests/examples/AddAndOr3.hs"             "AddAndOr3"
    manipulateAstTest False "tests/examples/Ann01.hs"                 "Ann01"
    manipulateAstTest False "tests/examples/StrictLet.hs"             "Main"
    manipulateAstTest False "tests/examples/Cg008.hs"                 "Cg008"
    manipulateAstTest False "tests/examples/T2388.hs"                 "T2388"
    manipulateAstTest False "tests/examples/T3132.hs"                 "T3132"
    manipulateAstTest False "tests/examples/Stream.hs"                "Stream"
    manipulateAstTest False "tests/examples/Trit.hs"                  "Trit"
    manipulateAstTest False "tests/examples/DataDecl.hs"              "Main"
    manipulateAstTest False "tests/examples/Zipper.hs"                "Zipper"
    manipulateAstTest False "tests/examples/Sigs.hs"                  "Sigs"
    manipulateAstTest False "tests/examples/Utils2.hs"                "Utils2"
    manipulateAstTest False "tests/examples/EmptyMostlyInst.hs"       "EmptyMostlyInst"
    manipulateAstTest False "tests/examples/EmptyMostlyNoSemis.hs"    "EmptyMostlyNoSemis"
    manipulateAstTest False "tests/examples/Dead1.hs"                 "Dead1"
    manipulateAstTest False "tests/examples/EmptyMostly.hs"           "EmptyMostly"
    manipulateAstTest False "tests/examples/FromUtils.hs"             "Main"
    manipulateAstTest False "tests/examples/DocDecls.hs"              "DocDecls"
    manipulateAstTest False "tests/examples/RecordUpdate.hs"          "Main"
    -- manipulateAstTest False "tests/examples/Unicode.hs"               "Main"
    manipulateAstTest False "tests/examples/B.hs"                     "Main"
    manipulateAstTest False "tests/examples/LayoutWhere.hs"           "Main"
    manipulateAstTest False "tests/examples/Infix.hs"                 "Main"
    manipulateAstTest False "tests/examples/BCase.hs"                 "Main"
    manipulateAstTest False "tests/examples/AltsSemis.hs"             "Main"
    manipulateAstTest False "tests/examples/LetExprSemi.hs"           "LetExprSemi"
    manipulateAstTest False "tests/examples/LayoutLet.hs"             "Main"
    manipulateAstTest False "tests/examples/DocDecls.hs"              "DocDecls"
    manipulateAstTest False "tests/examples/Deprecation.hs"           "Deprecation"
    manipulateAstTest False "tests/examples/BCase.hs"                 "Main"
    manipulateAstTest False "tests/examples/Arrows.hs"                "Main"
-}
    manipulateAstTest True "tests/examples/Arrow.hs"                 "Arrow"

{-
    manipulateAstTest False "tests/examples/Cpp.hs"                   "Main"
    manipulateAstTest False "tests/examples/Lhs.lhs"                  "Main"
    manipulateAstTest False "tests/examples/ParensAroundContext.hs"   "ParensAroundContext"
    manipulateAstTest False "tests/examples/EmptyMostly2.hs"          "EmptyMostly2"
    manipulateAstTest False "tests/examples/Foo.hs"                   "Main"
-}

-- | Where all the tests are to be found
examplesDir :: FilePath
examplesDir = "tests" </> "examples"

examplesDir2 :: FilePath
examplesDir2 = "examples"

manipulateAstTest :: Bool -> FilePath -> String -> IO Bool
manipulateAstTest useRenamed file modname = manipulateAstTest' False useRenamed file modname

manipulateAstTestTH :: Bool -> FilePath -> String -> IO Bool
manipulateAstTestTH useRenamed file modname = manipulateAstTest' True useRenamed file modname

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
  putStrLn $ "Test:showdata:parsed" ++ showAnnData (organiseAnns ann) 0 parsed
  putStrLn $ "\n===============================================================\n"
  putStrLn $ "Test:showdata:renamed" ++ showAnnData (organiseAnns ann) 0 renamed
  putStrLn $ "\n===============================================================\n"
  -- putStrLn $ "Test:ann:" ++ showGhc ann

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

