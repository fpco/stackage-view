{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

-- | Dealing with Cabal.

module Stackage.View.Cabal (getTargets,getCabalFp) where

import           Stackage.View.Defaults
import           Stackage.View.Types

import           Control.Exception
import           Control.Monad
import           Control.Monad.Loops
import           Data.Char (isLetter, isDigit)
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Distribution.Compiler
import           Distribution.ModuleName as Cabal
import qualified Distribution.ModuleName as ModuleName (fromString)
import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import           Distribution.System
import           Distribution.Text (display)
import           Distribution.Version
import           Filesystem
import           Filesystem.Loc as FL
import qualified Filesystem.Path.CurrentOS as FP
import           Filesystem.Path.Find
import           Language.Haskell.Extension
import           Prelude hiding (FilePath)
import           SharedTypes (TargetIdent(..))

-- | Get the .cabal filename from the given package directory.
getCabalFp :: Loc Absolute Dir -> IO (Loc Absolute File)
getCabalFp pkgDir =
  do mcabal <- findFileUp pkgDir
                          (flip FP.hasExtension "cabal" . FL.toFilePath)
                          (Just pkgDir)
     case mcabal of
       Nothing -> throwIO (FPNoCabalFile (FL.toFilePath pkgDir))
       Just cabalfp -> return cabalfp

-- | Default src dirs for Cabal targets.
defaultSrcDirs :: [String]
defaultSrcDirs = ["","dist/build/autogen"]

-- | Get targets of a package.
getTargets :: Map Text Bool -> Loc Absolute File -> IO [Target]
getTargets flags cabalfp =
  do chars <- Prelude.readFile (FL.encodeString cabalfp)
     case parsePackageDescription chars of
       ParseFailed per ->
         throwIO (FPInvalidCabalFile cabalfp per)
       ParseOk _ (resolvePackage flags -> pkg) ->
         do let dir = FL.parentOfFile cabalfp
                pname = pkgName (package pkg)
            lib <- maybe (return Nothing)
                         (\e -> (fmap (Just . (e,)) .
                                 libraryFiles pname dir) e)
                         (library pkg)
            exes <- mapM (\e ->
                            fmap (e,)
                                 (executableFiles pname dir e (library pkg)))
                         (executables pkg)
            benches <- mapM (\e ->
                               fmap (e,)
                                    (benchmarkFiles pname dir e (library pkg)))
                            (benchmarks pkg)
            tests <- mapM (\e ->
                             fmap (e,)
                                  (testFiles pname dir e (library pkg)))
                          (testSuites pkg)
            return (concat [[Target LibraryIdent files exts (deps (libBuildInfo lib))
                            | Just (lib,(exts,files)) <- [lib]]
                           ,[Target (ExecutableIdent (T.pack (exeName e))) files exts (deps (buildInfo e))
                            | (e,(exts,files)) <- exes]
                           ,[Target (BenchmarkIdent (T.pack (benchmarkName e))) files exts (deps (benchmarkBuildInfo e))
                            | (e,(exts,files)) <- benches]
                           ,[Target (TestSuiteIdent (T.pack (testName e))) files exts (deps (testBuildInfo e))
                            | (e,(exts,files)) <- tests]])
         where deps = map dependencyName . targetBuildDepends
               dependencyName (Dependency name _) = name

-- | Get all files referenced by the benchmark.
benchmarkFiles :: PackageName
               -> Loc Absolute Dir
               -> Benchmark
               -> Maybe Library
               -> IO ([Extension],Set (Loc Relative File))
benchmarkFiles pname dir exe mlib =
  do exposed <- resolveFiles
                  (map (either (error . show) id .
                        FL.parseRelativeDirLoc . FP.decodeString)
                       (defaultSrcDirs ++ hsSourceDirs build))
                  [Right (case benchmarkInterface exe of
                            BenchmarkExeV10 _ fp -> fp
                            ty ->
                              error ("Unsupported benchmark type: " ++ show ty))
                  ,paths_Name pname]
                  haskellFileExts
     (exts,bfiles) <- buildFiles pname dir mlib build
     return (exts,mconcat [bfiles,exposed])
  where build = benchmarkBuildInfo exe

-- | Get all files referenced by the test.
testFiles :: PackageName
          -> Loc Absolute Dir
          -> TestSuite
          -> Maybe Library
          -> IO ([Extension],Set (Loc Relative File))
testFiles pname dir exe mlib =
  do exposed <- resolveFiles
                  (map (either (error . show) id .
                        FL.parseRelativeDirLoc . FP.decodeString)
                       (defaultSrcDirs ++ hsSourceDirs build))
                  [case testInterface exe of
                     TestSuiteExeV10 _ fp -> Right fp
                     TestSuiteLibV09 _ mn -> Left mn
                     ty ->
                       error ("Unsupported test type: " ++ show ty)
                  ,paths_Name pname]
                  haskellFileExts
     (es,bfiles) <- buildFiles pname dir mlib build
     return (es,mconcat [bfiles,exposed])
  where build = testBuildInfo exe

-- | Get all files referenced by the executable.
executableFiles :: PackageName
                -> Loc Absolute Dir
                -> Executable
                -> Maybe Library
                -> IO ([Extension],Set (Loc Relative File))
executableFiles pname dir exe mlib =
  do exposed <- resolveFiles
                  (map (either (error . show)
                               id .
                        FL.parseRelativeDirLoc . FP.decodeString)
                       (defaultSrcDirs ++ hsSourceDirs build))
                  [Right (modulePath exe)
                  ,paths_Name pname]
                  haskellFileExts
     (es,bfiles) <- buildFiles pname dir mlib build
     return (es,mconcat [bfiles,exposed])
  where build = buildInfo exe

-- | Get all files referenced by the library.
libraryFiles :: PackageName
             -> Loc Absolute Dir
             -> Library
             -> IO ([Extension],Set (Loc Relative File))
libraryFiles pname dir lib =
  do exposed <- resolveFiles
                  (map (either (error . show) id .
                        FL.parseRelativeDirLoc . FP.decodeString)
                       (defaultSrcDirs ++ hsSourceDirs build))
                  (map Left (exposedModules lib) ++
                   [paths_Name pname])
                  haskellFileExts
     (es,bfiles) <- buildFiles pname dir Nothing build
     return (es,mconcat [bfiles,exposed])
  where build = libBuildInfo lib

-- | Get all files in a build. If any of this target's ('BuildInfo')
-- dependencies are the same as the package name, then this means it's
-- a target which depends on the library component, in which case the
-- necessary files and extensions should be drawn from the library,
-- too.
buildFiles :: PackageName
           -> Loc Absolute Dir
           -> Maybe Library
           -> BuildInfo
           -> IO ([Extension],Set (Loc Relative File))
buildFiles pname dir mlib build =
  do other <- resolveFiles
                (map (either (error . show) id .
                      FL.parseRelativeDirLoc . FP.decodeString)
                     (defaultSrcDirs ++ hsSourceDirs build))
                (map Left (otherModules build) ++
                 [paths_Name pname])
                haskellFileExts
     let (exts,files) =
           (defaultExtensions build ++ oldExtensions build ++
                                       otherExtensions build
           ,mconcat [other
                    ,S.fromList
                       (map (either (error . show) id .
                             FL.parseRelativeFileLoc . FP.decodeString)
                            (cSources build))])
     case mlib of
       Just lib
         | elem pname (map dependencyName (targetBuildDepends build)) ->
           do (libExts,libFiles) <- libraryFiles pname dir lib
              return (exts ++ libExts,mappend files libFiles)
       _ -> return (exts,files)
  where dependencyName (Dependency name _) = name

-- | Get all dependencies of a package, including library,
-- executables, tests, benchmarks.
resolvePackage  :: Map Text Bool -> GenericPackageDescription -> PackageDescription
resolvePackage passedFlags (GenericPackageDescription desc defaultFlags mlib exes tests benches) =
  desc {library =
          fmap (resolveConditions flags' updateLibDeps . ("",)) mlib
       ,executables = map (resolveConditions flags' updateExeDeps) exes
       ,testSuites = map (resolveConditions flags' updateTestDeps) tests
       ,benchmarks = map (resolveConditions flags' updateBenchmarkDeps) benches}
  where flags = M.union passedFlags (flagMap defaultFlags)
        flags' =
          map (FlagName . T.unpack)
              (map fst (filter snd
                               (M.toList flags)))
        updateLibDeps lib _ deps =
          lib {libBuildInfo =
                 (libBuildInfo lib) {targetBuildDepends = deps}}
        updateExeDeps exe name deps =
          exe {buildInfo =
                 (buildInfo exe) {targetBuildDepends = deps}
              ,exeName = name}
        updateTestDeps test name deps =
          test {testBuildInfo =
                  (testBuildInfo test) {targetBuildDepends = deps}
               ,testName = name}
        updateBenchmarkDeps benchmark name deps =
          benchmark {benchmarkBuildInfo =
                       (benchmarkBuildInfo benchmark) {targetBuildDepends = deps}
                    ,benchmarkName = name}

-- | Make a map from a list of flag specifications.
--
-- What is @flagManual@ for?
flagMap :: [Flag] -> Map Text Bool
flagMap = M.fromList . map pair
  where pair :: Flag -> (Text, Bool)
        pair (MkFlag (unName -> name) _desc def _manual) = (name,def)
        unName (FlagName t) = T.pack t

-- | Resolve the condition tree for the library.
resolveConditions :: (Monoid target,HasName target)
                  => [FlagName]
                  -> (target -> String -> cs -> target)
                  -> (String,CondTree ConfVar cs target)
                  -> target
resolveConditions flags addDeps (name,CondNode lib deps cs) =
  appendTargets basic children
  where basic = addDeps lib name deps
        children =
          foldr appendTargets mempty (map apply cs)
          where apply (cond,node,mcs) =
                  if condSatisfied cond
                     then appendTargets
                            (resolveConditions flags
                                               addDeps
                                               (name,node))
                            (maybe mempty
                                   (resolveConditions flags addDeps .
                                    (name,))
                                   mcs)
                     else mempty
                condSatisfied c =
                  case c of
                    Var v -> varSatisifed v
                    Lit b -> b
                    CNot c' ->
                      not (condSatisfied c')
                    COr cx cy ->
                      or [condSatisfied cx,condSatisfied cy]
                    CAnd cx cy ->
                      and [condSatisfied cx,condSatisfied cy]
                varSatisifed v =
                  case v of
                    OS os -> os == buildOS
                    Arch arch -> arch == buildArch
                    Flag flag -> elem flag flags
                    Impl flavor range ->
                      case buildCompilerId of
                        CompilerId flavor' ver ->
                          flavor' == flavor &&
                          withinRange ver range

-- | Safely append two targets without throwing an exception.
-- See here for explanation: https://github.com/haskell/cabal/blob/81330d032a174e8406bcd40e9c5c8c8cbdd6853b/Cabal/Distribution/PackageDescription.hs#L566..L570
appendTargets :: (HasName m,Monoid m) => m -> m -> m
appendTargets x y =
  if not (null (getName x)) &&
     not (null (getName y))
     then fst (modifyName (const "") x) <> y
     else x <> y

-- | Try to resolve the list of base names in the given directory by
-- looking for unique instances of base names applied with the given
-- extensions.
resolveFiles :: [Loc Relative Dir] -- ^ Directories to look in.
             -> [Either ModuleName String] -- ^ Base names.
             -> [Text] -- ^ Extentions.
             -> IO (Set (Loc Relative File))
resolveFiles dirs names exts =
  fmap (S.fromList . catMaybes) (forM names makeNameCandidates)
  where makeNameCandidates name =
          firstM (isFile . FL.toFilePath)
                 (concatMap (makeDirCandidates name) dirs)
        makeDirCandidates :: Either ModuleName String
                          -> Loc Relative Dir
                          -> [Loc Relative File]
        makeDirCandidates name dir =
          map (\ext ->
                 case name of
                   Left mn ->
                     (either (error . show)
                             (FL.appendLoc dir)
                             (FL.parseRelativeFileLoc
                                (FP.addExtension (FP.decodeString (Cabal.toFilePath mn))
                                                 ext)))
                   Right fp ->
                     either (error . show)
                            (FL.appendLoc dir)
                            (FL.parseRelativeFileLoc (FP.decodeString fp)))
              exts

-- | Get the name.
getName :: HasName m => m -> String
getName = snd . modifyName id

-- | Some target that has a name.
class HasName m where
  modifyName :: (String -> String) -> m -> (m,String)

instance HasName Library where
    modifyName f m = (m,f "")
instance HasName Executable where
    modifyName f (Executable name m b) =
        (Executable (f name) m b,f name)
instance HasName TestSuite where
    modifyName f (TestSuite name i b e) =
        (TestSuite (f name) i b e, f name)
instance HasName Benchmark where
    modifyName f (Benchmark name i b e) =
        (Benchmark (f name) i b e, f name)

-- | Make a file resolution entry for the special Paths_projectname module.
paths_Name :: PackageName -> Either ModuleName a
paths_Name pname = Left (ModuleName.fromString ("Paths_" <> map normalize (display pname)))
  where normalize c | isLetter c || isDigit c = c
                    | otherwise = '_'
