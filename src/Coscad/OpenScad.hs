-- | Everything that talks to the OpenSCAD binary: locating it and the
-- BOSL2 library, running it with the right environment, and turning a
-- .scad file into a parsed mesh with warnings treated as errors.
module Coscad.OpenScad
  ( findOpenscad
  , findBosl2
  , libraryDirs
  , openscadEnv
  , openscadStlArgs
  , runOpenscadRaw
  , runOpenscad
  , renderStlFile
  , openscadVersion
  , bosl2Version
  ) where

import Control.Exception (SomeException, try)
import Coscad.Mesh
import Data.Char (isSpace)
import Data.List (intercalate, isInfixOf, isPrefixOf)
import System.Directory (doesFileExist, findExecutable, getHomeDirectory)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (dropTrailingPathSeparator, takeDirectory, (</>))
import System.Info (os)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)

-- | The OpenSCAD binary: $COSCAD_OPENSCAD (may be a wrapper script,
-- e.g. xvfb-run on a headless box), then `openscad` on PATH, then the
-- usual install locations.
findOpenscad :: IO (Maybe FilePath)
findOpenscad = do
  env <- lookupEnv "COSCAD_OPENSCAD"
  case env of
    Just b | not (null b) -> return (Just b)
    _ -> do
      onPath <- findExecutable "openscad"
      case onPath of
        Just b -> return (Just b)
        Nothing -> firstFile candidates
  where
    candidates =
      [ "/Applications/OpenSCAD.app/Contents/MacOS/openscad"
      , "C:\\Program Files\\OpenSCAD\\openscad.exe"
      , "C:\\Program Files (x86)\\OpenSCAD\\openscad.exe"
      , "/usr/bin/openscad"
      , "/usr/local/bin/openscad"
      , "/snap/bin/openscad"
      ]

firstFile :: [FilePath] -> IO (Maybe FilePath)
firstFile [] = return Nothing
firstFile (c : cs) = do
  e <- doesFileExist c
  if e then return (Just c) else firstFile cs

pathSep :: Char
pathSep = if os == "mingw32" then ';' else ':'

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, []) -> [a]
  (a, _ : r) -> a : splitOn c r

-- | Directories OpenSCAD searches for `include <...>`: OPENSCADPATH
-- entries, then the per-platform user and system library folders.
libraryDirs :: IO [FilePath]
libraryDirs = do
  home <- getHomeDirectory
  osp <- maybe [] (splitOn pathSep) <$> lookupEnv "OPENSCADPATH"
  let user = case os of
        "darwin" -> [home </> "Documents" </> "OpenSCAD" </> "libraries"]
        "mingw32" -> [home </> "Documents" </> "OpenSCAD" </> "libraries", home </> "My Documents" </> "OpenSCAD" </> "libraries"]
        _ -> [home </> ".local" </> "share" </> "OpenSCAD" </> "libraries"]
      sys = case os of
        "darwin" -> ["/Applications/OpenSCAD.app/Contents/Resources/libraries"]
        "mingw32" -> ["C:\\Program Files\\OpenSCAD\\libraries"]
        _ -> ["/usr/share/openscad/libraries", "/usr/local/share/openscad/libraries"]
  return (filter (not . null) osp ++ user ++ sys)

-- | The BOSL2 checkout OpenSCAD will find: $COSCAD_BOSL2 (the BOSL2
-- directory itself), else the first library dir holding BOSL2/std.scad.
findBosl2 :: IO (Maybe FilePath)
findBosl2 = do
  env <- lookupEnv "COSCAD_BOSL2"
  case env of
    Just d | not (null d) -> return (Just d)
    _ -> do
      dirs <- libraryDirs
      go [d </> "BOSL2" | d <- dirs]
  where
    go [] = return Nothing
    go (d : ds) = do
      e <- doesFileExist (d </> "std.scad")
      if e then return (Just d) else go ds

-- | Process environment for OpenSCAD. When $COSCAD_BOSL2 names a
-- checkout outside the library folders, its parent is prepended to
-- OPENSCADPATH so the emitted `include <BOSL2/std.scad>` resolves.
openscadEnv :: IO [(String, String)]
openscadEnv = do
  env <- getEnvironment
  b <- lookupEnv "COSCAD_BOSL2"
  case b of
    Just d | not (null d) ->
      let parent = takeDirectory (dropTrailingPathSeparator d)
          old = maybe "" id (lookup "OPENSCADPATH" env)
          new = if null old then parent else parent ++ [pathSep] ++ old
       in return (("OPENSCADPATH", new) : filter ((/= "OPENSCADPATH") . fst) env)
    _ -> return env

-- | Arguments for a headless STL export. ASCII is requested explicitly:
-- the mesh reader is ASCII-only and OpenSCAD's default format depends
-- on version and preferences.
openscadStlArgs :: FilePath -> FilePath -> [String]
openscadStlArgs stlF scadF = ["-o", stlF, "--export-format", "asciistl", scadF]

-- | Run OpenSCAD with the given arguments (Left only if it cannot be run at all).
runOpenscadRaw :: [String] -> IO (Either String (ExitCode, String, String))
runOpenscadRaw args = do
  found <- findOpenscad
  case found of
    Nothing -> return (Left "OpenSCAD not found: install it, put `openscad` on PATH, or set COSCAD_OPENSCAD (see `coscad doctor`)")
    Just bin -> do
      env <- openscadEnv
      r <- try (readCreateProcessWithExitCode (proc bin args) {env = Just env} "")
      return $ case r of
        Left e -> Left ("could not run " ++ bin ++ ": " ++ show (e :: SomeException))
        Right x -> Right x

-- | .scad -> .stl, exit status only.
runOpenscad :: FilePath -> FilePath -> IO (Either String ())
runOpenscad scadF stlF = do
  r <- runOpenscadRaw (openscadStlArgs stlF scadF)
  return $ case r of
    Left e -> Left e
    Right (ExitSuccess, _, _) -> Right ()
    Right (ExitFailure n, _, err) -> Left ("openscad failed (" ++ show n ++ ") on " ++ scadF ++ ":\n" ++ err)

-- | .scad -> parsed mesh. OpenSCAD WARNING/ERROR lines are failures
-- here: a missing include or a dropped child means the mesh is not
-- what the source asked for, even though OpenSCAD exits 0.
renderStlFile :: FilePath -> FilePath -> IO (Either String [Tri])
renderStlFile scadF stlF = do
  r <- runOpenscadRaw (openscadStlArgs stlF scadF)
  case r of
    Left e -> return (Left e)
    Right (code, out, err) -> do
      let noise = [l | l <- lines out ++ lines err, any (`isPrefixOf` l) ["WARNING", "ERROR"]]
      case code of
        ExitFailure n -> return (Left ("openscad exit " ++ show n ++ " on " ++ scadF ++ ":\n" ++ unlines (take 8 (noise ++ lines err))))
        ExitSuccess
          | not (null noise) -> return (Left ("openscad reported problems rendering " ++ scadF ++ ":\n" ++ unlines noise))
          | otherwise -> do
              ok <- doesFileExist stlF
              if not ok
                then return (Left ("openscad produced no STL for " ++ scadF))
                else do
                  s <- readFile stlF
                  let tris = parseStlAscii s
                  length tris `seq` return (Right tris)

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

-- | "2025.05.16" etc., from `openscad --version`.
openscadVersion :: IO (Maybe String)
openscadVersion = do
  r <- runOpenscadRaw ["--version"]
  return $ case r of
    Right (_, out, err) ->
      case [drop (length pfx) l | l <- lines (out ++ err), pfx `isPrefixOf` l] of
        (v : _) -> Just (trim v)
        _ -> Nothing
    Left _ -> Nothing
  where
    pfx = "OpenSCAD version "

-- | BOSL2 version as OpenSCAD itself resolves the include (a probe
-- file is written into the given directory). Left carries OpenSCAD's
-- own complaint when the include cannot be found.
bosl2Version :: FilePath -> IO (Either String String)
bosl2Version dir = do
  let scadF = dir </> "bosl2_probe.scad"
      outF = dir </> "bosl2_probe.echo"
  writeFile scadF "include <BOSL2/std.scad>\necho(coscad_bosl2 = BOSL_VERSION);\n"
  r <- runOpenscadRaw ["-o", outF, "--export-format", "echo", scadF]
  case r of
    Left e -> return (Left e)
    Right (code, out, err) -> do
      exists <- doesFileExist outF
      echo <- if exists then readFile outF else return ""
      let ls = lines (echo ++ "\n" ++ out ++ "\n" ++ err)
          found = [l | l <- ls, "coscad_bosl2" `isInfixOf` l, '[' `elem` l]
          missing = [l | l <- ls, "Can't open" `isInfixOf` l || "WARNING" `isPrefixOf` l]
      return $ case (found, missing) of
        (l : _, _) -> Right (versionOf l)
        (_, m : _) -> Left (trim m)
        _ -> Left ("BOSL2 did not answer the version probe (openscad exit " ++ show code ++ ")")
  where
    versionOf l =
      let inside = takeWhile (/= ']') (drop 1 (dropWhile (/= '[') l))
       in intercalate "." (map trim (splitOn ',' inside))
