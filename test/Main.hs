module Main where

import Language.Haskell.Packages

import Data.Char
import Data.List
import Data.Ord

import Data.Time.Clock

import Distribution.InstalledPackageInfo
import Distribution.Text (display)

import Test.Tasty
import Test.Tasty.HUnit

import System.Directory
import System.FilePath
import System.Process


main :: IO()
main = defaultMain tests

tests :: TestTree
tests = testGroup "ghc-pkg-lib Tests"
    [ testCase "No Sandbox" $ do
        t1 <- getCurrentTime
        pkgs <- getPkgInfos Nothing
        length pkgs @?= 2
        t2 <- getCurrentTime
        putStr "getPkgInfos:"
        print (diffUTCTime t2 t1)
        ls <- execList "ghc-pkg" ["list"]
        t3 <- getCurrentTime
        putStr "ghc-pkg:"
        print (diffUTCTime t3 t2)
        let names = extractNames pkgs
        ls @=? names
    , testCase "My Sandbox" $ do
        d <- getCurrentDirectory
        let cfg = d </> "cabal.sandbox.config"
        cfgContents <- readFile cfg
        let cfgPath = head $ map (drop (length "prefix: ")) $ filter (isPrefixOf "prefix: ") $ map (dropWhile isSpace) $ lines cfgContents
        t1 <- getCurrentTime
        pkgs <- getPkgInfos (Just cfgPath)
        length pkgs @?= 2
        t2 <- getCurrentTime
        putStr "getPkgInfos:"
        print (diffUTCTime t2 t1)
        ls <- execList "cabal" ["sandbox","hc-pkg","list"]
        t3 <- getCurrentTime
        putStr "cabal:"
        print (diffUTCTime t3 t2)
        let names = extractNames pkgs
        ls @=? names
    ]

extractNames :: [(FilePath, [InstalledPackageInfo_ m])] -> [(FilePath, [String])]
extractNames = sortList . map (\(a,b)->(a,map (display . sourcePackageId) b))

sortList :: [(FilePath,[String])] -> [(FilePath,[String])]
sortList = map (\(a,b)->(a,sort b)) . sortBy (comparing fst)

execList :: String -> [String] -> IO [(FilePath,[String])]
execList cmd args = do
    output<-readProcess cmd args "" -- this is strict
    let ls = lines output
        ps = foldl' parse [] ls
    return $ sortList ps
    where
        parse :: [(FilePath,[String])] -> String -> [(FilePath,[String])]
        parse res "" = res -- empty line
        parse [] (' ':_)  = [] -- no current db
        parse ((fp,ls):xs) (' ':r) = let -- a db and a package name (indented)
            trimmed = dropWhile isSpace r
            in case head trimmed of
                '(' -> ((fp,(init $ tail trimmed):ls):xs) -- hidden package, remove brackets
                '{' -> ((fp,(init $ tail trimmed):ls):xs) -- broken package, remove brackets
                _   -> ((fp,trimmed:ls):xs) -- normal
        parse ls r = (init r,[]):ls -- db path, remove trailing colon
