import System.Environment

padLeft :: Int -> a -> [a] -> [a]
padLeft n x xs = replicate (n - length xs) x ++ xs

cabalEntry :: Integer -> String
cabalEntry x =
  unlines
    [ "",
      "executable day" ++ padLeft 2 '0' (show x),
      "  main-is:             day" ++ padLeft 2 '0' (show x) ++ ".hs",
      "  -- other-modules:",
      "  -- other-extensions:",
      "  build-depends:       base >=4.13, split, containers",
      "  hs-source-dirs:      src/",
      "  default-language:    Haskell2010"
    ]

sourceCode :: Integer -> String
sourceCode _ =
  unlines
    ["main = interact $ id"]

fileName :: Integer -> String -> String
fileName n ext = "day" ++ padLeft 2 '0' (show n) ++ "." ++ ext

createInputFile :: Integer -> IO ()
createInputFile n = writeFile ("./inputs/" ++ fileName n "txt") ""

createSourceFile :: Integer -> IO ()
createSourceFile n = writeFile ("./src/" ++ fileName n "hs") $ sourceCode n

createCabalEntry :: Integer -> IO ()
createCabalEntry n = appendFile "advent2021.cabal" $ cabalEntry n

main = do
  (n : _) <- getArgs
  let num = read n
  createInputFile num
  createSourceFile num
  createCabalEntry num