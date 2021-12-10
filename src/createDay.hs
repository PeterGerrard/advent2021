import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Environment

padLeft :: Int -> a -> [a] -> [a]
padLeft n x xs = replicate (n - length xs) x ++ xs

cabalEntry :: String -> String
cabalEntry fileName =
  unlines
    [ "",
      "library " ++ fileName,
      "  exposed-modules:     " ++ fileName,
      "  build-depends:       base >=4.13, split, containers",
      "  hs-source-dirs:      src/",
      "  default-language:    Haskell2010"
    ]

sourceCode :: String -> String
sourceCode modName =
  unlines
    [ "module " ++ modName ++ " where",
      "",
      "parse :: String -> String",
      "parse = id",
      "",
      "print :: String -> String",
      "print = id",
      "",
      "solve1 :: String -> String",
      "solve1 = id",
      "",
      "solve2 :: String -> String",
      "solve2 = id"
    ]

createInputFile :: String -> IO ()
createInputFile fileName = writeFile ("./inputs/" ++ fileName ++ ".txt") ""

createSourceFile :: String -> IO ()
createSourceFile fileName = writeFile ("./src/" ++ fileName ++ ".hs") $ sourceCode fileName

updateMain :: Integer -> String -> Text -> Text
updateMain num name =
  Text.replace (Text.pack " -- solve1Insert") (Text.pack $ "\n  " ++ show num ++ " -> " ++ name ++ ".parse  . " ++ name ++ ".solve1 . " ++ name ++ ".parse --solve1Insert")
    . Text.replace (Text.pack " -- solve2Insert") (Text.pack $ "\n  " ++ show num ++ " -> " ++ name ++ ".parse  . " ++ name ++ ".solve2 . " ++ name ++ ".parse --solve2Insert")
    . Text.replace (Text.pack " -- imports") (Text.pack $ "\nimport qualified " ++ name ++ " --imports")

updateCabal :: String -> Text -> Text
updateCabal name =
  Text.replace (Text.pack "-- mainDeps") (Text.pack $ name ++ "\n    -- mainDeps")
    . flip Text.append (Text.pack $ cabalEntry name)

main = do
  (n : _) <- getArgs
  let name = "Day" ++ padLeft 2 '0' n
  createInputFile name
  createSourceFile name
  mainIn <- Text.readFile "./src/main.hs"
  cabalIn <- Text.readFile "./advent2021.cabal"
  Text.writeFile "./src/main.hs" $ updateMain (read n) name mainIn
  Text.writeFile "./advent2021.cabal" $ updateCabal name cabalIn