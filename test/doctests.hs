import Test.DocTest
import Protolude

main :: IO ()
main = doctest
  [ "-isrc"
  , "src/DynamicProgramming.hs"
  , "src/Graphs.hs"
  , "src/Lists.hs"
  , "src/Sort.hs"
  , "src/Trees.hs" 
  ]