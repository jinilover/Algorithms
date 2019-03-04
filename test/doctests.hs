import Test.DocTest
import Protolude

main :: IO ()
main = doctest
  [ "-isrc"
  , "src/DynamicProgramming.hs"
  , "src/Trees.hs" 
  ]