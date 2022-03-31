import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain . testGroup "Hooky" $
    [ testCase "smoketest" $ return ()
    ]
