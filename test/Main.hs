import Test.Tasty

import Hooky.GitTest qualified

main :: IO ()
main =
  defaultMain . testGroup "Hooky" $
    [ Hooky.GitTest.test
    ]
