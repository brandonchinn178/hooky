import Test.Tasty

import Hooky.GitTest qualified
import Hooky.InstallTest qualified

main :: IO ()
main =
  defaultMain . testGroup "Hooky" $
    [ Hooky.GitTest.test
    , Hooky.InstallTest.test
    ]
