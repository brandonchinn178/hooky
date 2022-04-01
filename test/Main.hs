import Test.Tasty

import Hooky.InstallTest qualified
import Hooky.Utils.GitTest qualified

main :: IO ()
main =
  defaultMain . testGroup "Hooky" $
    [ Hooky.Utils.GitTest.test
    , Hooky.InstallTest.test
    ]
