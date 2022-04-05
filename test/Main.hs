import Test.Tasty

import Hooky.ConfigTest qualified
import Hooky.InstallTest qualified
import Hooky.Utils.GitTest qualified

main :: IO ()
main =
  defaultMain . testGroup "Hooky" $
    [ Hooky.Utils.GitTest.test
    , Hooky.ConfigTest.test
    , Hooky.InstallTest.test
    ]
