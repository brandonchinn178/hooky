import Test.Tasty

import Hooky.ConfigTest qualified
import Hooky.InstallTest qualified
import Hooky.Run.ExecutionPlanTest qualified
import Hooky.RunTest qualified
import Hooky.Utils.GitTest qualified

main :: IO ()
main =
  defaultMain . testGroup "Hooky" $
    unitTests <> integrationTests
  where
    unitTests =
      [ Hooky.Utils.GitTest.test
      , Hooky.ConfigTest.test
      , Hooky.Run.ExecutionPlanTest.test
      ]
    integrationTests =
      [ Hooky.InstallTest.test
      , Hooky.RunTest.test
      ]
