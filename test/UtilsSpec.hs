module UtilsSpec where

import Utils
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property

spec :: Spec
spec = describe "Utils" $ do

     it "" $
        1 `shouldBe` 1

     prop "property-based unit test" $
        \l -> reverse ( reverse l ) == ( l::[Int])

