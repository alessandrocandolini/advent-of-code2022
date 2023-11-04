{-# LANGUAGE QuasiQuotes #-}
module Day7Spec where

import Day7
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property
import NeatInterpolation

spec :: Spec
spec = describe "Day 7" $ do

     it "total size of a filesystem" $ let
       filesystem = Directory "/"
        [ File "file1.txt" 100
        , File "file2.txt" 200
        , Directory "folder1"
            [ File "file3.txt" 150
            , File "file4.txt" 300
            , Directory "folder3"
              [ File "file5.txt" 200
              , File "file6.txt" 300
              ]
            ]
        , Directory "folder2"
            [ File "file7.txt" 250
            ]
        ]
      in
        du filesystem `shouldBe` (1500 :: Size)


