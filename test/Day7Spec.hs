{-# LANGUAGE QuasiQuotes #-}
module Day7Spec where

import Day7
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property
import NeatInterpolation
import qualified Data.Text as T

filesystem = directory "/"
        [ file "file1.txt" 100
        , file "file2.txt" 200
        , directory "folder1"
            [ file "file3.txt" 150
            , file "file4.txt" 300
            , directory "folder3"
              [ file "file5.txt" 200
              , file "file6.txt" 300
              ]
            ]
        , directory "folder2"
            [ file "file7.txt" 250
            ]
        ]

spec :: Spec
spec = describe "Day 7" $ do

     it "total size of a filesystem" $
        du filesystem `shouldBe` 1500

     it "total size of a filesystem" $
        duDir filesystem `shouldBe` [("/", 1500), ("folder1", 950), ("folder3", 500), ("folder2", 250)]

     it "print directory and file structure" $
        display filesystem `shouldBe` T.unpack [trimming|
           /
           |-- file1.txt
           |-- file2.txt
           |-- folder1
           |-- |-- file3.txt
           |-- |-- file4.txt
           |-- |-- folder3
           |-- |-- |-- file5.txt
           |-- |-- |-- file6.txt
           |-- folder2
           |-- |-- file7.txt|]


