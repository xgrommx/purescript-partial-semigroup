module Test.Main where

import Prelude

import Data.Array ((..)) as A
import Data.Either (Either(..))
import Data.Monoid.Multiplicative (Multiplicative(..))
import Data.Newtype (unwrap)
import Data.PartialSemigroup (AppendLeft(..), AppendRight(..), Total(..), groupAndConcatArray, partialConcatArray, partialZipArray, (<>?))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (logShow)

main :: Effect Unit
main = do
  let xss = [Left "a", Left "b", Right "c", Right "d", Left "e", Left "f"]
  let x = Tuple (Left "ab" :: Either String String) (Right "hi" :: Either String String)
  let y = Tuple (Left "cd") (Right "jk")
  let xs = [Left "a", Left "b", Right "c"]
  let ys = [Left "1", Left "2", Right "3"]
  logShow $ Left "ab" <>? (Left "cd" :: Either String String)
  logShow $ Left "ab" <>? Right [1, 2]
  logShow $ AppendLeft (Left "ab") <>? AppendLeft (Left "cd" :: Either String String)
  logShow $ AppendLeft (Right "ab") <>? AppendLeft (Right "cd" :: Either String String)
  logShow $ map unwrap <<< groupAndConcatArray <<< map AppendLeft $ xss
  logShow $ AppendRight (Right "ab") <>? AppendRight (Right "cd" :: Either String String)
  logShow $ AppendRight (Left "ab") <>? AppendRight (Left "cd" :: Either String String)
  logShow $ map unwrap <<< groupAndConcatArray <<< map AppendRight $ xss
  logShow $ x <>? y
  logShow $ groupAndConcatArray xss
  logShow $ partialConcatArray [Left "a", Left "b", Left "c" :: Either String String]
  logShow $ partialConcatArray [Left "a", Left "b", Right "c" :: Either String String]
  logShow $ partialConcatArray ([] :: Array (Either String String))
  logShow $ partialZipArray xs ys
  logShow $ Total "ab" <>? Total "cd"
  logShow $ map (unwrap <<< unwrap) <<< partialConcatArray <<< map (Total <<< Multiplicative) $ (1 A...4 )
  logShow $ groupAndConcatArray [Left [1], Left [2], Right "a", Left [3], Right "b", Right "c"]
