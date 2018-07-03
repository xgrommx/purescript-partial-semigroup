module Data.PartialSemigroup where

import Prelude

import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Identity (Identity(..))
import Data.Lazy (defer, force)
import Data.List as L
import Data.List.Lazy as LL
import Data.List.Lazy.NonEmpty as NELL
import Data.List.NonEmpty as NEL
import Data.List.ZipList (ZipList(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid.Additive (Additive)
import Data.Monoid.Multiplicative (Multiplicative)
import Data.Newtype (class Newtype, wrap)
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..))

infixr 6 pappend as <>?

class PartialSemigroup a where
  pappend :: a -> a -> Maybe a

instance partialSemigroupUnit :: PartialSemigroup Unit where
  pappend _ _ = Just unit

instance partialSemigroupString :: PartialSemigroup String where
  pappend x y = Just (x <> y)

instance partialSemigroupArray :: PartialSemigroup (Array a) where
  pappend x y = Just (x <> y)

instance partialSemigroupAdditive :: Semiring a => PartialSemigroup (Additive a) where
  pappend x y = Just (x <> y)

instance partialSemigroupMultiplicative :: Semiring a => PartialSemigroup (Multiplicative a) where
  pappend x y = Just (x <> y)

instance partialSemigroupIdentity :: PartialSemigroup a => PartialSemigroup (Identity a) where
  pappend (Identity x) (Identity y) = Identity <$> (x <>? y)

instance partialSemigroupMaybe :: PartialSemigroup a => PartialSemigroup (Maybe a) where
  pappend ma mb = Just $ do
   a <- ma
   b <- mb
   pappend a b

instance partialSemigroupEither :: (PartialSemigroup a, PartialSemigroup b) => PartialSemigroup (Either a b) where
  pappend (Left x) (Left y) = Left <$> (x <>? y)
  pappend (Right x) (Right y) = Right <$> (x <>? y)
  pappend _ _ = Nothing

instance partialSemigroupTuple :: (PartialSemigroup a, PartialSemigroup b) => PartialSemigroup (Tuple a b) where
  pappend (Tuple a b) (Tuple a' b') = Tuple <$> (a <>? a') <*> (b <>? b')

instance partialSemigroupZipList :: PartialSemigroup a => PartialSemigroup (ZipList a) where
  pappend (ZipList x) (ZipList y) = ZipList <$> partialZipLazyList x y

newtype Partial a = Partial (Maybe a)

derive instance newtypePartial :: Newtype (Partial a) _
derive instance eqPartial :: Eq a => Eq (Partial a)
derive instance ordPartial :: Ord a => Ord (Partial a)
derive instance genericPartial :: Generic (Partial a) _

instance showPartial :: Show a => Show (Partial a) where
  show p = genericShow p

instance semigroupPartial :: PartialSemigroup a => Semigroup (Partial a) where
  append (Partial (Just x)) (Partial (Just y)) = wrap $ x <>? y
  append _ _ = wrap Nothing

instance monoidPartial :: (PartialSemigroup a, Monoid a) => Monoid (Partial a) where
  mempty = wrap $ Just mempty

newtype Total a = Total a

derive instance newtypeTotal :: Newtype (Total a) _
derive instance eqTotal :: Eq a => Eq (Total a)
derive instance ordTotal :: Ord a => Ord (Total a)
derive instance genericTotal :: Generic (Total a) _

instance showTotal :: Show a => Show (Total a) where
  show t = genericShow t

instance partialSemigroupTotal :: Semigroup a => PartialSemigroup (Total a) where
  pappend (Total x) (Total y) = Just $ wrap (x <> y)

newtype AppendLeft a b = AppendLeft (Either a b)

derive instance newtypeAppendLeft :: Newtype (AppendLeft a b) _
derive instance eqAppendLeft :: (Eq a, Eq b) => Eq (AppendLeft a b)
derive instance ordAppendLeft :: (Ord a, Ord b) => Ord (AppendLeft a b)
derive instance genericAppendLeft :: Generic (AppendLeft a b) _

instance showAppendLeft :: (Show a, Show b) => Show (AppendLeft a b) where
  show t = genericShow t

instance partialSemigroupAppendLeft :: PartialSemigroup a => PartialSemigroup (AppendLeft a b) where
  pappend (AppendLeft (Left x)) (AppendLeft (Left y)) = wrap <<< Left <$> (x <>? y)
  pappend _ _ = Nothing

newtype AppendRight a b = AppendRight (Either a b)

derive instance newtypeAppendRight :: Newtype (AppendRight a b) _
derive instance eqAppendRight :: (Eq a, Eq b) => Eq (AppendRight a b)
derive instance ordAppendRight :: (Ord a, Ord b) => Ord (AppendRight a b)
derive instance genericAppendRight :: Generic (AppendRight a b) _

instance showAppendRight :: (Show a, Show b) => Show (AppendRight a b) where
  show t = genericShow t

instance partialSemigroupAppendRight :: PartialSemigroup b => PartialSemigroup (AppendRight a b) where
  pappend (AppendRight (Right x)) (AppendRight (Right y)) = wrap <<< Right <$> (x <>? y)
  pappend _ _ = Nothing

groupAndConcatArray :: forall a. PartialSemigroup a => Array a -> Array a
groupAndConcatArray x = case A.uncons x of
  Nothing -> []
  Just r1 -> case A.uncons r1.tail of
    Nothing -> [r1.head]
    Just r2 -> case A.uncons r2.tail of
      Nothing -> case r1.head <>? r2.head of
        Nothing -> r1.head A.: r2.head A.: []
        Just a -> [a]
      Just _ -> case r1.head <>? r2.head of
        Nothing -> r1.head A.: groupAndConcatArray (r2.head A.: r2.tail)
        Just a -> groupAndConcatArray (a A.: r2.tail)

groupAndConcatList :: forall a. PartialSemigroup a => L.List a -> L.List a
groupAndConcatList a@L.Nil = a
groupAndConcatList a@(L.Cons _ L.Nil) = a
groupAndConcatList (x L.: y L.: zs) =
  case x <>? y of
    Nothing -> x L.: groupAndConcatList (y L.: zs)
    Just a  ->     groupAndConcatList (a L.: zs)

groupAndConcatLazyList :: forall a. PartialSemigroup a => LL.List a -> LL.List a
groupAndConcatLazyList xss = case LL.step xss of
  LL.Nil -> LL.nil
  LL.Cons x xs -> case LL.step xs of
    LL.Nil -> LL.cons x xs
    LL.Cons y ys -> case x <>? y of
      Nothing -> LL.cons x $ groupAndConcatLazyList (LL.cons y ys)
      Just a -> groupAndConcatLazyList (LL.cons a ys)

partialConcatArray :: forall a. PartialSemigroup a => Array a -> Maybe a
partialConcatArray = maybe Nothing partialConcatNonEmptyArray <<< NEA.fromArray

partialConcatList :: forall a. PartialSemigroup a => L.List a -> Maybe a
partialConcatList = maybe Nothing partialConcatNonEmptyList <<< NEL.fromList

partialConcatLazyList :: forall a. PartialSemigroup a => LL.List a -> Maybe a
partialConcatLazyList = maybe Nothing partialConcatNonEmptyLazyList <<< NELL.fromList

partialConcatNonEmptyList :: forall a. PartialSemigroup a => NEL.NonEmptyList a -> Maybe a
partialConcatNonEmptyList (NEL.NonEmptyList (x :| L.Nil)) = Just x
partialConcatNonEmptyList (NEL.NonEmptyList (x :| (L.Cons y zs))) = do
  a <- x <>? y
  partialConcatNonEmptyList $ NEL.NonEmptyList $ a :| zs

partialConcatNonEmptyLazyList :: forall a. PartialSemigroup a => NELL.NonEmptyList a -> Maybe a
partialConcatNonEmptyLazyList (NELL.NonEmptyList l) = case force l of
  x :| xs -> case LL.step xs of
    LL.Nil -> Just x
    LL.Cons y zs -> do
      a <- x <>? y
      partialConcatNonEmptyLazyList $ NELL.NonEmptyList $  defer \_ ->  a :| zs

partialConcatNonEmptyArray :: forall a. PartialSemigroup a => NEA.NonEmptyArray a -> Maybe a
partialConcatNonEmptyArray l = case NEA.uncons l of
  r -> case A.uncons r.tail of
    Nothing -> Just r.head
    Just r2 -> do
      a <- r.head <>? r2.head
      partialConcatNonEmptyArray $ NEA.cons' a r2.tail

partialZipArray :: forall a. PartialSemigroup a => Array a -> Array a -> Maybe (Array a)
partialZipArray x y = case A.uncons x, A.uncons y of
  Nothing, Nothing -> Just []
  Nothing, _ -> Nothing
  _, Nothing -> Nothing
  Just r1, Just r2 -> A.cons <$> (r1.head <>? r2.head) <*> partialZipArray r1.tail r2.tail

partialZipNonEmptyArray :: forall a. PartialSemigroup a => NEA.NonEmptyArray a -> NEA.NonEmptyArray a -> Maybe (NEA.NonEmptyArray a)
partialZipNonEmptyArray nx ny = case NEA.toNonEmpty nx, NEA.toNonEmpty ny of
  (x :| xs), (y :| ys) -> NEA.cons' <$> (x <>? y) <*> partialZipArray xs ys

partialZipList :: forall a. PartialSemigroup a => L.List a -> L.List a -> Maybe (L.List a)
partialZipList L.Nil L.Nil = Just L.Nil
partialZipList L.Nil _ = Nothing
partialZipList _ L.Nil = Nothing
partialZipList (L.Cons x xs) (L.Cons y ys) = L.Cons <$> (x <>? y) <*> partialZipList xs ys

partialZipNonEmptyList :: forall a. PartialSemigroup a => NEL.NonEmptyList a -> NEL.NonEmptyList a -> Maybe (NEL.NonEmptyList a)
partialZipNonEmptyList (NEL.NonEmptyList (x :| xs)) (NEL.NonEmptyList (y :| ys)) = (\a b -> NEL.NonEmptyList $ a :| b) <$> (x <>? y) <*> partialZipList xs ys

partialZipLazyList :: forall a. PartialSemigroup a => LL.List a -> LL.List a -> Maybe (LL.List a)
partialZipLazyList xss yss = case LL.step xss, LL.step yss of
  LL.Nil, LL.Nil -> Just $ LL.nil
  LL.Nil, _ -> Nothing
  _, LL.Nil -> Nothing
  LL.Cons x xs, LL.Cons y ys -> LL.cons <$> (x <>? y) <*> partialZipLazyList xs ys

partialZipNonEmptyLazyList :: forall a. PartialSemigroup a => NELL.NonEmptyList a -> NELL.NonEmptyList a -> Maybe (NELL.NonEmptyList a)
partialZipNonEmptyLazyList (NELL.NonEmptyList xss) (NELL.NonEmptyList yss) = case force xss, force yss of
  (x :| xs), (y :| ys) -> (\a b -> NELL.NonEmptyList $  defer \_ ->  a :| b) <$> (x <>? y) <*> partialZipLazyList xs ys