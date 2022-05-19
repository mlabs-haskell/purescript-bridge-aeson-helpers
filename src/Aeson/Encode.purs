module Aeson.Encode
  ( class RowListEncoder
  , class ToTupleEncoder
  , Encoder
  , dictionary
  , either
  , encode
  , enum
  , maybe
  , null
  , record
  , rowListEncoder
  , encodeTagged
  , tuple
  , toTupleEncoder
  , tupleDivided
  , unit
  , value
  , (>*<)
  , (>$<)
  , (>/\<)
  ) where

import Prelude

import Aeson (class EncodeAeson, Aeson, aesonNull, caseAeson, constAesonCases, encodeAeson, fromString)
import Aeson.Utils (contentsProp, leftProp, rightProp, tagProp, unconsRecord)
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.Bitraversable (ltraverse)
import Data.Divide (divided)
import Data.Either (Either(Left, Right))
import Data.Functor.Contravariant (cmap)
import Data.Map (Map, toUnfoldable)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (over, unwrap)
import Data.Op (Op(Op))
import Data.Semigroup.Last (Last(Last))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))
import Foreign.Object (Object)
import Foreign.Object as Obj
import Prim.Row as R
import Prim.RowList (class RowToList, Cons, Nil)
import Type.Prelude (Proxy(Proxy))

type Encoder = Op Aeson
type JPropEncoder = Op (Object (Last Aeson))
type TupleEncoder = Op (Array Aeson)

infixr 4 divided as >*<

infixr 4 cmap as >$<

class ToTupleEncoder f where
  toTupleEncoder :: forall a. f a -> TupleEncoder a

instance toTupleEncoderEncoder :: ToTupleEncoder (Op Aeson) where
  toTupleEncoder = mapEncoder Array.singleton

instance toTupleEncoderTupleEncoder :: ToTupleEncoder (Op (Array Aeson)) where
  toTupleEncoder = identity

class RowListEncoder :: forall k. k -> Row Type -> Row Type -> Constraint
class RowListEncoder rl ri ro | rl -> ri ro where
  rowListEncoder :: Proxy rl -> Record ri -> JPropEncoder (Record ro)

instance rowListEncoderNil :: RowListEncoder Nil () () where
  rowListEncoder _ _ = Op $ const Obj.empty

instance rowListEncoderEncoderCons ::
  ( IsSymbol prop
  , R.Lacks prop tailEncoders
  , R.Cons prop (Encoder a) tailEncoders encoders
  , R.Lacks prop tailValues
  , R.Cons prop a tailValues values
  , RowListEncoder tail tailEncoders tailValues
  ) =>
  RowListEncoder (Cons prop (Encoder a) tail) encoders values where
  rowListEncoder _ encoders =
    let
      Tuple valueEncoder tailEncoders = unconsRecord (Proxy :: _ prop) encoders
    in
      unconsRecord (Proxy :: _ prop)
        >$< propEncoder (Proxy :: _ prop) valueEncoder
        >*< rowListEncoder (Proxy :: _ tail) tailEncoders

propEncoder
  :: forall p a
   . IsSymbol p
  => Proxy p
  -> Encoder a
  -> JPropEncoder a
propEncoder p encoder =
  Op $ Obj.singleton (reflectSymbol p) <<< Last <<< encode encoder

value :: forall a. EncodeAeson a => Encoder a
value = Op $ encodeAeson

maybe :: forall a. Encoder a -> Encoder (Maybe a)
maybe encoder = Op case _ of
  Just a -> encode encoder a
  Nothing -> aesonNull

either :: forall a b. Encoder a -> Encoder b -> Encoder (Either a b)
either encoderA encoderB = Op case _ of
  Left a -> encodeAeson $ Obj.fromFoldable [ leftProp /\ encode encoderA a ]
  Right b -> encodeAeson $ Obj.fromFoldable [ rightProp /\ encode encoderB b ]

dictionary :: forall a b. Encoder a -> Encoder b -> Encoder (Map a b)
dictionary encoderA encoderB = Op $ toPairs >>> encodePairs
  where
  toPairs :: Map a b -> Array (Tuple Aeson Aeson)
  toPairs = map (bimap (encode encoderA) (encode encoderB)) <<< toUnfoldable
  encodePairs :: Array (Tuple Aeson Aeson) -> Aeson
  encodePairs pairs = case traverse (ltraverse tryString) pairs of
    Nothing -> encodeAeson $ map (lmap encodeAeson) pairs
    Just pairs' -> encodeAeson $ Obj.fromFoldable pairs'
  tryString =
    caseAeson (constAesonCases Nothing) { caseString = Just }

enum :: forall a. Show a => Encoder a
enum = Op $ fromString <<< show

record
  :: forall rl ro ri
   . RowToList ri rl
  => RowListEncoder rl ri ro
  => Record ri
  -> Encoder (Record ro)
record encoders =
  mapEncoder (encodeAeson <<< map unwrap) $ rowListEncoder (Proxy :: _ rl)
    encoders

tupleDivided
  :: forall f a b. ToTupleEncoder f => Encoder a -> f b -> TupleEncoder (a /\ b)
tupleDivided encoder = divided (toTupleEncoder encoder) <<< toTupleEncoder

infixr 6 tupleDivided as >/\<

tuple :: forall a. TupleEncoder a -> Encoder a
tuple = over Op $ compose encodeAeson

unit :: Encoder Unit
unit = Op $ const $ encodeAeson ([] :: Array Int)

null :: Encoder Unit
null = Op $ const aesonNull

encode :: forall a b. Op a b -> b -> a
encode = unwrap

encodeTagged :: forall a. String -> a -> Encoder a -> Aeson
encodeTagged tag a encoder = encodeAeson $ Obj.fromFoldable
  [ tagProp /\ (encodeAeson tag)
  , contentsProp /\ encode encoder a
  ]

mapEncoder :: forall a b c. (a -> b) -> Op a c -> Op b c
mapEncoder = over Op <<< map
