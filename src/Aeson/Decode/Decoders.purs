-- | Adaptation of `Data.Argonaut.Decode.Decoders` from `argonaut-codecs`
module Aeson.Decode.Decoders where

import Prelude

import Aeson (Aeson, caseAesonBoolean, caseAesonNull, caseAesonNumber, caseAesonString, isNull, toArray, toObject, toString, toStringifiedNumbersJson)
import Data.Argonaut.Core (fromString)
import Data.Argonaut.Decode.Error (JsonDecodeError(TypeMismatch, Named, AtKey, MissingValue, UnexpectedValue, AtIndex))
import Data.Array as Arr
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right), note)
import Data.Identity (Identity(Identity))
import Data.Int (fromNumber)
import Data.List (List, fromFoldable)
import Data.List as L
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Map as M
import Data.Maybe (maybe, Maybe(Just, Nothing))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Number as Number
import Data.Set as S
import Data.String (CodePoint, codePointAt)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NonEmptyString
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(Tuple))
import Foreign.Object as FO

decodeIdentity
  :: forall a
   . (Aeson -> Either JsonDecodeError a)
  -> Aeson
  -> Either JsonDecodeError (Identity a)
decodeIdentity decoder json = Identity <$> decoder json

decodeMaybe
  :: forall a
   . (Aeson -> Either JsonDecodeError a)
  -> Aeson
  -> Either JsonDecodeError (Maybe a)
decodeMaybe decoder json
  | isNull json = pure Nothing
  | otherwise = Just <$> decoder json

decodeTuple
  :: forall a b
   . (Aeson -> Either JsonDecodeError a)
  -> (Aeson -> Either JsonDecodeError b)
  -> Aeson
  -> Either JsonDecodeError (Tuple a b)
decodeTuple decoderA decoderB json = decodeArray Right json >>= f
  where
  f :: Array Aeson -> Either JsonDecodeError (Tuple a b)
  f = case _ of
    [a, b] -> Tuple <$> decoderA a <*> decoderB b
    _ -> Left $ TypeMismatch "Tuple"

decodeEither
  :: forall a b
   . (Aeson -> Either JsonDecodeError a)
  -> (Aeson -> Either JsonDecodeError b)
  -> Aeson
  -> Either JsonDecodeError (Either a b)
decodeEither decoderA decoderB json =
  lmap (Named "Either") $ decodeJObject json >>= \obj -> do
    tag <- note (AtKey "tag" MissingValue) $ FO.lookup "tag" obj
    val <- note (AtKey "value" MissingValue) $ FO.lookup "value" obj
    case toString tag of
      Just "Right" -> Right <$> decoderB val
      Just "Left" -> Left <$> decoderA val
      _ -> Left $ AtKey "tag" (UnexpectedValue $ toStringifiedNumbersJson tag)

decodeNull :: Aeson -> Either JsonDecodeError Unit
decodeNull = caseAesonNull (Left $ TypeMismatch "null") (const $ Right unit)

decodeBoolean :: Aeson -> Either JsonDecodeError Boolean
decodeBoolean = caseAesonBoolean (Left $ TypeMismatch "Boolean") Right

decodeNumber :: Aeson -> Either JsonDecodeError Number
decodeNumber aeson = note err $ caseAesonNumber Nothing Number.fromString aeson
  where
    err = TypeMismatch "Number"

decodeInt :: Aeson -> Either JsonDecodeError Int
decodeInt = note (TypeMismatch "Integer") <<< fromNumber <=< decodeNumber

decodeString :: Aeson -> Either JsonDecodeError String
decodeString = caseAesonString (Left $ TypeMismatch "String") Right

decodeNonEmptyString :: Aeson -> Either JsonDecodeError NonEmptyString
decodeNonEmptyString json =
  note (Named "NonEmptyString" $ UnexpectedValue $ toStringifiedNumbersJson json)
    =<< map (NonEmptyString.fromString) (decodeString json)

decodeNonEmpty_Array
  :: forall a
   . (Aeson -> Either JsonDecodeError a)
  -> Aeson
  -> Either JsonDecodeError (NonEmpty Array a)
decodeNonEmpty_Array decoder =
  lmap (Named "NonEmpty Array")
    <<< traverse decoder
    <=< map (\x -> x.head :| x.tail)
    <<< note (TypeMismatch "NonEmpty Array")
    <<< Arr.uncons
    <=< decodeJArray

decodeNonEmptyArray
  :: forall a
   . (Aeson -> Either JsonDecodeError a)
  -> Aeson
  -> Either JsonDecodeError (NonEmptyArray a)
decodeNonEmptyArray decoder =
  lmap (Named "NonEmptyArray")
    <<< traverse decoder
    <=< map (\x -> NEA.cons' x.head x.tail)
    <<< note (TypeMismatch "NonEmptyArray")
    <<< Arr.uncons
    <=< decodeJArray

decodeNonEmpty_List
  :: forall a
   . (Aeson -> Either JsonDecodeError a)
  -> Aeson
  -> Either JsonDecodeError (NonEmpty List a)
decodeNonEmpty_List decoder =
  lmap (Named "NonEmpty List")
    <<< traverse decoder
    <=< map (\x -> x.head :| x.tail)
    <<< note (TypeMismatch "NonEmpty List")
    <<< L.uncons
    <=< map (map fromFoldable) decodeJArray

decodeNonEmptyList
  :: forall a
   . (Aeson -> Either JsonDecodeError a)
  -> Aeson
  -> Either JsonDecodeError (NonEmptyList a)
decodeNonEmptyList decoder =
  lmap (Named "NonEmptyList")
    <<< traverse decoder
    <=< map (\x -> NEL.cons' x.head x.tail)
    <<< note (TypeMismatch "NonEmptyList")
    <<< L.uncons
    <=< map (map fromFoldable) decodeJArray

decodeCodePoint :: Aeson -> Either JsonDecodeError CodePoint
decodeCodePoint json =
  note (Named "CodePoint" $ UnexpectedValue $ toStringifiedNumbersJson json)
    =<< map (codePointAt 0) (decodeString json)

decodeForeignObject
  :: forall a
   . (Aeson -> Either JsonDecodeError a)
  -> Aeson
  -> Either JsonDecodeError (FO.Object a)
decodeForeignObject decoder =
  lmap (Named "ForeignObject")
    <<< traverse decoder
    <=< decodeJObject

decodeArray
  :: forall a
   . (Aeson -> Either JsonDecodeError a)
  -> Aeson
  -> Either JsonDecodeError (Array a)
decodeArray decoder =
  lmap (Named "Array")
    <<< traverseWithIndex (\i -> lmap (AtIndex i) <<< decoder)
    <=< decodeJArray

decodeList
  :: forall a
   . (Aeson -> Either JsonDecodeError a)
  -> Aeson
  -> Either JsonDecodeError (List a)
decodeList decoder =
  lmap (Named "List")
    <<< traverse decoder
    <=< map (map fromFoldable) decodeJArray

decodeSet
  :: forall a
   . Ord a
  => (Aeson -> Either JsonDecodeError a)
  -> Aeson
  -> Either JsonDecodeError (S.Set a)
decodeSet decoder =
  map (S.fromFoldable :: List a -> S.Set a) <<< decodeList decoder

decodeMap
  :: forall a b
   . Ord a
  => (Aeson -> Either JsonDecodeError a)
  -> (Aeson -> Either JsonDecodeError b)
  -> Aeson
  -> Either JsonDecodeError (M.Map a b)
decodeMap decoderA decoderB =
  map (M.fromFoldable :: List (Tuple a b) -> M.Map a b)
    <<< decodeList (decodeTuple decoderA decoderB)

decodeVoid :: Aeson -> Either JsonDecodeError Void
decodeVoid _ = Left $ UnexpectedValue $ fromString "Value cannot be Void"

decodeJArray :: Aeson -> Either JsonDecodeError (Array Aeson)
decodeJArray = note (TypeMismatch "Array") <<< toArray

decodeJObject :: Aeson -> Either JsonDecodeError (FO.Object Aeson)
decodeJObject = note (TypeMismatch "Object") <<< toObject

getField
  :: forall a
   . (Aeson -> Either JsonDecodeError a)
  -> FO.Object Aeson
  -> String
  -> Either JsonDecodeError a
getField decoder obj str =
  maybe
    (Left $ AtKey str MissingValue)
    (lmap (AtKey str) <<< decoder)
    (FO.lookup str obj)

getFieldOptional
  :: forall a
   . (Aeson -> Either JsonDecodeError a)
  -> FO.Object Aeson
  -> String
  -> Either JsonDecodeError (Maybe a)
getFieldOptional decoder obj str =
  maybe (pure Nothing) (map Just <<< decode) (FO.lookup str obj)
  where
  decode = lmap (AtKey str) <<< decoder

getFieldOptional'
  :: forall a
   . (Aeson -> Either JsonDecodeError a)
  -> FO.Object Aeson
  -> String
  -> Either JsonDecodeError (Maybe a)
getFieldOptional' decoder obj str =
  maybe (pure Nothing) decode (FO.lookup str obj)
  where
  decode json =
    if isNull json then
      pure Nothing
    else
      Just <$> (lmap (AtKey str) <<< decoder) json
