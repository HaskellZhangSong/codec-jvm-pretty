{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Codec.JVM.Pretty.GenericOut where

import Text.PrettyPrint.GenericPretty
import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Generics
import GHC.Int
import GHC.Word
import Text.PrettyPrint
import Data.ByteString
import Data.Text

instance Ord a => Generic (M.Map a b) where
  type Rep (M.Map a b) = Rep [(a,b)]
  from bs = from (M.toList bs)
  to   bs = M.fromList $ to bs
  
instance (Out a, Ord a, Out b) => Out (M.Map a b)

instance Ord a => Generic (S.Set a) where
  type Rep (S.Set a) = Rep [a]
  from bs = from (S.toList bs)
  to   bs = S.fromList $ to bs

instance (Out a, Ord a) => Out (S.Set a)
  
word8 :: Word8 -> Doc
word8 x = text (show x)

word16 :: Word16 -> Doc
word16 x = text (show x)

word32 :: Word32 -> Doc
word32 x = text (show x)

int8 :: Int8 -> Doc
int8 x = text (show x)

int16 :: Int16 -> Doc
int16 x = text (show x)

int32 :: Int32 -> Doc
int32 x = text (show x)

int64 :: Int64 -> Doc
int64 x = text (show x)

instance Out Word8 where
  docPrec n x
      | n/=0 && x<0 = parens $ word8 x
      | otherwise = word8 x
  doc = docPrec 0  

instance Out Word16 where
  docPrec n x
      | n/=0 && x<0 = parens $ word16 x
      | otherwise = word16 x
  doc = docPrec 0  

instance Out Word32 where
  docPrec n x
      | n/=0 && x<0 = parens $ word32 x
      | otherwise = word32 x
  doc = docPrec 0  

instance Out Int8 where
  docPrec n x
      | n/=0 && x<0 = parens $ int8 x
      | otherwise = int8 x
  doc = docPrec 0  

instance Out Int16 where
  docPrec n x
      | n/=0 && x<0 = parens $ int16 x
      | otherwise = int16 x
  doc = docPrec 0  

instance Out Int32 where
  docPrec n x
      | n/=0 && x<0 = parens $ int32 x
      | otherwise = int32 x
  doc = docPrec 0  

instance Out Int64 where
  docPrec n x
      | n/=0 && x<0 = parens $ int64 x
      | otherwise = int64 x
  doc = docPrec 0  

bytestring :: ByteString -> Doc
bytestring x= text (show x)

instance Out ByteString where
  docPrec n x = parens $ bytestring x
  doc = docPrec 0

text' :: Text -> Doc
text' x = text (show x)

instance Out Text where
  docPrec n x = parens $ text' x
  doc = docPrec 0  

