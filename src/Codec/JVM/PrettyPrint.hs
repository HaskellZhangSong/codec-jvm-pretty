{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving , CPP#-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Codec.JVM.PrettyPrint where

import Codec.JVM
import Codec.JVM.ASM.Code.Instr
import Codec.JVM.ConstPool
import Codec.JVM.Const
import Codec.JVM.Method
import Codec.JVM.Field
import Codec.JVM.Attr
import Codec.JVM.ASM.Code.CtrlFlow

import GHC.Generics
import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint
import Codec.JVM.Pretty.GenericOut

#define DERIVE_INSTANCE(T) deriving instance Generic (T); deriving instance Out (T)
DERIVE_INSTANCE(ClassFile)
DERIVE_INSTANCE(Version)
DERIVE_INSTANCE(IClassName)
DERIVE_INSTANCE(Const)
DERIVE_INSTANCE(MethodInfo)
DERIVE_INSTANCE(FieldInfo)
DERIVE_INSTANCE(AccessFlag)
DERIVE_INSTANCE(Attr)
DERIVE_INSTANCE(NameAndDesc)
DERIVE_INSTANCE(MethodRef)
DERIVE_INSTANCE(FieldRef)
DERIVE_INSTANCE(ConstVal)
DERIVE_INSTANCE(UName)
DERIVE_INSTANCE(Desc)
DERIVE_INSTANCE(Code)
DERIVE_INSTANCE(Offset)
DERIVE_INSTANCE(FieldType)
DERIVE_INSTANCE(PrimType)
DERIVE_INSTANCE(StackMapFrame)
DERIVE_INSTANCE(InnerClassMap)
DERIVE_INSTANCE(InnerClass)
DERIVE_INSTANCE(VerifType)
DERIVE_INSTANCE(MethodDef)
DERIVE_INSTANCE(MethodDesc)
#undef DERIVE_INSTANCE

deriving instance Generic (Signature a)
deriving instance Out a => Out (Signature a)

deriving instance Generic (MethodSignature a)
deriving instance Out a => Out (MethodSignature a)

deriving instance Generic (FieldSignature a)
deriving instance Out a => Out (FieldSignature a)

deriving instance Generic (ClassSignature a)
deriving instance Out a => Out (ClassSignature a)

deriving instance Generic (TypeVariableDeclaration a)
deriving instance Out a => Out (TypeVariableDeclaration a)

deriving instance Generic (ReferenceParameter a)
deriving instance Out a => Out (ReferenceParameter a)

deriving instance Generic (Parameter a)
deriving instance Out a => Out (Parameter a)

deriving instance Generic (TypeParameter a)
deriving instance Out a => Out (TypeParameter a)

deriving instance Generic (Bound a)
deriving instance Out a => Out (Bound a)

instance Out Instr where
  docPrec n x = parens $ text (show x)
  doc = docPrec 0  



