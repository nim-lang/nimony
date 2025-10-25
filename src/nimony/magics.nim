#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Magics, somewhat compatible with Nim 2.0, but it's neither required nor desirable.

import ".." / models / tags
import ".." / lib / nifstreams
import nimony_model

template res(t: ExprKind | StmtKind | TypeKind; bits = 0): (string, int) = ($t, bits)

const
  TypedMagic* = -3

proc magicToTag*(m: string): (string, int) =
  case m
  of "Defined": res DefinedX
  of "Declared": res DeclaredX
  of "AstToStr": res AstToStrX
  of "IsMainModule": res IsMainModuleX
  of "Compiles": res CompilesX
  of "ArrGet": res AtX
  of "ArrAt": res ArrAtX
  of "Pat": res PatX
  of "TupAt": res TupatX
  of "Asgn": res AsgnS
  of "AddI", "AddU", "AddF64", "Succ": res AddX, TypedMagic
  of "SubI", "SubU", "SubF64", "Pred": res SubX, TypedMagic
  of "MulI", "MulU", "MulF64": res MulX, TypedMagic
  of "DivI", "DivU", "DivF64": res DivX, TypedMagic
  of "ModI", "ModU": res ModX, TypedMagic
  of "ShrI": res ShrX, TypedMagic
  of "AshrI": res AshrX, TypedMagic
  of "ShlI": res ShlX, TypedMagic
  of "BitandI": res BitandX, TypedMagic
  of "BitorI": res BitorX, TypedMagic
  of "BitxorI": res BitxorX, TypedMagic
  of "BitnotI": res BitnotX, TypedMagic
  of "UnaryMinusI", "UnaryMinusF64": res NegX, TypedMagic
  of "Delay": res DelayX, TypedMagic
  of "And": res AndX
  of "Or": res OrX
  of "Not": res NotX
  of "Xor": res XorX
  of "SizeOf": res SizeofX
  of "Type", "TypeOf": res TypeofX
  of "Addr": res AddrX
  of "Deref": res DerefX
  of "EqI", "EqB", "EqCh", "EqF64", "EqRef", "EqEnum": res EqX, TypedMagic
  of "LeI", "LeU", "LeB", "LeCh", "LeF64", "LePtr", "LeEnum": res LeX, TypedMagic
  of "LtI", "LtU", "LtB", "LtCh", "LtF64", "LtPtr", "LtEnum": res LtX, TypedMagic
  of "Low": res LowX
  of "High": res HighX
  of "EnumToStr": res EnumToStrX
  of "Array": res ArrayT
  of "Range": res RangetypeT
  of "Set": res SetT
  of "Varargs": res VarargsT
  of "Ref": res RefT
  of "Ptr": res PtrT
  of "Var": res MutT
  of "Distinct": res DistinctT
  of "Void": res VoidT
  of "Tuple": res TupleT
  of "Ordinal": res OrdinalT
  of "IterableType": res IteratorT
  of "Int": res IntT, -1
  of "Int8": res IntT, 8
  of "Int16": res IntT, 16
  of "Int32": res IntT, 32
  of "Int64": res IntT, 64
  of "UInt": res UIntT, -1
  of "UInt8": res UIntT, 8
  of "UInt16": res UIntT, 16
  of "UInt32": res UIntT, 32
  of "UInt64": res UIntT, 64
  of "Float": res FloatT, 64
  of "Float32": res FloatT, 32
  of "Float64": res FloatT, 64
  of "Float128": res FloatT, 128
  of "Bool": res BoolT
  of "Char": res CharT, 8
  of "TypeDesc": res TypedescT
  of "VoidType": res VoidT
  of "Unpack": res UnpackX
  of "Expr": res UntypedT
  of "Stmt": res TypedT
  of "Cstring": res CstringT
  of "Pointer": res PointerT
  of "DefaultObj": res DefaultObjX
  of "DefaultTup": res DefaultTupX
  of "DefaultDistinct": res DefaultDistinctX
  of "PlusSet": res PlusSetX, TypedMagic
  of "MinusSet": res MinusSetX, TypedMagic
  of "MulSet": res MulSetX, TypedMagic
  of "XorSet": res XorSetX, TypedMagic
  of "EqSet": res EqSetX, TypedMagic
  of "LeSet": res LeSetX, TypedMagic
  of "LtSet": res LtSetX, TypedMagic
  of "InSet": res InSetX, TypedMagic
  of "Card": res CardX, TypedMagic
  of "Incl": res InclS, TypedMagic
  of "Excl": res ExclS, TypedMagic
  of "EnsureMove": res EmoveX
  of "UncheckedArray": res UarrayT
  of "Sink": res SinkT
  of "Lent": res LentT
  of "Destroy": res DestroyX
  of "SinkHook": res SinkhX
  of "Dup": res DupX
  of "Copy": res CopyX
  of "WasMoved": res WasMovedX
  of "Trace": res TraceX
  of "NewRef": res NewrefX, TypedMagic
  of "OverflowFlag": res OvfX
  of "Fields": res FieldsX, TypedMagic
  of "FieldPairs": res FieldPairsX, TypedMagic
  of "Of": res InstanceofX
  of "ProcCall": res ProccallX
  of "InternalTypeName": res InternalTypeNameX
  of "InternalFieldPairs": res InternalFieldPairsX, TypedMagic
  of "OrdinalEnum": res EnumT
  of "HoleyEnum": res HoleyEnumT
  of "NaN": res NanX
  of "Inf": res InfX
  of "Is": res IsX
  else: ("", 0)
