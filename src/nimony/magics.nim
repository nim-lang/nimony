#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Same as compiler/ast.nim, but could remove plenty of magics and then add new ones
## so it's better not to import it from compiler/ast.nim.

import nimony_model, stringviews, keymatcher

type
  TMagic* = enum # symbols that require compiler magic:
    mNone,
    mDefined, mDeclared, mDeclaredInScope, mCompiles, mArrGet, mArrPut, mAsgn,
    mLow, mHigh, mSizeOf, mAlignOf, mOffsetOf, mTypeTrait,
    mIs, mOf, mAddr, mType, mTypeOf,
    mPlugin, mEcho, mShallowCopy, mSlurp, mStaticExec, mStatic,
    mParseExprToAst, mParseStmtToAst, mExpandToAst, mQuoteAst,
    mInc, mDec, mOrd,
    mNew, mNewFinalize, mNewSeq, mNewSeqOfCap,
    mLengthOpenArray, mLengthStr, mLengthArray, mLengthSeq,
    mIncl, mExcl, mCard, mChr,
    mGCref, mGCunref,
    mAddI, mSubI, mMulI, mDivI, mModI,
    mSucc, mPred,
    mAddF64, mSubF64, mMulF64, mDivF64,
    mShrI, mShlI, mAshrI, mBitandI, mBitorI, mBitxorI,
    mMinI, mMaxI,
    mAddU, mSubU, mMulU, mDivU, mModU,
    mEqI, mLeI, mLtI,
    mEqF64, mLeF64, mLtF64,
    mLeU, mLtU,
    mEqEnum, mLeEnum, mLtEnum,
    mEqCh, mLeCh, mLtCh,
    mEqB, mLeB, mLtB,
    mEqRef, mLePtr, mLtPtr,
    mXor, mEqCString, mEqProc,
    mUnaryMinusI, mUnaryMinusI64, mAbsI, mNot,
    mUnaryPlusI, mBitnotI,
    mUnaryPlusF64, mUnaryMinusF64,
    mCharToStr, mBoolToStr,
    mCStrToStr,
    mStrToStr, mEnumToStr,
    mAnd, mOr,
    mImplies, mIff, mExists, mForall, mOld,
    mEqStr, mLeStr, mLtStr,
    mEqSet, mLeSet, mLtSet, mMulSet, mPlusSet, mMinusSet, mXorSet,
    mConStrStr, mSlice,
    mDotDot, # this one is only necessary to give nice compile time warnings
    mFields, mFieldPairs, mOmpParFor,
    mAppendStrCh, mAppendStrStr, mAppendSeqElem,
    mInSet, mRepr, mExit,
    mSetLengthStr, mSetLengthSeq,
    mIsPartOf, mAstToStr, mParallel,
    mSwap, mIsNil, mArrToSeq, mOpenArrayToSeq,
    mNewString, mNewStringOfCap, mParseBiggestFloat,
    mMove, mEnsureMove, mWasMoved, mDup, mDestroy, mTrace,
    mDefault, mUnown, mFinished, mIsolate, mAccessEnv, mAccessTypeField,
    mArray, mOpenArray, mRange, mSet, mSeq, mVarargs,
    mRef, mPtr, mVar, mDistinct, mVoid, mTuple,
    mOrdinal, mIterableType,
    mInt, mInt8, mInt16, mInt32, mInt64,
    mUInt, mUInt8, mUInt16, mUInt32, mUInt64,
    mFloat, mFloat32, mFloat64, mFloat128,
    mBool, mChar, mString, mCstring,
    mPointer, mNil, mExpr, mStmt, mTypeDesc,
    mVoidType, mPNimrodNode, mSpawn, mDeepCopy,
    mIsMainModule, mCompileDate, mCompileTime, mProcCall,
    mCpuEndian, mHostOS, mHostCPU, mBuildOS, mBuildCPU, mAppType,
    mCompileOption, mCompileOptionArg,
    mNLen, mNChild, mNSetChild, mNAdd, mNAddMultiple, mNDel,
    mNKind, mNSymKind,

    mNccValue, mNccInc, mNcsAdd, mNcsIncl, mNcsLen, mNcsAt,
    mNctPut, mNctLen, mNctGet, mNctHasNext, mNctNext,

    mNIntVal, mNFloatVal, mNSymbol, mNIdent, mNGetType, mNStrVal, mNSetIntVal,
    mNSetFloatVal, mNSetSymbol, mNSetIdent, mNSetStrVal, mNLineInfo,
    mNNewNimNode, mNCopyNimNode, mNCopyNimTree, mStrToIdent, mNSigHash, mNSizeOf,
    mNBindSym, mNCallSite,
    mEqIdent, mEqNimrodNode, mSameNodeType, mGetImpl, mNGenSym,
    mNHint, mNWarning, mNError,
    mInstantiationInfo, mGetTypeInfo, mGetTypeInfoV2,
    mNimvm, mIntDefine, mStrDefine, mBoolDefine, mGenericDefine, mRunnableExamples,
    mException, mBuiltinType, mSymOwner, mUncheckedArray, mGetImplTransf,
    mSymIsInstantiationOf, mNodeId, mPrivateAccess, mZeroDefault,
    # not in Nim 2:
    mUnpack
    mDefaultObj, mDefaultTup
    mArrAt, mPat
    mDeref

declareMatcher parseMagic, TMagic, 1, 1

template res(t: ExprKind | StmtKind | TypeKind; bits = 0): (string, int) = ($t, bits)

const
  TypedMagic* = -3

proc magicToTag*(m: TMagic): (string, int) =
  case m
  of mDefined: res DefinedX
  of mDeclared: res DeclaredX
  of mIsMainModule: res IsMainModuleX
  of mCompiles: res CompilesX
  of mArrGet: res AtX
  of mArrAt: res ArrAtX
  of mPat: res PatX
  of mAsgn: res AsgnS
  of mAddI, mAddU, mAddF64: res AddX, TypedMagic
  of mSubI, mSubU, mSubF64: res SubX, TypedMagic
  of mMulI, mMulU, mMulF64: res MulX, TypedMagic
  of mDivI, mDivU, mDivF64: res DivX, TypedMagic
  of mModI, mModU: res ModX, TypedMagic
  of mShrI: res ShrX, TypedMagic
  of mAshrI: res AshrX, TypedMagic
  of mShlI: res ShlX, TypedMagic
  of mBitandI: res BitandX, TypedMagic
  of mBitorI: res BitorX, TypedMagic
  of mBitxorI: res BitxorX, TypedMagic
  of mBitnotI: res BitnotX, TypedMagic
  of mAnd: res AndX
  of mOr: res OrX
  of mNot: res NotX
  of mSizeOf: res SizeofX
  of mType, mTypeOf: res TypeofX
  of mAddr: res AddrX
  of mDeref: res DerefX
  of mEqI, mEqB, mEqCh, mEqF64, mEqRef: res EqX, TypedMagic
  of mLeI, mLeB, mLeCh, mLeF64, mLePtr: res LeX, TypedMagic
  of mLtI, mLtB, mLtCh, mLtF64, mLtPtr: res LtX, TypedMagic
  of mLow: res LowX
  of mHigh: res HighX
  of mEnumToStr: res EnumToStrX
  of mArray: res ArrayT
  of mRange: res RangeT
  of mSet: res SetT
  of mVarargs: res VarargsT
  of mRef: res RefT
  of mPtr: res PtrT
  of mVar: res MutT
  of mDistinct: res DistinctT
  of mVoid: res VoidT
  of mTuple: res TupleT
  of mOrdinal: res OrdinalT
  of mIterableType: res IterT
  of mInt: res IntT, -1
  of mInt8: res IntT, 8
  of mInt16: res IntT, 16
  of mInt32: res IntT, 32
  of mInt64: res IntT, 64
  of mUInt: res UIntT, -1
  of mUInt8: res UIntT, 8
  of mUInt16: res UIntT, 16
  of mUInt32: res UIntT, 32
  of mUInt64: res UIntT, 64
  of mFloat: res FloatT, 64
  of mFloat32: res FloatT, 32
  of mFloat64: res FloatT, 64
  of mFloat128: res FloatT, 128
  of mBool: res BoolT
  of mChar: res CharT, 8
  of mString: res StringT
  of mTypeDesc: res TypedescT
  of mVoidType: res VoidT
  of mUnpack: res UnpackX
  of mExpr: res UntypedT
  of mStmt: res TypedT
  of mCstring: res CstringT
  of mPointer: res PointerT
  of mDefaultObj: res DefaultObjX
  of mDefaultTup: res DefaultTupX
  of mOpenArray: res OpenArrayT
  else: ("", 0)

when isMainModule:
  echo parseMagic "Int32"
