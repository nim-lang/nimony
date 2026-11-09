# A raised `ref` object travels as `ref Exception`, and `except T` tests its
# type at run time: its object needs RTTI. Inheriting from `Exception` or
# `RootObj`, or being an `inheritable` root, gives it that, `final` or not.
# `traise_ref_lacks_rtti.nim` has the types that lack it.
import std/syncio

type
  ExcErr = ref object of Exception
    x: int
  RootErr = ref object of RootObj
    x: int
  OwnRoot {.inheritable.} = ref object
    x: int
  FinalErr {.final.} = ref object of Exception
    x: int
  GenErr[T] = ref object of Exception
    x: T

proc raiseExc() {.raises: ExcErr.} = raise ExcErr(x: 1)
proc raiseRoot() {.raises: RootErr.} = raise RootErr(x: 2)
proc raiseOwn() {.raises: OwnRoot.} = raise OwnRoot(x: 3)
proc raiseFinal() {.raises: FinalErr.} = raise FinalErr(x: 4)
proc raiseGen() {.raises: GenErr[int].} = raise GenErr[int](x: 5)

proc typedArms() =
  try: raiseExc()
  except ExcErr as e: echo "ExcErr ", e.x
  try: raiseRoot()
  except RootErr as e: echo "RootErr ", e.x
  try: raiseOwn()
  except OwnRoot as e: echo "OwnRoot ", e.x
  try: raiseFinal()
  except FinalErr as e: echo "FinalErr ", e.x
  try: raiseGen()
  except GenErr[int] as e: echo "GenErr ", e.x

proc bareArms() =
  try: raiseRoot()
  except: echo "bare RootErr"
  try: raiseOwn()
  except: echo "bare OwnRoot"

proc localRaise() {.raises: OwnRoot.} =
  # raised and caught in the same routine, then the next arm misses
  try: raise OwnRoot(x: 6)
  except ExcErr: echo "wrong arm"
  except OwnRoot as e: echo "local OwnRoot ", e.x

typedArms()
bareArms()
try: localRaise()
except: echo "not reached"
