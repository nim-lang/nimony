#
#
#            Nim's Runtime Library
#        (c) Copyright 2024 Nim contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements types which encapsulate an optional value.
##
## A value of type `Option[T]` either contains a value `x` (represented as
## `some(x)`) or is empty (`none(T)`).
##
## This is the Nimony port. Since exceptions are not yet wired up, `get` on an
## empty `Option` terminates the program (mirroring `assert`) rather than
## raising `UnpackDefect`. Equality is the compiler-provided structural
## comparison: an empty `Option` always stores the default value, so two empty
## `Option`s compare equal and an empty one never equals a populated one.

import std/syncio

type
  Option*[T] = object
    ## An optional type that may or may not contain a value of type `T`.
    val: T
    has: bool


func raiseEmptyOption() {.noinline, noreturn.} =
  {.cast(noSideEffect).}: quit "Tried to access the value of an empty Option"

func some*[T](val: sink T): Option[T] =
  ## Returns an `Option` that contains `val`.
  runnableExamples:
    let a = some(5)
    assert a.isSome
  result = Option[T](val: val, has: true)

func none*[T: HasDefault](): Option[T] =
  ## Returns an empty `Option` (i.e. with no value).
  runnableExamples:
    assert none[int]().isNone
  result = Option[T](val: default(T), has: false)

func option*[T](val: sink T): Option[T] =
  ## Wraps `val` in an `Option`; for value types this is simply `some(val)`.
  result = Option[T](val: val, has: true)

func isSome*[T](self: Option[T]): bool {.inline.} =
  ## Returns `true` if `self` contains a value.
  self.has

func isNone*[T](self: Option[T]): bool {.inline.} =
  ## Returns `true` if `self` is empty.
  not self.has

func unsafeGet*[T](self: Option[T]): T {.inline.} =
  ## Returns the contained value. Behaviour is undefined when `self` is empty;
  ## only use after checking `isSome`.
  self.val

func get*[T](self: Option[T]): T =
  ## Returns the contained value. Terminates the program if `self` is empty.
  if self.isNone:
    raiseEmptyOption()
  self.val

func get*[T](self: Option[T]; otherwise: T): T =
  ## Returns the contained value, or `otherwise` if `self` is empty.
  if self.isSome: self.val
  else: otherwise

func `$`*[T: Stringable](self: Option[T]): string =
  ## Returns the string representation of `self`.
  if self.has:
    result = "Some("
    result.add $self.val
    result.add ")"
  else:
    result = "None"

proc map*[T; R: HasDefault](self: Option[T]; cb: proc (x: T): R): Option[R] =
  ## Applies `cb` to the contained value and wraps the result; empty stays empty.
  if self.has: some(cb(self.val))
  else: none[R]()

proc filter*[T: HasDefault](self: Option[T]; cb: proc (x: T): bool): Option[T] =
  ## Returns `self` if it holds a value satisfying `cb`, otherwise empty.
  if self.has and cb(self.val): self
  else: none[T]()

proc flatMap*[T; R: HasDefault](self: Option[T]; cb: proc (x: T): Option[R]): Option[R] =
  ## Applies `cb` (which itself returns an `Option`) and flattens the result.
  if self.has: cb(self.val)
  else: none[R]()

func flatten*[T: HasDefault](self: Option[Option[T]]): Option[T] =
  ## Collapses a nested `Option` into a single one.
  if self.has: self.val
  else: none[T]()
