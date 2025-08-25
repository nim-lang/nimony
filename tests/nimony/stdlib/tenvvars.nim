import std/[envvars, assertions]

import std/syncio


# "LATIN CAPITAL LETTER AE" in UTF-8 (0xc386)
const unicodeUtf8 = "\xc3\x86"

proc findEnvPair(key, val: string): bool =
  for (k, v) in envPairs():
    if k == key and v == val:
      return true

  return false


block: # delEnv, existsEnv, getEnv, envPairs
  var val = "val"
  const key = "NIM_TESTS_TOSENV_KEY"
  assert not existsEnv(key)

  putEnv(key, "tempval")
  assert existsEnv(key)
  assert getEnv(key) == "tempval"

  putEnv(key, val) # change a key that already exists
  assert existsEnv(key)
  assert getEnv(key) == val


  assert findEnvPair(key, val)
  delEnv(key)
  assert not findEnvPair(key, val)
  assert not existsEnv(key)
  delEnv(key) # deleting an already deleted env var
  assert not existsEnv(key)


block osenv:
  block delEnv:
    const dummyEnvVar = "DUMMY_ENV_VAR" # This env var wouldn't be likely to exist to begin with
    assert existsEnv(dummyEnvVar) == false
    putEnv(dummyEnvVar, "1")
    assert existsEnv(dummyEnvVar) == true
    delEnv(dummyEnvVar)
    assert existsEnv(dummyEnvVar) == false
    delEnv(dummyEnvVar)         # deleting an already deleted env var
    assert existsEnv(dummyEnvVar) == false
  # block: # putEnv, bug #18502
  #   doAssertRaises(OSError): putEnv("DUMMY_ENV_VAR_PUT=DUMMY_VALUE", "NEW_DUMMY_VALUE")
  #   doAssertRaises(OSError): putEnv("", "NEW_DUMMY_VALUE")
  block:
    assert getEnv("DUMMY_ENV_VAR_NONEXISTENT", "") == ""
    assert getEnv("DUMMY_ENV_VAR_NONEXISTENT", " ") == " "
    assert getEnv("DUMMY_ENV_VAR_NONEXISTENT", "Arrakis") == "Arrakis"