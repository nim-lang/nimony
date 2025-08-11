when not defined(windows):
  import std/envvars
  import std/[assertions]
  
  import std/syncio


  # "LATIN CAPITAL LETTER AE" in UTF-8 (0xc386)
  const unicodeUtf8 = "\xc3\x86"

  proc findEnvPair(key, val: string): bool =
    for (k, v) in envPairs():
      if k == key and v == val:
        return true

    return false


  block: # delEnv, existsEnv, getEnv, envPairs
    for val in ["val", "", unicodeUtf8]: # ensures empty val works too
      const key = "NIM_TESTS_TOSENV_KEY"
      assert not existsEnv(key)

      putEnv(key, "tempval")
      assert existsEnv(key)
      assert getEnv(key) == "tempval"

      putEnv(key, val) # change a key that already exists
      assert existsEnv(key)
      assert getEnv(key) == val


      # In the main code:

      # TODO: when val is empty, memory sems to be corrupt somehow
      # assert findEnvPair(key, val)
      delEnv(key)
      assert not findEnvPair(key, val)
      assert not existsEnv(key)
      delEnv(key) # deleting an already deleted env var
      assert not existsEnv(key)

    block:
      assert getEnv("NIM_TESTS_TOSENV_NONEXISTENT", "") == ""
      assert getEnv("NIM_TESTS_TOSENV_NONEXISTENT", " ") == " "
      assert getEnv("NIM_TESTS_TOSENV_NONEXISTENT", "defval") == "defval"


      assert not existsEnv("")
      assert not existsEnv("NIM_TESTS_TOSENV_PUT=DUMMY_VALUE")
      assert not existsEnv("NIM_TESTS_TOSENV_PUT")
