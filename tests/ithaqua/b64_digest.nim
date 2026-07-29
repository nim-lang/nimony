import std/[syncio, base64, md5, sha1]

# base64 round trips, padding variants
echo encode("ward")                    # d2FyZA==
echo encode("wards")
echo encode("a")
echo encode("")
try:
  echo decode("d2FyZA==")              # ward
  echo decode(encode("progressive JXL streaming")) == "progressive JXL streaming"
except:
  echo "decode failed"

# digests render as hex strings — pure byte-crunching, width-independent.
# (nimony's sha1 has no one-shot secureHash; drive the state machine)
echo getMD5("hello world")             # 5eb63bbbe01eeed093cb22bb8f5acdc3
echo getMD5("")
var st = newSha1State()
st.update("hello world")
echo $SecureHash(st.finalize())        # 2AAE6C35C94FCFB415DBE95F408B9CE91EE846ED
var st2 = newSha1State()
echo $SecureHash(st2.finalize())
