when defined(nimony):
  discard "good"
else:
  discard "bad"

when defined(undefinedDefine):
  discard "bad"
else:
  discard "good"

when defined(nimony) and not defined(undefinedDefine):
  discard "good"
else:
  discard "bad"

when defined(undefinedDefine) or not defined(nimony):
  discard "bad"
else:
  discard "good"
