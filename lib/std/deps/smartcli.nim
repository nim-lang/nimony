import std/[parseopt, parseutils, strutils]

import plugins
import std/smartcli

type
  FieldKind = enum
    fkString
    fkBool
    fkEnum

  PositionalMode = enum
    pmNone
    pmFlat
    pmSharedCommand
    pmInlineCommand

  SectionKind = enum
    skNone
    skUsage
    skArguments
    skCommands
    skOptions

  OptionSpec = object
    shortName: string
    longName: string
    kind: FieldKind
    choices: seq[string]

  CommandSpec = object
    name: string
    argumentNames: seq[string]

  CliSpec = object
    commands: seq[CommandSpec]
    argumentNames: seq[string]
    options: seq[OptionSpec]

proc toPascalCase(s: string): string =
  result = newStringOfCap(s.len)
  var upperNext = true
  for c in s:
    if c.isAlphaNumeric:
      let lower = c.toLowerAscii
      if upperNext:
        result.add lower.toUpperAscii
        upperNext = false
      else:
        result.add lower
    else:
      upperNext = true
  if result.len == 0:
    result = "Value"

proc toCamelCase(s: string): string =
  result = newStringOfCap(s.len)
  var seenWord = false
  var upperNext = false
  for c in s:
    if c.isAlphaNumeric:
      let lower = c.toLowerAscii
      if not seenWord:
        result.add lower
        seenWord = true
      elif upperNext:
        result.add lower.toUpperAscii
        upperNext = false
      else:
        result.add lower
    elif seenWord:
      upperNext = true
  if result.len == 0:
    result = "value"

proc argumentFieldName(name: string): string =
  result = toCamelCase(name)

proc commandEnumName(name: string): string =
  result = "cmd" & toPascalCase(name)

proc optionFieldName(option: OptionSpec): string =
  result = toCamelCase(option.longName)

proc optionEnumPrefix(option: OptionSpec): string =
  result = "cli" & toPascalCase(option.longName)

proc optionEnumTypeName(option: OptionSpec): string =
  result = "Cli" & toPascalCase(option.longName)

proc optionEnumNoneName(option: OptionSpec): string =
  result = option.optionEnumPrefix & "None"

proc optionEnumValueName(option: OptionSpec; choice: string): string =
  result = option.optionEnumPrefix & toPascalCase(choice)

proc positionalMode(spec: CliSpec): PositionalMode =
  if spec.commands.len == 0:
    if spec.argumentNames.len == 0:
      result = pmNone
    else:
      result = pmFlat
  else:
    result = pmSharedCommand
    for command in spec.commands:
      if command.argumentNames.len > 0:
        return pmInlineCommand

proc parseSectionHeader(line: string): SectionKind =
  if line.startsWith("Usage:"):
    result = skUsage
  elif line.startsWith("Arguments:"):
    result = skArguments
  elif line.startsWith("Commands:"):
    result = skCommands
  elif line.startsWith("Options:"):
    result = skOptions
  else:
    result = skNone

proc parseEntryHead(line: string): string =
  result = line.strip(trailing = false)
  var i = 0
  while i + 1 < result.len:
    if result[i] in Whitespace and result[i + 1] in Whitespace:
      return result.substr(0, i - 1)
    inc i

proc parseArgument(spec: var CliSpec; head: string) =
  let tokenLen = skipUntil(head, Whitespace)
  if tokenLen > 0:
    spec.argumentNames.add head.substr(0, tokenLen - 1)

proc parseCommand(spec: var CliSpec; head: string) =
  var name = ""
  var argumentNames: seq[string] = @[]
  for token in strutils.splitWhitespace(head):
    if name.len == 0:
      name = token
    else:
      argumentNames.add token
  if name.len > 0:
    spec.commands.add CommandSpec(name: name, argumentNames: argumentNames)

proc parseOption(spec: var CliSpec; head: string) =
  var option = OptionSpec(kind: fkBool)
  for rawPart in head.split(','):
    let part = rawPart.strip()
    if part.startsWith("--"):
      let valueAt = part.find('=')
      if valueAt >= 0:
        option.longName = part.substr(2, valueAt - 1)
        let placeholder = part.substr(valueAt + 1)
        if placeholder.contains('|'):
          option.kind = fkEnum
          option.choices = placeholder.split('|')
        else:
          option.kind = fkString
      else:
        option.longName = part.substr(2)
    elif part.startsWith("-"):
      option.shortName = part.substr(1)

  if option.longName.len == 0:
    option.longName = option.shortName
  if option.longName != "help":
    spec.options.add option

proc parseSectionEntry(spec: var CliSpec; section: SectionKind; line: string) =
  let head = parseEntryHead(line)
  if head.len > 0:
    case section
    of skArguments:
      parseArgument spec, head
    of skCommands:
      parseCommand spec, head
    of skOptions:
      parseOption spec, head
    of skUsage, skNone:
      discard

proc parseSpec(rawSpec: string): CliSpec =
  result = CliSpec()
  var currentSection = skNone
  for rawLine in rawSpec.splitLines():
    let header = parseSectionHeader(rawLine)
    if header != skNone:
      currentSection = header
    elif not rawLine.isEmptyOrWhitespace:
      parseSectionEntry result, currentSection, rawLine

proc extractSpecNode(n: NifCursor): NifCursor =
  result = callArgs(n)
  if result.exprKind == SufX:
    result = firstChild(result)

# (dot VALUE FIELD)
proc emitDotExpr(dest: var NifBuilder; valueName, fieldName: string)
    {.ensuresNif: addedExpr(dest).} =
  dest.withTree DotX, NoLineInfo:
    dest.addIdent(valueName)
    dest.addIdent(fieldName)

# (infix "<" NAME INT)
proc emitLtIntExpr(dest: var NifBuilder; name: string; value: int)
    {.ensuresNif: addedExpr(dest).} =
  dest.withTree InfixX, NoLineInfo:
    dest.addIdent("<")
    dest.addIdent(name)
    dest.addIntLit(value)

# (asgn (dot result FIELD) (dot VALUE SOURCE_FIELD))
proc emitAssignResultFieldFromField(dest: var NifBuilder; fieldName, valueName, sourceField: string)
    {.ensuresNif: addedStmt(dest).} =
  dest.withTree AsgnS, NoLineInfo:
    emitDotExpr dest, "result", fieldName
    emitDotExpr dest, valueName, sourceField

# (asgn (dot result FIELD) VALUE)
proc emitAssignResultFieldIdent(dest: var NifBuilder; fieldName, valueName: string)
    {.ensuresNif: addedStmt(dest).} =
  dest.withTree AsgnS, NoLineInfo:
    emitDotExpr dest, "result", fieldName
    dest.addIdent(valueName)

# (asgn (dot result FIELD) true)
proc emitAssignResultFieldTrue(dest: var NifBuilder; fieldName: string)
    {.ensuresNif: addedStmt(dest).} =
  dest.withTree AsgnS, NoLineInfo:
    emitDotExpr dest, "result", fieldName
    dest.addIdent("true")

# (asgn result (call CliOptions))
proc emitInitResultObject(dest: var NifBuilder)
    {.ensuresNif: addedStmt(dest).} =
  dest.withTree AsgnS, NoLineInfo:
    dest.addIdent("result")
    dest.withTree CallX, NoLineInfo:
      dest.addIdent("CliOptions")

# (fld FIELD . . TYPE .)
proc emitFieldDecl(dest: var NifBuilder; fieldName, typeName: string)
    {.ensuresNif: addedNested(dest).} =
  dest.withTree FldU, NoLineInfo:
    dest.addIdent(fieldName)
    dest.addEmptyNode2()
    dest.addIdent(typeName)
    dest.addEmptyNode()

# (efld FIELD . . . .)
proc emitEnumField(dest: var NifBuilder; fieldName: string)
    {.ensuresNif: addedNested(dest).} =
  dest.withTree EfldU, NoLineInfo:
    dest.addIdent(fieldName)
    dest.addEmptyNode4()

# (fld ARG . . string .)*
proc emitArgumentFieldDecls(dest: var NifBuilder; spec: CliSpec)
    {.ensuresNif: addedAny(dest).} =
  case spec.positionalMode()
  of pmNone:
    discard
  of pmFlat, pmSharedCommand:
    for argumentName in spec.argumentNames:
      emitFieldDecl dest, argumentFieldName(argumentName), "string"
  of pmInlineCommand:
    var emitted: seq[string] = @[]
    for command in spec.commands:
      for argumentName in command.argumentNames:
        if argumentName notin emitted:
          emitted.add argumentName
          emitFieldDecl dest, argumentFieldName(argumentName), "string"

template withOfIdent(dest: var NifBuilder; valueName: string; body: untyped) =
  dest.withTree OfU, NoLineInfo:
    dest.withTree RangesU, NoLineInfo:
      dest.addIdent(valueName)
    dest.withTree StmtsS, NoLineInfo:
      body

template withOfBindSym(dest: var NifBuilder; name: untyped; body: untyped) =
  dest.withTree OfU, NoLineInfo:
    dest.withTree RangesU, NoLineInfo:
      dest.bindSym(name)
    dest.withTree StmtsS, NoLineInfo:
      body

template withOfString(dest: var NifBuilder; value: string; body: untyped) =
  dest.withTree OfU, NoLineInfo:
    dest.withTree RangesU, NoLineInfo:
      dest.addStrLit(value)
    dest.withTree StmtsS, NoLineInfo:
      body

template withOfInt(dest: var NifBuilder; value: int; body: untyped) =
  dest.withTree OfU, NoLineInfo:
    dest.withTree RangesU, NoLineInfo:
      dest.addIntLit(value)
    dest.withTree StmtsS, NoLineInfo:
      body

# (type TYPE . . . (enum . (efld NONE . . . .) (efld VALUE . . . .)*))
proc emitEnumDecl(dest: var NifBuilder; typeName, noneName: string; enumNames: seq[string])
    {.ensuresNif: addedStmt(dest).} =
  dest.withTree TypeS, NoLineInfo:
    dest.addIdent(typeName)
    dest.addEmptyNode3()
    dest.withTree EnumT, NoLineInfo:
      dest.addEmptyNode()
      emitEnumField dest, noneName
      for enumName in enumNames:
        emitEnumField dest, enumName

# (type CliCommand . . . (enum . (efld cmdNone . . . .) (efld COMMAND . . . .)*))?
# (type ENUM_TYPE . . . (enum . (efld ENUM_NONE . . . .) (efld ENUM_VALUE . . . .)*))*
# (type CliOptions . . . (object . (fld ARG . . string .)* (fld command . . CliCommand .)? (fld OPTION . . OPTION_TYPE .)*))
proc emitOptionsDecl(dest: var NifBuilder; spec: CliSpec)
    {.ensuresNif: addedAny(dest).} =
  if spec.commands.len > 0:
    var commandNames: seq[string] = @[]
    for command in spec.commands:
      commandNames.add commandEnumName(command.name)
    emitEnumDecl dest, "CliCommand", "cmdNone", commandNames

  for option in spec.options:
    if option.kind == fkEnum:
      var enumNames: seq[string] = @[]
      for choice in option.choices:
        enumNames.add option.optionEnumValueName(choice)
      emitEnumDecl dest, option.optionEnumTypeName,
        option.optionEnumNoneName, enumNames

  dest.withTree TypeS, NoLineInfo:
    dest.addIdent("CliOptions")
    dest.addEmptyNode3()
    dest.withTree ObjectT, NoLineInfo:
      dest.addEmptyNode()
      emitArgumentFieldDecls dest, spec
      if spec.commands.len > 0:
        emitFieldDecl dest, "command", "CliCommand"
      for option in spec.options:
        case option.kind
        of fkString:
          emitFieldDecl dest, option.optionFieldName, "string"
        of fkBool:
          emitFieldDecl dest, option.optionFieldName, "bool"
        of fkEnum:
          emitFieldDecl dest, option.optionFieldName, option.optionEnumTypeName

# (var NAME . . int INT)
proc emitVarDeclInt(dest: var NifBuilder; name: string; value: int)
    {.ensuresNif: addedStmt(dest).} =
  dest.withTree VarS, NoLineInfo:
    dest.addIdent(name)
    dest.addEmptyNode2()
    dest.addIdent("int")
    dest.addIntLit(value)

# (call NAME ARG)
proc emitCallStmt1(dest: var NifBuilder; name, arg: string)
    {.ensuresNif: addedStmt(dest).} =
  dest.withTree CallS, NoLineInfo:
    dest.addIdent(name)
    dest.addIdent(arg)

# (call cliUnknownShortOption SPEC (dot VALUE KEY))
# (call cliUnknownLongOption SPEC (dot VALUE KEY))
proc emitUnknownOption(dest: var NifBuilder; rawSpec: string; shortOption: bool)
    {.ensuresNif: addedStmt(dest).} =
  dest.withTree CallS, NoLineInfo:
    if shortOption:
      dest.bindSym("cliUnknownShortOption")
    else:
      dest.bindSym("cliUnknownLongOption")
    dest.addStrLit(rawSpec)
    emitDotExpr dest, "p", "key"

# (case (dot p val)
#   (of CHOICE (stmts (asgn (dot result FIELD) ENUM_VALUE)))+
#   (else (stmts (call cliInvalidValue SPEC OPTION (dot p val)))))
proc emitEnumOptionBody(dest: var NifBuilder; rawSpec: string; option: OptionSpec)
    {.ensuresNif: addedStmt(dest).} =
  dest.withTree CaseS, NoLineInfo:
    emitDotExpr dest, "p", "val"
    for choice in option.choices:
      dest.withOfString choice:
        emitAssignResultFieldIdent dest, option.optionFieldName,
          option.optionEnumValueName(choice)
    dest.withTree ElseU, NoLineInfo:
      dest.withTree StmtsS, NoLineInfo:
        dest.withTree CallS, NoLineInfo:
          dest.bindSym("cliInvalidValue")
          dest.addStrLit(rawSpec)
          dest.addStrLit("--" & option.longName)
          emitDotExpr dest, "p", "val"

# (case (dot p key)
#   (of HELP (stmts (call cliExitHelp SPEC)))?
#   (of OPTION_KEY (stmts OPTION_BODY))*
#   (else (stmts (call cliUnknown{Short,Long}Option SPEC (dot p key))))
proc emitOptionDispatch(dest: var NifBuilder; rawSpec: string; spec: CliSpec; shortOption: bool)
    {.ensuresNif: addedStmt(dest).} =
  dest.withTree CaseS, NoLineInfo:
    emitDotExpr dest, "p", "key"
    if shortOption:
      dest.withOfString "h":
        dest.withTree CallS, NoLineInfo:
          dest.bindSym("cliExitHelp")
          dest.addStrLit(rawSpec)
    else:
      dest.withOfString "help":
        dest.withTree CallS, NoLineInfo:
          dest.bindSym("cliExitHelp")
          dest.addStrLit(rawSpec)

    for option in spec.options:
      let key = if shortOption: option.shortName else: option.longName
      if key.len > 0:
        dest.withOfString key:
          case option.kind
          of fkString:
            emitAssignResultFieldFromField dest, option.optionFieldName, "p", "val"
          of fkBool:
            emitAssignResultFieldTrue dest, option.optionFieldName
          of fkEnum:
            emitEnumOptionBody dest, rawSpec, option

    dest.withTree ElseU, NoLineInfo:
      dest.withTree StmtsS, NoLineInfo:
        emitUnknownOption dest, rawSpec, shortOption

# (case (dot p key)
#   (of COMMAND (stmts (asgn (dot result command) COMMAND_ENUM)))+
#   (else (stmts (call cliUnexpectedArgument SPEC (dot p key))))
proc emitCommandChoice(dest: var NifBuilder; rawSpec: string; spec: CliSpec)
    {.ensuresNif: addedStmt(dest).} =
  dest.withTree CaseS, NoLineInfo:
    emitDotExpr dest, "p", "key"
    for command in spec.commands:
      dest.withOfString command.name:
        emitAssignResultFieldIdent dest, "command", commandEnumName(command.name)
    dest.withTree ElseU, NoLineInfo:
      dest.withTree StmtsS, NoLineInfo:
        dest.withTree CallS, NoLineInfo:
          dest.bindSym("cliUnexpectedArgument")
          dest.addStrLit(rawSpec)
          emitDotExpr dest, "p", "key"

# (case argSlot
#   (of INT (stmts SLOT_BODY (cmd inc argSlot)))+
#   (else (stmts (call cliUnexpectedArgument SPEC (dot p key)))))
proc emitArgumentSlots(dest: var NifBuilder; rawSpec: string; argumentNames: seq[string];
    slotOffset: int) {.ensuresNif: addedStmt(dest).} =
  dest.withTree CaseS, NoLineInfo:
    dest.addIdent("argSlot")
    for i, argumentName in argumentNames:
      dest.withOfInt i + slotOffset:
        emitAssignResultFieldFromField dest,
          argumentFieldName(argumentName), "p", "key"
        emitCallStmt1 dest, "inc", "argSlot"
    dest.withTree ElseU, NoLineInfo:
      dest.withTree StmtsS, NoLineInfo:
        dest.withTree CallS, NoLineInfo:
          dest.bindSym("cliUnexpectedArgument")
          dest.addStrLit(rawSpec)
          emitDotExpr dest, "p", "key"

# (case (dot result command)
#   (of COMMAND_ENUM (stmts (call cliUnexpectedArgument SPEC (dot p key))))?
#   (of COMMAND_ENUM (stmts ARGUMENT_SLOTS))*
#   (else (stmts (call cliUnexpectedArgument SPEC (dot p key)))))
proc emitCommandArgumentDispatch(dest: var NifBuilder; rawSpec: string; spec: CliSpec)
    {.ensuresNif: addedStmt(dest).} =
  dest.withTree CaseS, NoLineInfo:
    emitDotExpr dest, "result", "command"
    for command in spec.commands:
      dest.withOfIdent commandEnumName(command.name):
        if command.argumentNames.len == 0:
          dest.withTree CallS, NoLineInfo:
            dest.bindSym("cliUnexpectedArgument")
            dest.addStrLit(rawSpec)
            emitDotExpr dest, "p", "key"
        else:
          emitArgumentSlots dest, rawSpec, command.argumentNames, 1
    dest.withTree ElseU, NoLineInfo:
      dest.withTree StmtsS, NoLineInfo:
        dest.withTree CallS, NoLineInfo:
          dest.bindSym("cliUnexpectedArgument")
          dest.addStrLit(rawSpec)
          emitDotExpr dest, "p", "key"

# (case argSlot ARGUMENT_SLOTS)?
# (if (elif (infix "<" argSlot 1) (stmts COMMAND_CHOICE (cmd inc argSlot)))
#   (else (stmts ARGUMENT_SLOTS_OR_COMMAND_ARGUMENT_DISPATCH)))?
proc emitArgumentDispatch(dest: var NifBuilder; rawSpec: string; spec: CliSpec)
    {.ensuresNif: addedAny(dest).} =
  let mode = spec.positionalMode()
  case mode
  of pmNone:
    discard
  of pmFlat:
    emitArgumentSlots dest, rawSpec, spec.argumentNames, 0
  of pmSharedCommand, pmInlineCommand:
    dest.withTree IfS, NoLineInfo:
      dest.withTree ElifU, NoLineInfo:
        emitLtIntExpr dest, "argSlot", 1
        dest.withTree StmtsS, NoLineInfo:
          emitCommandChoice dest, rawSpec, spec
          emitCallStmt1 dest, "inc", "argSlot"
      dest.withTree ElseU, NoLineInfo:
        dest.withTree StmtsS, NoLineInfo:
          if mode == pmSharedCommand:
            emitArgumentSlots dest, rawSpec, spec.argumentNames, 1
          else:
            emitCommandArgumentDispatch dest, rawSpec, spec

# (case (dot result command)
#   (of COMMAND_ENUM (stmts (if (elif (infix "<" argSlot REQUIRED) (stmts (call cliMissingArguments SPEC))))))+
#   (else (stmts (call cliMissingArguments SPEC))))
proc emitInlineCommandMissingCheck(dest: var NifBuilder; rawSpec: string; spec: CliSpec)
    {.ensuresNif: addedStmt(dest).} =
  dest.withTree CaseS, NoLineInfo:
    emitDotExpr dest, "result", "command"
    for command in spec.commands:
      let requiredSlots = 1 + command.argumentNames.len
      dest.withOfIdent commandEnumName(command.name):
        dest.withTree IfS, NoLineInfo:
          dest.withTree ElifU, NoLineInfo:
            emitLtIntExpr dest, "argSlot", requiredSlots
            dest.withTree StmtsS, NoLineInfo:
              dest.withTree CallS, NoLineInfo:
                dest.bindSym("cliMissingArguments")
                dest.addStrLit(rawSpec)
    dest.withTree ElseU, NoLineInfo:
      dest.withTree StmtsS, NoLineInfo:
        dest.withTree CallS, NoLineInfo:
          dest.bindSym("cliMissingArguments")
          dest.addStrLit(rawSpec)

# (if (elif (infix "<" argSlot REQUIRED) (stmts (call cliMissingArguments SPEC))))
proc emitSharedCommandMissingCheck(dest: var NifBuilder; rawSpec: string; argumentCount: int)
    {.ensuresNif: addedStmt(dest).} =
  dest.withTree IfS, NoLineInfo:
    dest.withTree ElifU, NoLineInfo:
      emitLtIntExpr dest, "argSlot", 1 + argumentCount
      dest.withTree StmtsS, NoLineInfo:
        dest.withTree CallS, NoLineInfo:
          dest.bindSym("cliMissingArguments")
          dest.addStrLit(rawSpec)

# (proc parseCli . . . (params) CliOptions . .
#   (stmts
#     (var p . . OptParser (call initOptParser))
#     (var argSlot . . int 0)
#     (asgn result (call CliOptions))
#     (while true
#       (stmts
#         (call next p)
#         (case (dot p kind)
#           (of cmdEnd (stmts (break .)))
#           (of cmdArgument (stmts ARG_DISPATCH))
#           (of cmdLongOption (stmts LONG_OPTION_DISPATCH))
#           (of cmdShortOption (stmts SHORT_OPTION_DISPATCH)))))
#     (if (elif (infix "<" argSlot INT) (stmts (call cliMissingArguments SPEC))))?
#     (if (elif (infix "==" (dot result command) VERSION_ENUM) (stmts (call cliExitVersion SPEC))))?))
proc emitParseProc(dest: var NifBuilder; rawSpec: string; spec: CliSpec)
    {.ensuresNif: addedStmt(dest).} =
  let mode = spec.positionalMode()
  dest.withTree ProcS, NoLineInfo:
    dest.addIdent("parseCli")
    dest.addEmptyNode3()
    dest.withTree ParamsU, NoLineInfo:
      discard
    dest.addIdent("CliOptions")
    dest.addEmptyNode2()
    dest.withTree StmtsS, NoLineInfo:
      dest.withTree VarS, NoLineInfo:
        dest.addIdent("p")
        dest.addEmptyNode2()
        dest.bindSym("OptParser")
        dest.withTree CallX, NoLineInfo:
          dest.bindSym("initOptParser")
      if mode != pmNone:
        emitVarDeclInt dest, "argSlot", 0
      emitInitResultObject dest

      dest.withTree WhileS, NoLineInfo:
        dest.addIdent("true")
        dest.withTree StmtsS, NoLineInfo:
          dest.withTree CallS, NoLineInfo:
            dest.bindSym("next")
            dest.addIdent("p")
          dest.withTree CaseS, NoLineInfo:
            emitDotExpr dest, "p", "kind"
            dest.withOfBindSym "cmdEnd":
              dest.withTree BreakS, NoLineInfo:
                dest.addEmptyNode()
            dest.withOfBindSym "cmdArgument":
              if mode != pmNone:
                emitArgumentDispatch dest, rawSpec, spec
              else:
                dest.withTree CallS, NoLineInfo:
                  dest.bindSym("cliUnexpectedArgument")
                  dest.addStrLit(rawSpec)
                  emitDotExpr dest, "p", "key"
            dest.withOfBindSym "cmdLongOption":
              emitOptionDispatch dest, rawSpec, spec, false
            dest.withOfBindSym "cmdShortOption":
              emitOptionDispatch dest, rawSpec, spec, true

      if spec.commands.len > 0:
        for command in spec.commands:
          if command.name == "version":
            dest.withTree IfS, NoLineInfo:
              dest.withTree ElifU, NoLineInfo:
                dest.withTree InfixX, NoLineInfo:
                  dest.addIdent("==")
                  emitDotExpr dest, "result", "command"
                  dest.addIdent(commandEnumName(command.name))
                dest.withTree StmtsS, NoLineInfo:
                  dest.withTree CallS, NoLineInfo:
                    dest.bindSym("cliExitVersion")
                    dest.addStrLit(rawSpec)
            break

      case mode
      of pmNone:
        discard
      of pmFlat:
        dest.withTree IfS, NoLineInfo:
          dest.withTree ElifU, NoLineInfo:
            emitLtIntExpr dest, "argSlot", spec.argumentNames.len
            dest.withTree StmtsS, NoLineInfo:
              dest.withTree CallS, NoLineInfo:
                dest.bindSym("cliMissingArguments")
                dest.addStrLit(rawSpec)
      of pmSharedCommand:
        emitSharedCommandMissingCheck dest, rawSpec, spec.argumentNames.len
      of pmInlineCommand:
        emitInlineCommandMissingCheck dest, rawSpec, spec

proc generate(rawSpec: string; spec: CliSpec; info: LineInfo): NifBuilder =
  result = createTree()
  result.withTree StmtsS, info:
    result.withTree BlockS, info:
      result.addEmptyNode()
      result.withTree StmtsS, info:
        emitOptionsDecl result, spec
        emitParseProc result, rawSpec, spec
        result.withTree CallS, NoLineInfo:
          result.addIdent("parseCli")

let root = loadPluginInput()
let specNode = extractSpecNode(root)
if specNode.kind == StrLit:
  let rawSpec = specNode.stringValue
  let spec = parseSpec(rawSpec)
  saveTree generate(rawSpec, spec, root.info)
else:
  saveTree errorTree("cliapp expects a string literal", specNode)
