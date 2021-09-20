import ast
import strformat
import parser
import tables
import typetraits

type
  Coders = object
    encoder:string
    decoder:string

  Translator = object
    coders:Table[string, Coders]
    impls:Table[string, string] # Maps Go function name -> its implementation
    # FIXME: topological sort deps?


proc newTranslator*() : Translator =
  result = Translator(coders:initTable[string,Coders](), impls:initTable[string,string]())
  result.coders["uint64"] = Coders(encoder:"encode_uint64", decoder:"decode_uint64")

method addCoders(t:Translator, s:Type) : Coders {.base.} =
  quit "en/decoders unimpl for " & s.type.name

proc getEncoder*(t:var Translator, s:Type) : string
proc getDecoder(t:var Translator, s:Type) : string

proc addCodersStruct(t:var Translator, s:StructType) : Coders

method addCoders(t:var Translator, s:Type) : Coders =
  if s of StructType:
    t.addCodersStruct(cast[StructType](s))
  elif s of NamedType:
    quit "en/decoders not impl for named `" & $cast[NamedType](s).name & "`"
  elif s of SliceType:
    quit "en/decoders not impl for slice"
  else:
    quit "en/decoders only impl for Struct type"

proc addCodersStruct(t:var Translator, s:StructType) : Coders =
  var encoderName = &"encode_{s.name}"
  var enimpl = &"func () {encoderName}(data []byte, s *{s.name}) {{\n"
  for (fname, ftype) in s.fields:
    enimpl = enimpl & (&"\tdata = {t.getEncoder(ftype)}(data, s.{fname})\n")

  enimpl = enimpl & "}\n"
  if t.impls.hasKeyOrPut(encoderName, enimpl):
    quit "bug: duplicate encoder func name"

  var decoderName = &"decode_{s.name}"
  var deimpl = &"func () {decoderName}(data []byte, s *{s.name}) {{\n"
  for (fname, ftype) in s.fields:
    deimpl = deimpl & (&"\tdata = {t.getDecoder(ftype)}(data, &s.{fname})\n")

  deimpl = deimpl & "}\n"
  if t.impls.hasKeyOrPut(decoderName, deimpl):
    quit "bug: duplicate decoder func name"
  return Coders(encoder:encoderName, decoder:decoderName)

# returns name of encoder, after ensuring that it has an implementation
proc getEncoder*(t:var Translator, s:Type) : string =
  # FIXME: detect type recursion
  if t.coders.hasKey($s):
    return t.coders[$s].encoder
  else:
    return t.addCoders(s).encoder

# returns name of encoder, after ensuring that it has an implementation
proc getDecoder(t:var Translator, s:Type) : string =
  # FIXME: detect type recursion
  if t.coders.hasKey($s):
    return t.coders[$s].decoder
  else:
    return t.addCoders(s).decoder
