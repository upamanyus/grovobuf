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

method genCoder(s:Type, t:var Translator) : (Coders, string, string) {.base.} =
  quit "en/decoders unimpl for " & s.type.name

proc getEncoder*(t:var Translator, s:Type) : string
proc getDecoder(t:var Translator, s:Type) : string

method genCoder(s:StructType, t:var Translator) : (Coders, string, string) =
  var encoderName = &"encode_{s.name}"
  var enimpl = &"func () {encoderName}(data []byte, s *{s.name}) {{\n"
  for (fname, ftype) in s.fields:
    enimpl = enimpl & (&"\tdata = {t.getEncoder(ftype)}(data, s.{fname})\n")
  enimpl = enimpl & "}\n"

  var decoderName = &"decode_{s.name}"
  var deimpl = &"func () {decoderName}(data []byte, s *{s.name}) {{\n"
  for (fname, ftype) in s.fields:
    deimpl = deimpl & (&"\tdata = {t.getDecoder(ftype)}(data, &s.{fname})\n")
  deimpl = deimpl & "}\n"

  return (Coders(encoder:encoderName, decoder:decoderName), enimpl, deimpl)

proc addCoder(t:var Translator, s:Type) : Coders =
  var (c, enimpl, deimpl) = s.genCoder(t)
  if t.impls.hasKeyOrPut(c.encoder, enimpl):
    quit "bug: duplicate encoder func name"
  if t.impls.hasKeyOrPut(c.decoder, deimpl):
    quit "bug: duplicate decoder func name"
  return c

# returns name of encoder, after ensuring that it has an implementation
proc getEncoder*(t:var Translator, s:Type) : string =
  # FIXME: detect type recursion
  if t.coders.hasKey($s):
    return t.coders[$s].encoder
  else:
    return t.addCoder(s).encoder

# returns name of encoder, after ensuring that it has an implementation
proc getDecoder(t:var Translator, s:Type) : string =
  # FIXME: detect type recursion
  if t.coders.hasKey($s):
    return t.coders[$s].decoder
  else:
    return t.addCoder(s).decoder
