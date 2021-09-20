import strutils
import strformat
import terminal
import ast

method `$`*(e:Expr) : string {.base.} =
  quit "bug: must implement tostring for Expr"

method `$`(e:Identifier) : string =
  $e.t

method `$`(n:Type) : string =
  quit "bug: must implement tostring for Type"

method `$`(n:NamedType) : string =
  $n.name

method `$`(n:SliceType) : string =
  "[]" & $n.elt

method `$`(n:StructType) : string =
  $n.name

proc error(msg:string) =
  stderr.setForegroundColor(ForegroundColor.fgRed)
  stderr.writeLine(msg)
  stderr.setForegroundColor(ForegroundColor.fgDefault)

method error(t:Token, msg:string) =
  error(&"{t.filename}({t.startLine}:{t.startCh}): {msg}")

method errorWithTok(t:Token, msg:string) =
  error(&"{t.filename}({t.startLine}:{t.startCh}): {t.src[t.b..t.e]} {msg}")

type
  Tokenizer = object
    toks: seq[Token]
    i : int
    n : int
    src : ref string # XXX: maybe ref string
    line : int
    lineOffset : int
    filename : string

proc nextChar(t:var Tokenizer) =
  if t.src[t.i] == '\n':
    inc(t.line)
    t.lineOffset = 0
    inc(t.i)
  else:
    inc(t.lineOffset)
    inc(t.i)

proc tokenizeIdent(t:var Tokenizer) =
  var startIdx = t.i
  var startLine = t.line
  var startCh = t.lineOffset
  inc(t.i)
  while t.i < t.n:
    case t.src[t.i]
    of '_', 'a'..'z', 'A'..'Z', '0'..'9':
      t.nextChar()
    else:
      break

  var s = t.src[startIdx..<t.i]
  var kind:TokenKind

  case s
  of "struct":
    kind = TokenKind.Struct
  else:
    kind = TokenKind.Ident

  t.toks.add(Token(b:startIdx, e:t.i, startLine:startLine, startCh:startCh, filename:t.filename, src:t.src, kind:kind))

proc tokenizeSpecial(t:var Tokenizer) =
  var k:TokenKind
  case t.src[t.i]
  of '{':
    k = TokenKind.LBrace
  of '}':
    k = TokenKind.RBrace
  of ':':
    k = TokenKind.Colon
  of ';':
    k = TokenKind.Semi
  of '[':
    k = TokenKind.LBracket
  of ']':
    k = TokenKind.RBracket
  else:
    discard

  t.toks.add(Token(b:t.i, e:t.i+1, startLine:t.line, startCh:t.lineOffset, filename:t.filename, src:t.src, kind:k))
  t.nextChar()

proc tokenizeComment(t:var Tokenizer) =
  while t.i < t.n:
    case t.src[t.i]
    of '\n', '\r':
       break
    else:
      t.nextChar()

proc tokenize(t:var Tokenizer) =
  while t.i < t.n:
    var c = t.src[t.i]
    case c
    of '#':
      t.tokenizeComment()
    of ' ', '\n', '\t', '\r':
      t.nextChar()
    of '{', '}', '[', ']', ':', ';':
      t.tokenizeSpecial()
    of '_', 'a'..'z', 'A'..'Z':
      t.tokenizeIdent()
    else:
      error(&"{t.filename}: unrecognized character {c}")

proc tokenizeGrovoBuf(src:ref string) : seq[Token] =
  var t = Tokenizer(toks:newSeq[Token](), i:0, n: src[].len(), src:src, line:1, lineOffset:0, filename:"blah.gb")
  t.tokenize()
  return t.toks

type
  Parser = object
    toks:seq[Token]
    i:int
    n:int
    filename:string

proc consume(p:var Parser) : Token =
  result = p.toks[p.i]
  inc(p.i)

proc isKind(p:var Parser, k:TokenKind) : bool =
  if p.i >= p.n:
    return false
  return p.toks[p.i].kind == k

proc error(p:var Parser, msg:string) =
  p.toks[p.i].error(msg)

proc expect(p:var Parser, k:TokenKind, msg:string) : Token =
  if p.i >= p.n:
    error(&"{p.filename}: file ended unexpectedly {msg}")
  result = p.toks[p.i]
  if p.toks[p.i].kind != k:
    p.error(msg)
  else: # XXX: Assume that the user forgot the token if the wrong type shows up
    inc(p.i)

proc parseIdentifier(p:var Parser): Identifier =
  var t = p.expect(TokenKind.Ident, "expected identifier")
  return Identifier(t:t)

proc parseType(p:var Parser) : Type =
  if p.i >= p.n:
    error(&"{p.filename}: file ended unexpectedly, expected type")
  case p.toks[p.i].kind
  of TokenKind.LBracket:
    discard p.consume()
    discard p.expect(TokenKind.RBracket, "expecetd ]")
    var elt = p.parseType()
    return SliceType(elt:elt)
  of TokenKind.Ident:
    return NamedType(name:p.parseIdentifier())
  else:
    p.error("unexpected token")

proc parseStructType(p:var Parser): StructType =
  assert p.isKind(TokenKind.Struct), "expected structDefn"
  discard p.expect(TokenKind.Struct, "expected keyword \"struct\"")
  var name = p.expect(TokenKind.Ident, "expected name of struct")
  discard p.expect(TokenKind.LBrace, "expected {")
  var fields = newSeq[(Identifier,Type)]()

  while p.isKind(TokenKind.Ident):
    var fieldIdent = p.parseIdentifier()
    discard p.expect(TokenKind.Colon, "expected ':' for struct field type")
    var ty = p.parseType()
    fields.add((fieldIdent, ty))
    discard p.expect(TokenKind.Semi, "expected ;")

  result = StructType(name:name, fields:fields)
  discard p.expect(TokenKind.RBrace, "expected }")

method prettyPrint*(e:Expr) : string {.base.} =
  quit "bug: must override this prettyPrint"

method prettyPrint(e:StructType) : string =
  result = ""
  result = result & "struct " & $e.name & " {\n"
  for field in e.fields:
    result = result & "  " & $field[0].t & ":" & $field[1] & "\n"
  result = result & "}"

proc parseAliasDefn(p:Parser): Expr =
  discard

proc parse(p:var Parser): StructType =
  while p.i < p.n:
    case p.toks[p.i].kind
    of TokenKind.Struct:
      return p.parseStructType()
      break
    else:
      p.error("unexpected token")
      discard p.consume()

proc parseGrovoBuf*(src:ref string) : StructType =
  var x = tokenizeGrovoBuf(src)
  var p = Parser(toks:x, i:0, n:x.len(), filename:"blah.gb")
  return p.parse()
