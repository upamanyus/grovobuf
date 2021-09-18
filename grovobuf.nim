import strutils
import strformat
import terminal

type
  TokenKind = enum
    Ident, Struct, Alias, Enum, LBrace, RBrace

  Token = object
    b: int
    e: int
    startLine: int
    startCh: int
    filename: string # XXX: maybe ref string?
    src: ref string
    kind: TokenKind

type
  Expr = ref object of RootObj
  Type = ref object of Expr

  Defn = ref object of Expr
  Identifier = ref object of Expr
    t:Token

  NamedType = ref object of Type
    name:Identifier

  StructDefn = ref object of Defn
    name:Token
    fields:seq[(Identifier,Type)]

method Error(t:Token, msg:string) =
  stderr.setForegroundColor(ForegroundColor.fgRed)
  stderr.writeLine(&"{t.filename}({t.startLine}:{t.startCh}): {msg}")
  stderr.setForegroundColor(ForegroundColor.fgDefault)

method ErrorWithTok(t:Token, msg:string) =
  stderr.writeLine(&"{t.filename}({t.startLine}:{t.startCh}): {t.src[t.b..t.e]} {msg}")

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
      stderr.setForegroundColor(ForegroundColor.fgRed)
      stderr.writeLine(&"{t.filename}: unrecognized character {c}")
      stderr.setForegroundColor(ForegroundColor.fgDefault)

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

proc expect(p:Parser, k:TokenKind, msg:string) : Token =
  if p.i >= p.n:
    stderr.setForegroundColor(ForegroundColor.fgRed)
    stderr.writeLine(&"{p.filename}: file ended unexpectedlyl {msg}")
    stderr.setForegroundColor(ForegroundColor.fgDefault)
  if p.toks[p.i].kind != k:
    p.toks[p.i].Error(msg)
  return p.toks[p.i]

proc parseStructDefn(p:Parser): Expr =
  assert p.toks[p.i].kind == TokenKind.Struct, "expected structDefn"
  var name = p.expect(TokenKind.Ident, "expected name of struct")
  discard p.expect(TokenKind.LBrace, "expected {")
  discard p.expect(TokenKind.RBrace, "expected }")

# proc parseAliasDefn(p:Parser): Expr =
# proc parseField(p:Parser): Expr =
# proc parseIdentifier(p:Parser): Expr =

proc parse(p:Parser): Expr =
  while p.i < p.n:
    case p.toks[p.i].kind
    of TokenKind.Struct:
      return p.parseStructDefn()
      break
    else:
      p.toks[p.i].Error("unexpected token")

proc main() =
  var src = new(string)
  src[] = readFile("sample.gb").string
  let x = tokenizeGrovoBuf(src)
  echo "test"
  echo x
  # x[3].ErrorWithTok("this is just a test")
  # echo repr(parseGrovoBuf(src))

main()
