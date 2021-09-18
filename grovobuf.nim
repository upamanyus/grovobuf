import strutils
import strformat

type
  Expr = ref object of RootObj

type
  Token = object
    b: int
    e: int
    startLine: int
    startCh: int
    filename: string # XXX: maybe ref string?
    src: string


method Error(t:Token, msg:string) =
  stderr.writeLine(&"{t.filename}({t.startLine}:{t.startCh}): {msg}\n" )

method ErrorWithTok(t:Token, msg:string) =
  stderr.writeLine(&"{t.filename}({t.startLine}:{t.startCh}): {t.src[t.b..t.e]} {msg}\n" )


type
  Tokenizer = object
    toks: seq[Token]
    i : int
    n : int
    src : string # XXX: maybe ref string
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
    of '_', 'a'..'z', 'A'..'Z':
      discard
    else:
      break
    t.nextChar()
  t.toks.add(Token(b:startIdx, e:t.i, startLine:startLine, startCh:startCh, filename:t.filename, src:t.src))

proc tokenize(t:var Tokenizer) =
  while t.i < t.n:
    var c = t.src[t.i]
    case c
    of ' ', '\n', '\t', '\r':
      discard
    of '{', '}', '[', ']', ':', ';':
      t.toks.add(Token(b:t.i, e:t.i+1, startLine:t.line, startCh:t.lineOffset, filename:t.filename, src:t.src))
    of '_', 'a'..'z', 'A'..'Z':
      t.tokenizeIdent()
    else:
      discard # FIXME: error
    t.nextChar()

proc tokenizeGrovoBuf(src:string) : seq[Token] =
  var t = Tokenizer(toks:newSeq[Token](), i:0, n:src.len, src:src, line:0, lineOffset:0, filename:"blah.gb")
  t.tokenize()
  return t.toks

# proc parseGrovoBuf(src:string) : Expr

proc main() =
  let src = readfile("sample.gb")
  let x = tokenizeGrovoBuf(src)
  echo "test"
  x[3].ErrorWithTok("this is just a test")
  # echo repr(parseGrovoBuf(src))

main()
