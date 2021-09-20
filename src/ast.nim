type
  TokenKind* = enum
    Ident, Struct, Alias, Enum, LBrace, RBrace, Colon, Semi, LBracket, RBracket
    # Bracket == [], Brace = []

  Token* = object
    b*: int
    e*: int
    startLine*: int
    startCh*: int
    filename*: string # XXX: maybe ref string?
    src*: ref string
    kind*: TokenKind

proc `$`*(t:Token) : string =
  t.src[t.b..<t.e]

type
  Expr* = ref object of RootObj # FIXME: rename to "AstNode" or some such
  Type* = ref object of Expr

  Identifier* = ref object of Expr
    t*:Token

  NamedType* = ref object of Type
    name*:Identifier

  SliceType* = ref object of Type
    elt*:Type

  StructType* = ref object of Type
    name*:Token
    fields*:seq[(Identifier,Type)]
