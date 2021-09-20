import parser

proc main() =
  var src = new(string)
  src[] = readFile("sample.gb").string
  let e = parseGrovoBuf(src)
  echo repr(e)
  echo prettyPrint(e)

main()
