import parser
import translator

proc main() =
  var src = new(string)
  src[] = readFile("sample.gb").string
  let e = parseGrovoBuf(src)
  var t = newTranslator()

  discard t.getEncoder(e)
  echo t

main()
