structure LexDef :> LANGUAGE_DEF =
struct

  open ParserCombinators
  open CharParser

  type scanner = char charParser

  (* multiline comment start/end sequence *)
  val commentStart    = SOME "(*"
  val commentEnd      = SOME "*)"

  (* single line comment start *)
  val commentLine     = NONE

  (* do the multiline comments support nesting *)
  val nestedComments  = true

  (* parsers for first and subsequent letters of identifiers *)
  val identStart      = letter
  val identLetter     = alphaNum

  (* parsers for first and subsequent chars of operators *)
  val opStart         = char #"="
  val opLetter        = fail "Multi-charachter operators not supported." : scanner

  (* reserved keywords and operators *)
  val reservedNames   = ["smackspec", "specpath", "target", "option", "ffi", "smb",
                         "sources", "pkg", "val", "lnkopt", "cflags", "header", "end"]
  val reservedOpNames = ["="] : string list

  (* is the language case sensitive *)
  val caseSensitive   = true

end

structure Pre_AST =
struct

  datatype ffid = FFIFile of string | FFILnk of string | FFIFlgs of string | FFIHdr of string
  datatype pkg  = SmackPKG of string * string | LocalPKG of string * string
  datatype dec  = Opt of string * string | FFI of ffid list | Src of string list | Pkg of pkg
                | Macro of string * string | Target of string * dec list
  datatype spec = SLnk of string

end

structure Parser =
struct

  structure TP = TokenParser (LexDef)
  open Sum
  open ParserCombinators
  open CharParser
  open Pre_AST
  infixr 4 << >>
  infixr 3 &&
  infix  2 -- ##
  infix  2 wth suchthat return guard when
  infixr 1 || <|> ??

  val name = TP.identifier
  val version = TP.lexeme (char #"v" && repeat1 (digit <|> char #".") wth String.implode o op::)
  val nwsstr = TP.lexeme (repeat1 (satisfy (Char.isGraph)) wth String.implode)
     suchthat (fn x => Bool.not (List.exists (fn y => x = y) LexDef.reservedNames))
  val pqstring = TP.stringLiteral <|> nwsstr
  val macro = TP.reserved "val" >> name << TP.reservedOp "=" && pqstring
  val pkg = TP.reserved "pkg" >> name && version wth SmackPKG
          <|> TP.reserved "smb" >> pqstring && name wth LocalPKG
  val ffidec = TP.reserved "lnkopt" >> name wth FFILnk <|> TP.reserved "cflags" >> pqstring wth FFIFlgs
          <|> TP.reserved "header" >> pqstring wth FFIHdr <|> pqstring wth FFIFile
  val ffi = TP.reserved "ffi" >> repeat ffidec << TP.reserved "end"
  val sources = TP.reserved "sources" >> repeat pqstring << TP.reserved "end"
  val option = TP.reserved "option" >> name << TP.reservedOp "=" && pqstring
  fun dec' () = option wth Opt <|> ffi wth FFI <|> sources wth Src <|> pkg wth Pkg <|> macro wth Macro <|> ($ target') wth Target
  and target' () = TP.reserved "target" >> name && repeat ($ dec') << TP.reserved "end"
  val target = $target'
  val dec = $dec'
  val sms = TP.reserved "specpath" >> pqstring wth SLnk (* TODO: fill in parser for internal smackspecs *)
  val smb = sms && repeat target

  fun parse p s = sum (fn s => raise Fail s) (fn x => x) (parseString p s)
  fun parseFileSuc fileName =
      let fun isEol s = (case Stream.front s of Stream.Cons (#"\n", _) => true | _ => false)
          val is = Stream.fromTextInstream (TextIO.openIn fileName)
          val cs = CoordinatedStream.coordinate isEol (Coord.init fileName) is
      in outR (CharParser.parseChars (smb << eos) cs)
      end

end
