(* Smbt, an SML build tool
 *  Copyright (c) 2012 Filip Sieczkowski & Gian Perrone
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both the copyright notice and this permission notice and warranty
 * disclaimer appear in supporting documentation, and that the name of
 * the above copyright holders, or their entities, not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.
 * 
 * The above copyright holders disclaim all warranties with regard to
 * this software, including all implied warranties of merchantability and
 * fitness. In no event shall the above copyright holders be liable for
 * any special, indirect or consequential damages or any damages
 * whatsoever resulting from loss of use, data or profits, whether in an
 * action of contract, negligence or other tortious action, arising out
 * of or in connection with the use or performance of this software.
 *
*)

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
  val nestedComments  = false

  (* parsers for first and subsequent letters of identifiers *)
  val identStart      = letter
  val identLetter     = alphaNum

  (* parsers for first and subsequent chars of operators *)
  val opStart         = char #"="
  val opLetter        = fail "Multi-character operators not supported." : scanner

  (* reserved keywords and operators *)
  val reservedNames   = ["smackspec", "specpath", "target", "option", "ffi",
                         "smb", "sources", "pkg", "val", "lnkopt", "cflags",
                         "header", "pre", "post", "hooks", "end"]
  val reservedOpNames = ["="] : string list

  (* is the language case sensitive *)
  val caseSensitive   = true

end

structure Pre_AST =
struct

  datatype 'a ffid = FFIFile of string * 'a | FFILnk of string * 'a
		   | FFIFlgs of string * 'a | FFIHdr of string * 'a
  datatype 'a pkg  = SmackPKG of string * string * string option * 'a
		   | LocalPKG of string * string * 'a
  datatype 'a dec  = Opt of string * string * 'a | FFI of 'a ffid list * 'a
		   | Src of string list * 'a | Pkg of 'a pkg * 'a
                   | Macro of string * string * 'a | Target of string * 'a dec list * 'a
                   | PreHook of string list * 'a | PostHook of string list * 'a
  datatype spec = SLnk of string

end

structure Parser :>
sig

    exception ParseError of string

    (** Parse the contents of a file as an Smbt specification **)
    val parseFile : string -> Pre_AST.spec option * Pos.t Pre_AST.dec list

    (** Parse a string as an Smbt specification **)
    val parseStr  : string -> Pre_AST.spec option * Pos.t Pre_AST.dec list

end =
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

  exception ParseError of string

  fun flat3' ((a, b), c) = (a, b, c)
  fun flat4' ((a, b, c), d) = (a, b, c, d)

  val name = TP.identifier
  val version = TP.lexeme (char #"v" && repeat1 (digit <|> char #".") wth String.implode o op::)
  val hook = not (TP.reserved "end") >> TP.lexeme (repeat1 (any suchthat (fn c => c <> #"\n")) wth String.implode)
  val nwsstr = TP.lexeme (repeat1 (satisfy (Char.isGraph)) wth String.implode)
    suchthat (fn x => Bool.not (List.exists (fn y => x = y) LexDef.reservedNames))
  val pqstring = TP.stringLiteral <|> nwsstr
  val macro = !!(TP.reserved "val" >> name << TP.reservedOp "=" && pqstring) wth flat3'
  val pkg = !! (TP.reserved "pkg" >> pqstring && version && opt pqstring wth flat3) wth SmackPKG o flat4'
        <|> !! (TP.reserved "smb" >> pqstring && pqstring) wth LocalPKG o flat3'
  val ffidec = !! (TP.reserved "lnkopt" >> pqstring) wth FFILnk <|> !! (TP.reserved "cflags" >> pqstring) wth FFIFlgs
           <|> !! (TP.reserved "header" >> pqstring) wth FFIHdr <|> !! pqstring wth FFIFile
  val ffi = TP.reserved "ffi" >> repeat ffidec << TP.reserved "end"
  val sources = TP.reserved "sources" >> repeat pqstring << TP.reserved "end"
  val option = !! (TP.reserved "option" >> name << TP.reservedOp "=" && pqstring) wth flat3'
  val prehooks = TP.reserved "pre" >> TP.reserved "hooks" >> repeat hook << TP.reserved "end"
  val posthooks = TP.reserved "post" >> TP.reserved "hooks" >> repeat hook << TP.reserved "end"
  fun dec' () = option wth Opt <|> !! ffi wth FFI <|> !! sources wth Src <|> !! pkg wth Pkg <|> macro wth Macro
            <|> ($ target') wth Target <|> !! prehooks wth PreHook <|> !! posthooks wth PostHook
  and target' () = !! (TP.reserved "target" >> name && repeat ($ dec') << TP.reserved "end") wth flat3'
  val target = $target' wth Target
  val dec = $dec'
  val sms = TP.reserved "specpath" >> pqstring wth SLnk (* TODO: fill in parser for internal smackspecs *)
  val smb = opt sms && repeat target

  fun parse p s = sum (fn s => raise ParseError s) (fn x => x) (parseString (TP.whiteSpace >> p << eos) s)
  fun parseStr s = parse smb s
  fun parseFile fileName =
      let fun isEol s = (case Stream.front s of Stream.Cons (#"\n", _) => true | _ => false)
          val is = Stream.fromTextInstream (TextIO.openIn fileName)
          val cs = CoordinatedStream.coordinate isEol (Coord.init fileName) is
      in sum (fn s => raise ParseError s) (fn x => x) (CharParser.parseChars (TP.whiteSpace >> smb << eos) cs)
      end

end
