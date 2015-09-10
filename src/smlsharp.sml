(* vim: set et ts=4: *)
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

structure SMLSharpCompiler :> COMPILER =
struct
    open CompilerUtil 

    val name = "Smlsharp"

    fun compileOne cmd src out cflags = let
        val command = (String.concatWith " " [cmd, cflags, "-c", src, "-o", out])
        val () = print (command ^ "\n")
    in
        exec command
    end

    fun outFileOf src = if String.isSuffix ".sml" src
                        then (String.substring(src, 0,String.size src - 4)) ^ ".o"
                        else if String.isSuffix ".c" src
                        then (String.substring(src, 0,String.size src - 2)) ^ ".o"
                        else raise Fail (src ^ " isn't sml file")

    fun compile' (srcs,ffisrcs,lnkopts,cflags,hdr,opts) output =
        let
            val _ = print " - Invoking Smlsharp\n"
            val lnkopts = String.concatWith " " lnkopts
            val cflags  = String.concatWith " " cflags
            val smlsharp = case selectOpt opts "smlsharp" of
                SOME t => t
              | NONE => "smlsharp"

            val cc = case selectOpt opts "cc" of
                SOME t => t
              | NONE => "cc"

            val dir = tempdir ()

            fun compileWith cmd sources = List.app (fn src => let
                                  val outFile = outFileOf src
                                  val () = print (" - Object file: " ^ statFile outFile ^ "\n")
                              in
                                  compileOne cmd src (outFileOf src) cflags
                              end) sources
            val _ = compileWith smlsharp srcs
            val _ = compileWith cc ffisrcs
            
            val _ = print (" - Compiling\n")
            val _ = print (" - Linking\n")
            val objs = String.concatWith " " (List.map outFileOf (srcs @ ffisrcs))
            val _ = exec (String.concatWith " " [smlsharp, cflags, "-o", output, objs ,cflags, lnkopts])

            val _ = print (" - Output: " ^ statFile output ^ "\n")
        in
            ()
        end


    fun compile (c as (srcs,ffisrcs,lnkopts,cflags,hdr,opts)) =
        case selectOpt opts "output" of
                SOME output => compile' c output
              | NONE => raise Fail ("Compiler invoked with no output.\n")


    fun interactive (srcs,ffisrcs,lnkopts,cflags,hdr,opts) = raise Fail "interactive session isn't supported under SML#"

end



