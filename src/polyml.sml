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

structure PolyMLCompiler :> COMPILER =
struct
    open CompilerUtil 

    val name = "PolyML"

    fun compile' (srcs,ffisrcs,lnkopts,cflags,hdr,opts) output =
        let
            val _ = print " - Invoking PolyML\n"

            val polyml = case selectOpt opts "polyml" of
                SOME t => t
              | NONE => "poly"

            val cc = case selectOpt opts "cc" of
                SOME t => t
              | NONE => "cc"

            val dir = tempdir ()
            val polyGoFile = dir ^ "/smbt-poly-main.sml"

            val objFile' = case selectOpt opts "objectFile" of
                  SOME t => t
                | NONE => "smbt-poly-export"

            val objFile = 
                    OS.Path.mkAbsolute {path=objFile', 
                        relativeTo = OS.FileSys.fullPath (OS.Path.dir output)}

            val exportFn = case selectOpt opts "exportFn" of 
                SOME t => t
              | NONE => raise Fail "PolyML Output requires 'exportFn' directive.\n"

            val fp = TextIO.openOut polyGoFile
            val _ = TextIO.output (fp,
                        String.concatWith "\n" (map (fn f => "use \"" ^ absolutePath f ^ "\";") (List.rev srcs)) ^ "\n" ^
                        "PolyML.export (\"" ^ objFile ^ "\", " ^ exportFn ^ ");\n"
                        )
            val _ = TextIO.closeOut fp
            
            val cmd = polyml ^ " < " ^ polyGoFile
            val _ = exec cmd
            
            val _ = print (" - Object file: " ^ statFile (objFile ^ ".o") ^ "\n")

            val ccmd = [cc] @ cflags @ ["-o", output, objFile ^ ".o", "-lpolymain -lpolyml"] @ lnkopts
            val _ = print (" - Linking\n")
            val _ = exec (String.concatWith " " ccmd)

            val _ = print (" - Output: " ^ statFile output ^ "\n")
        in
            ()
        end


    fun compile (c as (srcs,ffisrcs,lnkopts,cflags,hdr,opts)) =
        case selectOpt opts "output" of
                SOME output => compile' c output
              | NONE => raise Fail ("Compiler invoked with no output.\n")

end



