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

structure MLtonCompiler :> COMPILER =
struct
    open ToolOpt

    datatype t = M of {
        sources : string list,
        ffisources : string list,
        linkopts : string list,
        cflags : string list,
        header : string option,
        output : string option,
        mlbFile : string option
    }

    val name = "MLton"

    val empty = M {sources = [],
                   ffisources = [],
                   linkopts = [],
                   cflags = [],
                   header = NONE,
                   output = NONE,
                   mlbFile = NONE}

    fun addSources (M c) l = 
      M {sources = #sources c @ l,
         ffisources = #ffisources c, 
         linkopts = #linkopts c, 
         cflags = #cflags c, 
         header = #header c, 
         output = #output c, 
         mlbFile = #mlbFile c}

    fun addFFISources (M c) l =
      M {sources = #sources c,
         ffisources = #ffisources c @ l, 
         linkopts = #linkopts c, 
         cflags = #cflags c, 
         header = #header c, 
         output = #output c, 
         mlbFile = #mlbFile c}

    fun addLinkOpts (M c) l =
      M {sources = #sources c,
         ffisources = #ffisources c,
         linkopts = #linkopts c @ l, 
         cflags = #cflags c, 
         header = #header c, 
         output = #output c, 
         mlbFile = #mlbFile c}

    fun addCFlags (M c) s =
      M {sources = #sources c,
         ffisources = #ffisources c,
         linkopts = #linkopts c, 
         cflags = #cflags c @ [s], 
         header = #header c, 
         output = #output c, 
         mlbFile = #mlbFile c}


    fun setExportHeader (M c) s =
      M {sources = #sources c,
         ffisources = #ffisources c,
         linkopts = #linkopts c, 
         cflags = #cflags c, 
         header = SOME s, 
         output = #output c, 
         mlbFile = #mlbFile c}

    fun setOutput (M c) s =
      M {sources = #sources c,
         ffisources = #ffisources c,
         linkopts = #linkopts c,  
         cflags = #cflags c, 
         header = #header c, 
         output = SOME s, 
         mlbFile = #mlbFile c}

    fun setOption c (k,v) = c

    fun generateFiles (M c) =
        let
            val mlbFile = "dummy.mlb"
        in
          M {sources = #sources c,
             ffisources = #ffisources c,
             linkopts = #linkopts c,  
             cflags = #cflags c, 
             header = #header c, 
             output = #output c, 
             mlbFile = SOME mlbFile}
        end

    fun invoke (M c) =
        let
            val cmd =
                ["mlton",
                 "-output", valOf (#output c)] @
                (case #header c of NONE => nil | SOME h =>
                    ["-export-header", h]) @
                (case #cflags c of [] => nil | l => [
                    "-cc-opt \"" ^ String.concatWith " " l ^ "\""]) @
                (case #linkopts c of [] => nil | l => [
                    "-link-opt \"" ^ String.concatWith " " l ^ "\""]) @
                [valOf (#mlbFile c)] @
                (#ffisources c)

            val cmd' = String.concatWith " " cmd

            val _ = exec cmd'
        in
            ()
        end
end


