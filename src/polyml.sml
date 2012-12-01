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
    open ToolOpt

    type t = unit

    val name = "PolyML"

    val empty = ()

    fun addSources c l = c
    fun addFFISources c l = c
    fun addLinkOpts c l = c
    fun addCFlags c s = c
    fun setExportHeader c s = c
    fun setOutput c s = c
    fun setOption c (k,v) = c
    fun generateFiles c = c
    fun invoke c = ()
end



