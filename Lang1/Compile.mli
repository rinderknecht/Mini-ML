(* This module exports a transformation from Mini-ML programs to
   functionally equivalent OCaml programs whose execution builds
   networks.

     The transformations are based on the library implemented by the
   module [Edit]. That module defines an abstract Domain-Specific
   Language for defining editions (it is abstract in the sense that it
   lacks a concrete syntax). Actually, there are two of such
   languages, a low-level one, and a high-level one, the latter being
   based on a filtering paradigm (that is, copying the input to the
   output is the default, for which there is no editing command).

     The type [trans] is the type of transformations.
*)

module Trans :
  sig
    type t = Id
    type trans = t

    val compare   : t -> t -> int
    val to_string : t -> string
  end

module TEdit : Edit.S with type trans = Trans.t

(* Stateful filters and editors *)

type state = {
  trans : Trans.t; (* Transformation         *)
  input : string   (* Basename of input file *)
}

type filter = state * TEdit.filter

type editor = filter -> filter

(* Wrappers for creating editors *)

val insert      : ?loc:Loc.t   -> string               -> editor
val overwrite   : Loc.t        -> string               -> editor
val patch       : ?start:Loc.t -> stop:Loc.t  -> string -> editor
val delete      : ?start:Loc.t -> stop:Loc.t           -> editor
val discard     :                 Loc.t               -> editor
val copy_to     :                 Loc.t               -> editor
val skip_to     :                 char                -> editor
val skip_to_end :                                       editor
val append      :                              string -> editor
val copy_to_end :                                       editor
val stop        :                                       editor

val patch_region  : Region.t -> string -> editor
val delete_region : Region.t -> editor

(* Setters and getters of the state  *)

val set_trans : Trans.t -> editor
val get_input : filter -> string
val get_state : filter -> state
val get_edit  : filter -> TEdit.filter

(* Edition of the main file *)

val edit_ast    : verb:Utils.String.Set.t -> AST.t -> editor
