type 'a setting


module ReceiveTypingStrategy : sig
    type t = Strict | Interface | Nothing
    val enum : (string * t) list
end

val verbose : bool setting
val debug : bool setting
val benchmark : int setting
val receive_typing_strategy : ReceiveTypingStrategy.t setting

val set : 'a setting -> 'a -> unit
val get : 'a setting -> 'a

val if_verbose : (unit -> unit) -> unit
val if_debug : (unit -> unit) -> unit
