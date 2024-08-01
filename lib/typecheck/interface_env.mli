open Common
open Common.SourceCode
type t

val lookup : Common_types.interface_name -> t -> Position.t list -> Interface.t WithPos.t
val bind : Common_types.interface_name -> Interface.t WithPos.t -> t -> t
val bind_many : (Common_types.interface_name * Interface.t WithPos.t) list -> t -> t
val from_list : (Interface.t WithPos.t) list -> t
