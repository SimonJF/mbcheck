open Common
type t

val lookup : Common_types.interface_name -> t -> Interface.t
val bind : Common_types.interface_name -> Interface.t -> t -> t
val bind_many : (Common_types.interface_name * Interface.t) list -> t -> t
val from_list : Interface.t list -> t
