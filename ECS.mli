type entity

exception Property_Error of string

module type BASEPROP = sig
  val bind : entity -> unit
  val rem  : entity -> unit
end

module type PROPERTY = sig
  type t
  (* Can't hide function bind ??? *)
  val bind : entity -> unit
  val get : entity -> t
  val set : entity -> t -> unit 
  val s   : t -> entity -> entity
  val rem : entity -> unit
end

module type COMPONENT = sig
  val bind : entity -> unit
  val b    : entity -> entity
  val rem  : entity -> unit
  val bound: entity -> bool
  val iter : (entity -> unit) -> unit
end

val next_id : unit -> entity

val null_id : entity

val delete : entity -> unit

(** new_component bl cl creates a component containing properties
 *  bl and depending on components cl *)
val new_component : 
  (module BASEPROP) list -> (module COMPONENT) list -> (module COMPONENT)

(** new_property d creates a property with default value d
 *  when binding a component containing this property, d will
 *  be used as the initial value *)
val new_property :
  ?default:'a -> unit -> (module PROPERTY with type t = 'a)

(** iterate f l c iterates the function f through all the entities
 *  of l bound to all the components of c *)
val iterate :
  (entity -> unit) -> (entity list) -> (module COMPONENT) list -> unit

(** iterate_all f c iterates the function f through all the entities
 *  bound to all the components of c *)
val iterate_all :
  (entity -> unit) -> (module COMPONENT) list -> unit

(** filter l c returns the sublist of l of all the entities
 *  bound to all the components of c 
 *  Allows for easy creation of Systems *)
val filter :
  (entity list) -> (module COMPONENT) list -> (entity list)

(** filter_all c returns the list of all the entities
 *  bound to all the components of c *)
val filter_all :
  (module COMPONENT) list -> (entity list)

