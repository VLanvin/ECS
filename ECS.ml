(* Module representing keys aka entities
 * Entities are just keys to access properties' hashtables *)
module type KEY = sig
  type t
  val zero : t
  val incr : t ref -> unit
end

(* Implementation of a basic key type *)
module IntKey : KEY = struct
  type t = int
  let zero = 0
  let incr = incr
end

(* Functor to create the ECS system *)
module Make (Key : KEY) = struct

  exception Property_Error of string 

  type entity = Key.t

  (* Non type-dependant signature for properties.
   * Allows for lists of properties *)
  module type BASEPROP = sig
    val bind : entity -> unit
    val rem  : entity -> unit
  end

  (* Signature for properties.
   * Just some basic accessors *)
  module type PROPERTY = sig
    type t
    val bind : entity -> unit
    val get  : entity -> t
    val set  : entity -> t -> unit 
    (* Like set, but returns the entity after modification.
     * Allows for easy composition *)
    val s    : t -> entity -> entity
    val rem  : entity -> unit
  end

  (* Signature for components (groups of properties)
   * When you bind an entity to a component,
   * properties are added automatically.
   * Warning : values of properties must be initialized
   * before use, or a default value must be provided. *)
  module type COMPONENT = sig
    val bind : entity -> unit
    val b    : entity -> entity
    val rem  : entity -> unit
    val bound: entity -> bool
    val iter : (entity -> unit) -> unit
  end

  let last_id = ref Key.zero

  let next_id () = Key.incr last_id; !last_id

  let null_id = Key.zero

  (* Store components for easy deletion of entities *)
  let components : (module COMPONENT) list ref = ref []

  (* Delete an entity *)
  let delete ent = 
    List.iter (fun (module C : COMPONENT) -> C.rem ent) !components

  (* Register a component *)
  let register_component c = components := c :: !components

  (* Creates a first-class module representing a component *)
  let new_component props deps = 
    let entities = ref [] in
    let module T = struct
      let bound elt =
        let rec aux = function
          |[] -> false
          |t::q when t = elt -> true
          |t::q -> aux q
        in aux !entities
      let bind elt = 
        if bound elt then () else begin 
          entities := elt :: !entities;
          List.iter
          (fun (module P : BASEPROP) -> P.bind elt)
          props; 
          List.iter
          (fun (module C : COMPONENT) -> C.bind elt)
          deps
        end
      let b elt = bind elt; elt
      let rem elt = List.iter 
        (fun (module P : BASEPROP) -> P.rem elt)
        props;
        let rec aux = function 
          |[] -> [] 
          |t::q when t = elt -> q
          |t::q -> t :: (aux q)
        in entities := aux !entities
      let iter f = List.iter f !entities
    end in
    register_component (module T : COMPONENT);
    (module T : COMPONENT)

  (* Creates a first-class module representing a property *)
  let new_property (type s) ?default () = 
    let table = Hashtbl.create 10 in
    let module T = struct
      type t = s
      let bind elt =
        try ignore (Hashtbl.find table elt)
        with |Not_found -> Hashtbl.add table elt default 
      let get elt =
        try 
          match Hashtbl.find table elt with
          |None    -> raise (Property_Error "Not set (no default value)")
          |Some(v) -> v
        with |Not_found -> raise (Property_Error "Property not bound")
      let set e v = Hashtbl.replace table e (Some v)
      let s v e = set e v; e
      let rem = Hashtbl.remove table
    end in
    (module T : PROPERTY with type t = s)

  let iterate func entities comps = 
    let rec cfilter e = 
      if List.fold_left 
        (fun b (module C : COMPONENT) -> 
        b && (C.bound e)) true comps
      then func e
    in List.iter cfilter entities
        
  let iterate_all func = function
    |[] -> raise (Property_Error "Can't iterate on empty components list")
    |(module T : COMPONENT)::q -> begin
      let rec cfilter e =
        if List.fold_left
          (fun b (module C : COMPONENT) ->
          b && (C.bound e)) true q
        then func e
      in T.iter cfilter
    end

  let filter entities comps =
    let l = ref [] in
    iterate (fun e -> l := e::!l) entities comps; !l

  let filter_all comps = 
    let l = ref [] in 
    iterate_all (fun e -> l := e::!l) comps; !l
end
