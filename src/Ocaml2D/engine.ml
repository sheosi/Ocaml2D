type key = int
module Component = struct
  module type Sig = sig
    type t

    val set: t -> key -> key
    val s: t -> key -> key
    val fold: (key -> t -> 'a -> 'a) -> 'a -> 'a
    val iter: (key -> t -> unit) -> unit
    val get: key -> t
    val get_opt: key -> t option
    val remove: key-> unit
  end;;

  let create (type s) ()  =
    let h = Hashtbl.create 0 in

    let module Def = struct
      type t = s
      let set t key = Hashtbl.replace h key t; key
      let s  t key = Hashtbl.replace h key t; key
      let fold f = Hashtbl.fold f h
      let iter f = Hashtbl.iter f h
      let get = Hashtbl.find h
      let get_opt = Hashtbl.find_opt h
      let remove = Hashtbl.remove h
    end in

    (module Def: Sig with type t = s)
end;;

module Position = struct
  type s = {
    x: int;
    y: int
  }
  
  include(val (Component.create ()): Component.Sig with type t = s)
end;;

module Multiposition = struct
  type a = {x: int; y: int}
  type s = a array

  include(val (Component.create ()): Component.Sig with type t = s)
end;;

module Sprite = struct
  type s = Raylib.Texture2D.t' Raylib.ctyp

  let load path = (
    let tex = Raylib.load_texture path in
    Gc.finalise Raylib.unload_texture tex;
    tex
  )

  include(val (Component.create ()): Component.Sig with type t = s)
end;;

module Entity = struct
  let entities: (string, int) Hashtbl.t = Hashtbl.create 0
  let next_id = ref 0
  let by_name name =
    match Hashtbl.find_opt entities name with
    | Some a -> a
    | None -> 
      let new_id = !next_id in
      next_id := !next_id + 1;
      Hashtbl.add entities name new_id; new_id

end;;

module type System = sig
  val process_elements: unit -> unit
end;;

module type System1RItf = sig
  type t_a
  val comp_a: (module Component.Sig)
  val on_process: key -> t_a -> unit
end;;

module System1R = struct
  let create on_process (iter_a: (key->'t_a->unit)->unit) =
    let module Def = struct
      let process_elements () = 
        iter_a (fun key v_a ->
          on_process key v_a)
    end in
    (module Def: System)
end;;

module type System2RItf = sig
  type t_a
  type t_b
  val comp_a: (module Component.Sig)
  val comp_b: (module Component.Sig)
  val on_process: key -> t_a -> t_b -> unit
end;;

module System2R = struct
  let create  on_process (iter_a: (key->'t_a->unit)->unit) (get_b: key->'t_b option) =
    let module Def = struct
      let process_elements () =
        iter_a (fun key v_a-> 
          match get_b key with
          | Some v_b -> on_process key v_a v_b
          | None -> ())

    end in

    (module Def: System)
    
end;;



module Multirendering = struct
  let on_process (_key: key) (poses: Multiposition.t) (spr: Sprite.t) =
    Array.iter (
      fun (pos: Multiposition.a) -> Raylib.draw_texture spr pos.x pos.y Raylib.Color.white;
    ) poses
      
  include (val (System2R.create on_process Multiposition.iter Sprite.get_opt ))
end;;

module Rendering = struct
  let on_process _key (pos: Position.t) spr =
    Raylib.draw_texture spr pos.x pos.y Raylib.Color.white

  include (val (System2R.create on_process Position.iter Sprite.get_opt))
end;;


module type GameItf = sig
  val systems: (module System) array
  val init_entities: unit -> unit
end;;
module MakeGame(G: GameItf)= struct
  let setup () =
    Raylib.init_window 800 450 "raylib [core] example - basic window";
    Raylib.set_target_fps 60;
    G.init_entities ()
  let rec loop () =
    match Raylib.window_should_close () with
    | true -> Raylib.close_window ()
    | false ->
        let open Raylib in
        begin_drawing ();
        
        clear_background Color.raywhite;

        Array.iter (
          fun system -> 
            let module CurrSystem = (val system: System) in
            CurrSystem.process_elements ()
        ) G.systems;

        end_drawing ();
        loop ();
end;;

module type InputCommands = sig
  type command
end;;

module InputFrom(C: InputCommands) = struct
  type s = (Raylib.Key.t * C.command) array
  include (val (Component.create ()): Component.Sig with type t = s)
end;;

module Text = struct
  type s = string
  include (val (Component.create ()): Component.Sig with type t = s)
end;;

module Script = struct
  type s = (key->unit)
  include (val (Component.create ()): Component.Sig with type t = s)
end;;

module Scripting = struct
  let on_process key (f: key->unit) =
    f key
  include (val (System1R.create on_process Script.iter))
end;;

module TextRendering = struct
  let on_process _key (pos: Position.t) text =
    Raylib.draw_text text pos.x pos.y 20 Raylib.Color.lightgray

  include (val (System2R.create on_process Position.iter Text.get_opt ))
end;;