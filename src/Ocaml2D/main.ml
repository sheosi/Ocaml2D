open Engine

module Commands = struct
type command =
  | GoUp
  | GoDown
  | GoLeft
  | GoRight
  | Attack
end;;

module Input = InputFrom(Commands)

module Health = struct
  type s = int
  include (val (Component.create ()): Component.Sig with type t = s)
end;;

module Enemy = struct
  type s = unit
  include (val (Component.create ()): Component.Sig with type t = s)
end;;

module HealthTracking = struct
  let opt_or def (opt: 'a option) =
    match opt with
    | Some a -> a
    | None -> def
  
  let opt_map f (opt: 'a option) =
    match opt with
    | Some a -> Option.some (f a)
    | None -> Option.none
  let get_health name =
    (Entity.by_name name |> Health.get_opt |> opt_map string_of_int |> opt_or "DEAD")

  let track_health tracked key =
    key |> Text.set (get_health tracked) |> ignore
end;;

let remove_entity id =
  let comps: (module Component.Sig) array = [|
    (module Position); (module Multiposition);
    (module Sprite); (module Text);
    (module Script); (module Input);
    (module Health); (module Enemy);
  |] in

  Array.iter (
    fun system -> 
      let module CurrComp= (val system: Component.Sig) in
      CurrComp.remove id
  ) comps;
;;
  
let can_die key =
  if (Health.get key) = 0 then
    remove_entity key

module InputHandling = struct  
  let on_input cmd (pos: Position.t) = 
    let x_size = 128 in
    let y_size = 60 in
    match cmd with
    | Commands.GoDown -> {pos with y=(Stdlib.min (pos.y + y_size) ((2 * y_size)))}
    | Commands.GoUp -> {pos with y=(Stdlib.max (pos.y - y_size) 0)}
    | Commands.GoLeft -> {pos with x=(Stdlib.max (pos.x - x_size) 32)}
    | Commands.GoRight -> {pos with x=(Stdlib.min (pos.x +  x_size) ((2 * x_size)+32)) }
    | Attack -> let key = Entity.by_name "Enemy" in Health.set ( (Health.get key) - 1 ) key|> ignore; pos

  let on_process key pos inp =
    Array.iter (
      fun (kb_key, cmd) -> 
        if Raylib.is_key_pressed kb_key then 
          Position.set (on_input cmd pos) key |> ignore 
        else ()
    )
    inp

  include (val (System2R.create on_process Position.iter Input.get_opt ))
end;;

module MyGame = struct
  let systems: (module System) array = [|
        (module InputHandling);
        (module Scripting);
        (module Multirendering);
        (module Rendering);
        (module TextRendering);
      |]
  let init_entities() =
    Entity.by_name "Floor"
    |> Multiposition.s [|
      {x=0;y=  0};{x=128;y=  0};{x=256;y=  0};
      {x=0;y= 60};{x=128;y= 60};{x=256;y= 60};
      {x=0;y=120};{x=128;y=120};{x=256;y=120};
    |]
    |> Sprite.s (Sprite.load "assets/floor.png")|> ignore;

    Entity.by_name "Enemy_Floor"
    |> Multiposition.s [|
      {x=384;y=  0};{x=512;y=  0};{x=640;y=  0};
      {x=384;y= 60};{x=512;y= 60};{x=640;y= 60};
      {x=384;y=120};{x=512;y=120};{x=640;y=120};
    |]
    |> Sprite.s (Sprite.load "assets/enemy_floor.png")|> ignore;

    Entity.by_name "Player"
    |> Position.s {x=32;y=0}
    |> Sprite.s (Sprite.load "assets/player.png")
    |> Health.s 100
    |> Script.s (can_die)
    |> Input.s [|
      (Raylib.Key.Up, Commands.GoUp);
      (Raylib.Key.Down, Commands.GoDown);
      (Raylib.Key.Left, Commands.GoLeft);
      (Raylib.Key.Right, Commands.GoRight);
      (Raylib.Key.Space, Commands.Attack);
    |] |> ignore;

    Entity.by_name "Enemy"
    |> Position.s {x=32+384;y=0}
    |> Sprite.s (Sprite.load "assets/enemy.png")
    |> Health.s 10
    |> Enemy.s ()
    |> Script.s (can_die)
    |> ignore;

    Entity.by_name "Health_hud"
    |> Position.s {x=0; y=0}
    |> Text.s (HealthTracking.get_health "Player")
    |> Script.s (HealthTracking.track_health "Player")
    |> ignore;

    Entity.by_name "Enemy_health"
    |> Position.s {x=384; y=0}
    |> Text.s (HealthTracking.get_health "Enemy")
    |> Script.s (HealthTracking.track_health "Enemy")
    |> ignore;
end;;

module Game = MakeGame (MyGame)
let () = Game.setup (); Game.loop ()