type resources = {
  floor: Raylib.Texture2D.t' Raylib.ctyp;
  player: Raylib.Texture2D.t' Raylib.ctyp;
}

type renderable = {
  x: int
  y: int
}



module Floor = struct 
  type state = unit

  let on_render resources _state =
    let open Raylib in
    draw_texture resources.floor 0 0 Color.white;
    draw_texture resources.floor 128 0 Color.white;
    draw_texture resources.floor 256 0 Color.white;
    draw_texture resources.floor 0 60 Color.white;
    draw_texture resources.floor 128 60 Color.white;
    draw_texture resources.floor 256 60 Color.white;
    draw_texture resources.floor 0 120 Color.white;
    draw_texture resources.floor 128 120 Color.white;
    draw_texture resources.floor 256 120 Color.white;

end

module Player = struct
  type state = {
    x: int;
    y: int;
  }

  type command =
    | GoUp
    | GoDown
    | GoLeft
    | GoRight

  let update cmd state = match cmd with
    | GoDown  -> {state with y=(Stdlib.min (state.y + 1) 2)}
    | GoUp    -> {state with y=(Stdlib.max (state.y - 1) 0)}
    | GoRight -> {state with x=(Stdlib.min (state.x + 1) 2)}
    | GoLeft  -> {state with x=(Stdlib.max (state.x - 1) 0)}

  let on_input () =
    let open Raylib in
           if is_key_pressed Key.Down then Option.Some GoDown
      else if is_key_pressed Key.Up then Option.Some GoUp
      else if is_key_pressed Key.Left then Option.Some GoLeft
      else if is_key_pressed Key.Right then Option.Some GoRight
      else Option.none

  let on_render resources player =
    Raylib.draw_texture resources.player (player.x*128 + 32) (player.y*60) Raylib.Color.white;;

end

type state = {
  mutable player: Player.state;
}

let setup () =
  Raylib.init_window 800 450 "raylib [core] example - basic window";
  Raylib.set_target_fps 60;

  let a = Raylib.load_texture "assets/floor.png" in
  Gc.finalise Raylib.unload_texture a;
  let b = Raylib.load_texture "assets/player.png" in
  Gc.finalise Raylib.unload_texture b;

  {
    floor = a;
    player = b;
  }

let rec loop (state: state) resources =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      let open Raylib in
      begin_drawing ();
      
      clear_background Color.raywhite;
      draw_text "Congrats! You created your first window!" 190 200 20
        Color.lightgray;
      
      let () = match Player.on_input () with
        Some a -> state.player <- Player.update a state.player;
        | None -> () in

      Floor.on_render resources ();
      Player.on_render resources state.player;
      end_drawing ();
      loop state resources
  
let () = setup ()|> loop {player={x=0;y=0}}