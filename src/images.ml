open Events
open OcamlCanvas.V1

let wall : Canvas.t option ref = ref None
let dock : Canvas.t option ref = ref None
let floor : Canvas.t option ref = ref None

let crate_active : Canvas.t option ref = ref None
let crate_inactive : Canvas.t option ref = ref None

let player_n : Canvas.t option ref = ref None
let player_s : Canvas.t option ref = ref None
let player_e : Canvas.t option ref = ref None
let player_w : Canvas.t option ref = ref None

let side =
  match Utils.get_resolution () with
  | w, h ->
      ( if h < w then
          h
        else
          w
      )
      / 30

let load_images () =
  ignore
    [
      Events.on
        ~run:(fun w -> wall := Some w)
        (Canvas.create_offscreen_from_png
           "/home/freeone/Desktop/code/playgrounds/sokoban/src/assets/wall.png"
        );
      Events.on
        ~run:(fun d -> dock := Some d)
        (Canvas.create_offscreen_from_png
           "/home/freeone/Desktop/code/playgrounds/sokoban/src/assets/dock.png"
        );
      Events.on
        ~run:(fun f -> floor := Some f)
        (Canvas.create_offscreen_from_png
           "/home/freeone/Desktop/code/playgrounds/sokoban/src/assets/floor.png"
        );
      Events.on
        ~run:(fun ca -> crate_active := Some ca)
        (Canvas.create_offscreen_from_png
           "/home/freeone/Desktop/code/playgrounds/sokoban/src/assets/crate-active.png"
        );
      Events.on
        ~run:(fun cia -> crate_inactive := Some cia)
        (Canvas.create_offscreen_from_png
           "/home/freeone/Desktop/code/playgrounds/sokoban/src/assets/crate-inactive.png"
        );
      Events.on
        ~run:(fun pn -> player_n := Some pn)
        (Canvas.create_offscreen_from_png
           "/home/freeone/Desktop/code/playgrounds/sokoban/src/assets/player_n.png"
        );
      Events.on
        ~run:(fun ps -> player_s := Some ps)
        (Canvas.create_offscreen_from_png
           "/home/freeone/Desktop/code/playgrounds/sokoban/src/assets/player_s.png"
        );
      Events.on
        ~run:(fun pe -> player_e := Some pe)
        (Canvas.create_offscreen_from_png
           "/home/freeone/Desktop/code/playgrounds/sokoban/src/assets/player_e.png"
        );
      Events.on
        ~run:(fun pw -> player_w := Some pw)
        (Canvas.create_offscreen_from_png
           "/home/freeone/Desktop/code/playgrounds/sokoban/src/assets/player_w.png"
        );
    ]
