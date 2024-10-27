open Event

open OcamlCanvas.V1
open OcamlCanvas.V1.Event
open OcamlCanvas.V1.Canvas ;;

Backend.init ()

(* center it 80% height & maintain aspect ratio *)

let w, h = (ref 800, ref 600)

let screen = create_onscreen ~size:(!w, !h) ~title:"sokoban" ()

let redraw f =
  save screen ;
  fill_rect screen ~pos:(0., 0.) ~size:Int.(to_float !w, to_float !h) ;
  f () ;
  restore screen

let mainscreen at levels play_level =
  let cursor = ref at in
  let no_of_levels = List.length levels in

  let render () =
    set_fill_color screen Color.red ;
    fill_text screen ("main " ^ string_of_int (!cursor + 1)) (30., 30.) ;
    ()
  in

  redraw render ;

  let wrap n =
    ((n mod no_of_levels) + no_of_levels) mod no_of_levels
  in

  Events.register
    [
      Events.on key_down ~run:(fun e ->
          match e.data.key with
          | KeyLeftArrow ->
              cursor := wrap (!cursor - 1) ;
              redraw render
          | KeyRightArrow ->
              cursor := wrap (!cursor + 1) ;
              redraw render
          | KeyReturn ->
              let level = List.nth levels !cursor in
                play_level !cursor level
          | _ -> ()
      );
    ]

let gamescreen n level goto_mainscreen =
  let render () = () in

  redraw render ;

  (* to winscreen for 3secs *)
  Events.register
    [
      Events.on key_down ~run:(fun e ->
          match e.data.key with
          | KeySpacebar -> goto_mainscreen n ()
          | _ -> ()
          (* clear interval & closured resources *)
      );
    ]
