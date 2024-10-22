open OcamlCanvas.V1.Color
open OcamlCanvas.V1.Event
open OcamlCanvas.V1.Canvas
open OcamlCanvas.V1.Backend

let on event ~run = React.E.map run event

let listeners : 'a React.event list ref = ref []

let register l =
  !listeners |> List.iter (fun e -> React.E.stop e) ;
  listeners := l

let w, h = (ref 300, ref 300) ;;

(* center it 80% height & maintain aspect ratio *)
init ()
let screen = create_onscreen ~size:(!w, !h) ~title:"Sokoban" ()

let redraw f =
  save screen ;
  fill_rect screen ~pos:(0., 0.) ~size:Int.(to_float !w, to_float !h) ;
  f () ;
  restore screen

let gamescreen level goto_mainscreen =
  let render () =
    set_fill_color screen green ;
    fill_text screen level (50., 70.) ;
    ()
  in

  redraw render ;

  (* to winscreen for 3secs *)
  register
    [
      on key_down ~run:(fun e ->
          match e.data.key with
          | KeyB -> goto_mainscreen ()
          | _ -> ()
          (* clear interval & closured resources *)
      );
    ]

let mainscreen lvls play_level =
  let cursor = ref 0 in
  let no_of_levels = List.length lvls in

  let render () =
    set_fill_color screen red ;
    fill_text screen ("main " ^ List.nth lvls !cursor) (30., 30.) ;
    ()
  in

  redraw render ;

  let wrap n =
    ((n mod no_of_levels) + no_of_levels) mod no_of_levels
  in

  register
    [
      on key_down ~run:(fun e ->
          match e.data.key with
          | KeyLeftArrow ->
              cursor := wrap (!cursor - 1) ;
              redraw render
          | KeyRightArrow ->
              cursor := wrap (!cursor + 1) ;
              redraw render
          | KeyReturn ->
              let lvl = List.nth lvls !cursor in
                play_level lvl
          | _ -> ()
      );
    ]

let root () =
  let levels = [ "one"; "two"; "three"; "four" ] in

  (* bail if no levels *)
  let rec goto_mainscreen () = mainscreen levels goto_gamescreen
  and goto_gamescreen level = gamescreen level goto_mainscreen in
    goto_mainscreen ()

let () =
  show screen ;

  root () ;

  let on_resize =
    on resize ~run:(fun e ->
        match e.data with
        | nw, nh ->
            w := nw ;
            h := nh
    )
  in

  run (fun () -> ignore [ on_resize ])
