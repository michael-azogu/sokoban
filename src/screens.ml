open Core
open Events
open Levels
open OcamlCanvas.V1
open OcamlCanvas.V1.Event
open OcamlCanvas.V1.Canvas ;;

Backend.init ()

(* center it 80% height & maintain aspect ratio *)

let w, h = Utils.get_resolution ()

let screen =
  create_onscreen ~size:(w, h) ~title:"sokoban" ~resizeable:true ()

let blank = ImageData.create (w, h)

let redraw f =
  ImageData.fill blank Color.white ;
  Canvas.put_image_data
    screen
    ~dpos:(0, 0)
    blank
    ~size:(w, h)
    ~spos:(0, 0) ;
  f ()

let mainscreen at (levels : Levels.level list) play_level =
  let cursor = ref at in
  let no_of_levels = List.length levels in

  let render () =
    set_fill_color screen Color.red ;
    fill_text
      screen
      (sprintf "level %d of %d" (!cursor + 1) (List.length levels))
      (30., 30.) ;
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
          | KeyReturn -> (
            match List.nth levels !cursor with
            | Some level -> play_level !cursor level
            | None -> print_endline "out of bounds list index"
          )
          | _ -> ()
      );
    ]

let gamescreen n level goto_mainscreen =
  let side = Images.side in

  let render () =
    Canvas.set_size screen (level.cols * side, level.rows * side) ;

    for c = 0 to level.cols - 1 do
      for r = 0 to level.rows - 1 do
        let tile =
          List.nth_exn level.warehouse ((r * level.cols) + c)
        in

        let x, y = (c * side, r * side) in

        List.iter [ Floor; Dock; Crate; Wall ] ~f:(fun e ->
            let sprite =
              match
                List.find tile ~f:(fun te ->
                    match (te, e) with
                    | Wall, Wall -> true
                    | Dock, Dock -> true
                    | Crate, Crate -> true
                    | _ -> false
                )
              with
              | Some t -> (
                match t with
                | Wall -> Images.wall
                | Dock -> Images.dock
                | Crate -> Images.crate_active
                | _ -> Images.floor
              )
              | None -> Images.floor
            in

            match !sprite with
            | None -> ()
            | Some image ->
                let data =
                  Canvas.get_image_data
                    image
                    ~pos:(0, 0)
                    ~size:(side, side)
                in
                  Canvas.put_image_data
                    screen
                    data
                    ~spos:(0, 0)
                    ~dpos:(x, y)
                    ~size:(side, side)
        ) ;

        if level.keeper.r = r && level.keeper.c = c then (
          let sprite =
            match level.keeper.direction with
            | N -> Images.player_n
            | E -> Images.player_e
            | S -> Images.player_s
            | W -> Images.player_w
          in
            match !sprite with
            | None -> ()
            | Some image ->
                let data =
                  Canvas.get_image_data
                    image
                    ~pos:(0, 0)
                    ~size:(side, side)
                in
                  Canvas.put_image_data
                    screen
                    data
                    ~spos:(0, 0)
                    ~dpos:(x, y)
                    ~size:(side, side)
        )
      done
    done
  in

  redraw render ;

  Events.register
    [
      Events.on key_down ~run:(fun e ->
          match e.data.key with
          | KeySpacebar -> goto_mainscreen n ()
          (* | KeyUpArrow ->
              gamescreen
                n
                {
                  level with
                  keeper =
                    { level.keeper with r = level.keeper.r + 1 };
                }
                goto_mainscreen *)
          | _ -> ()
          (* clear interval & closured resources *)
      );
    ]
