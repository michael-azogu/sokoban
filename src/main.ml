(* [@@@warning "-33"] *)

open Event
open Screens

open OcamlCanvas.V1

let () =
  Canvas.show screen ;

  let root () =
    let levels, errors = Levels.get () in

    if List.length levels > 0 then (
      let rec goto_gamescreen n level =
        gamescreen n level goto_mainscreen
      and goto_mainscreen at () =
        mainscreen at levels goto_gamescreen
      in
        goto_mainscreen 0 ()
    ) else
      print_endline "no levels present"
  in

  root () ;

  let on_resize =
    Events.on Event.resize ~run:(fun e ->
        match e.data with
        | nw, nh -> ((w := nw), (h := nh))
    )
  in

  let on_close = Events.on Event.close ~run:(fun _ -> exit 0) in

  Backend.run (fun () -> ignore [ on_resize; on_close ])
