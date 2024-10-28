open Core
open Events
open Screens

open OcamlCanvas.V1

let () =
  Images.load_images () ;
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
      print_endline
        ("no levels present\n" ^ String.concat ~sep:"\n" errors)
  in

  root () ;

  let on_close = Events.on Event.close ~run:(fun _ -> exit 0) in

  Backend.run (fun () ->
      Events.register [] ;
      ignore [ on_close ]
  )
