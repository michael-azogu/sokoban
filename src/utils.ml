open Core

let ord n =
  let suffix =
    let j, k = (n mod 10, n mod 100) in
      if j = 1 && k <> 11 then
        "st"
      else if j = 2 && k <> 12 then
        "nd"
      else if j = 3 && k <> 13 then
        "rd"
      else
        "th"
  in
    string_of_int n ^ suffix

let get_resolution () =
  let ic =
    Core_unix.open_process_in "xrandr | grep '*' | awk '{print $1}'"
  in
  let resolution = In_channel.input_line ic in
    In_channel.close ic ;
    match resolution with
    | Some r -> (
      match String.split ~on:'x' r with
      | [ w; h ] -> (int_of_string w, int_of_string h)
      | _ -> (0, 0)
    )
    | None -> (0, 0)
