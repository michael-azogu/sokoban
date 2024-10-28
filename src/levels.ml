open Core

type direction =
   | N
   | E
   | S
   | W

type entity =
   | Floor
   | Wall
   | Dock
   | Crate

type tile = entity list

type warehouse = tile list

type keeper = {
   r : int;
   c : int;
   direction : direction;
 }

type level = {
   rows : int;
   cols : int;
   keeper : keeper;
   warehouse : warehouse;
 }

type file = {
   bytes : Bytes.t;
   filename : string;
 }

let wall = 1 lsl 0
let dock = 1 lsl 1
let crate = 1 lsl 2

let default_direction = W

(**

 Sokoban Level File Format
 -------------------------
 level metadata:
   byte 0:               number of rows
   byte 1:               number of columns
   byte 2:               keeper's row position (0-indexed)
   byte 3:               keeper's column position (0-indexed)

   00001100 00010010 00000011 00001111
   └──────┤ └──────┤ └──────┤ └──────┤
          |        |        |        |
          12 rows  |        |        |
                   18 cols  |        |
                            row 3    |
                                     col 15

 tile layout:
   from bytes (n=4 - n=r*c) two tiles per byte

   bit 3:  `reserved for possible future use`
   bit 2:   crate
   bit 1:   dock
   bit 0:   wall

                   only a wall | 0001
               crate on a dock | 0110
   ! INVALID - crate on a wall | 0101

   NB: 0 = absent , 1 = present

*)
let parse { filename; bytes } =
  let nbytes = Bytes.length bytes in

  if nbytes <= 4 then
    Error (filename ^ "\ndoesnt have enough data <= 4bytes")
  else (
    let rows, cols, kr, kc =
      ( Bytes.get bytes 0 |> int_of_char,
        Bytes.get bytes 1 |> int_of_char,
        Bytes.get bytes 2 |> int_of_char,
        Bytes.get bytes 3 |> int_of_char
      )
    in

    let because pred msg acc =
      if pred then
        msg :: acc
      else
        acc
    in
    let krth, kcth = (kr + 1, kc + 1) in
    let reasons =
      []
      |> because (cols = 0 || rows = 0)
         @@ sprintf
              "- the warehouse cant have %d rows and %d columns"
              rows
              cols
      |> because (krth > rows)
         @@ sprintf
              "- the keeper's position on the (%s)row is beyond no-of-rows:(%d)"
              (Utils.ord krth)
              rows
      |> because (kcth > cols)
         @@ sprintf
              "- the keeper's position on the (%s)column is beyond no-of-columns:(%d)"
              (Utils.ord kcth)
              cols
    in

    let split_byte i = [ i lsr 4; i land 0xF ] in

    let to_tile i =
      let has x tile =
        if i land x <> 0 then
          [ tile ]
        else
          []
      in
        has wall Wall @ has dock Dock @ has crate Crate
    in

    if List.is_empty reasons then (
      let tile_bytes = Bytes.sub bytes ~pos:4 ~len:(nbytes - 4) in
        Ok
          {
            rows;
            cols;
            keeper = { r = kr; c = kc; direction = default_direction };
            warehouse =
              List.take
                (tile_bytes
                |> Bytes.to_list
                |> List.map ~f:int_of_char
                |> List.concat_map ~f:split_byte
                |> List.map ~f:to_tile
                )
                (rows * cols);
          }
    ) else
      Error
        (sprintf
           "parsing terminated for %s:\n%s\n"
           filename
           (String.concat ~sep:"\n" reasons)
        )
  )

let read_sokoban_level_files () =
  let dir = Core_unix.getcwd () ^ "/" in
    Stdlib.Sys.readdir dir
    |> Array.to_list
    |> List.filter ~f:(fun file ->
           Filename.check_suffix file ".sokoban"
       )
    |> List.map ~f:(fun filename ->
           let filename = dir ^ filename in
           let channel = In_channel.create filename in
           let len = Int.of_int64_exn (In_channel.length channel) in
           let bytes = Bytes.create len in
             In_channel.really_input_exn
               channel
               ~buf:bytes
               ~pos:0
               ~len ;
             In_channel.close channel ;
             { bytes; filename }
       )

let get () =
  read_sokoban_level_files ()
  |> List.partition_map ~f:(fun file ->
         match parse file with
         | Ok level -> First level
         | Error reason -> Second reason
     )
