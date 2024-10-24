open Printf

type direction =
   | N
   | E
   | S
   | W

let defualt_direction = W

type entity =
   | Wall
   | Dock
   | Crate
   | Keeper of direction

type tile = entity list

type warehouse = tile list

(* defaults + extras *)
let read_levels = '\\'

(*

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

let parse bytes =
  let nbytes = Bytes.length bytes in

  if nbytes > 4 then (
    let r, c, kr, kc =
      ( Bytes.get_uint8 bytes 0,
        Bytes.get_uint8 bytes 1,
        Bytes.get_uint8 bytes 2,
        Bytes.get_uint8 bytes 3
      )
    in

    let reasons =
      let because pred msg acc =
        if pred = true then
          msg :: acc
        else
          acc
      in
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
      in
      let krth, kcth = (kr + 1, kc + 1) in
        []
        |> because (c = 0 || r = 0)
           @@ sprintf
                "- the warehouse cant have %d rows and %d columns"
                r
                c
        |> because (krth > r)
           @@ sprintf
                "- the keeper's position on the (%s)row is beyond no-of-rows:(%d)"
                (ord krth)
                r
        |> because (kcth > c)
           @@ sprintf
                "- the keeper's position on the (%s)column is beyond no-of-columns:(%d)"
                (ord kcth)
                c
    in

    if List.is_empty reasons then (
      let tile_bytes = Bytes.sub bytes 2 nbytes in
        ()
    ) else
      (* try with raise *)
      printf "parsing terminated:\n%s\n" (String.concat "\n" reasons)
  ) else
    ()
