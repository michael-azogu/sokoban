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
