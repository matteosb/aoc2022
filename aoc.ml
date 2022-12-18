open! Core

let integer_p =
  Angstrom.(
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string)

module Coord = struct
  type t = int * int [@@deriving compare, sexp, show, eq, hash]
end

module CoordMap = Map.Make (Coord)
