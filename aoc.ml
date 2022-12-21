open! Core

let integer_parser =
  Angstrom.(
    take_while1 (function '-' | '0' .. '9' -> true | _ -> false)
    >>| int_of_string)

let whitespace = Angstrom.take_while1 (function ' ' -> true | _ -> false)
let pluralized_p str = Angstrom.(string str <* take_while (Char.equal 's'))

let parse_input_file file p =
  Angstrom.(
    match
      parse_string ~consume:Consume.All
        (p <* many any_char <* end_of_input)
        (In_channel.read_all file)
    with
    | Ok res -> res
    | Error err -> failwith err)

module Coord = struct
  type t = int * int [@@deriving compare, sexp, show, eq, hash]

  let manhattan (x1, y1) (x2, y2) = Int.abs (x1 - x2) + Int.abs (y1 - y2)

  let bounding_box coord_non_empty_list =
    let fst = List.hd_exn coord_non_empty_list in
    List.fold coord_non_empty_list ~init:(fst, fst)
      ~f:(fun ((minx, miny), (maxx, maxy)) (x, y) ->
        let nminx = if x < minx then x else minx in
        let nminy = if y < miny then y else miny in
        let nmaxx = if x > maxx then x else maxx in
        let nmaxy = if y > maxy then y else maxy in
        ((nminx, nminy), (nmaxx, nmaxy)))

  let draw_bb bb char_for_coord =
    let (min_x, min_y), (max_x, max_y) = bb in
    for y = min_y to max_y do
      for x = min_x to max_x do
        Out_channel.output_char stdout (char_for_coord (x, y))
      done;
      Out_channel.output_char stdout '\n'
    done
end

module CoordMap = Map.Make (Coord)
module CoordSet = Set.Make (Coord)

let range start end_ =
  let step = if start < end_ then fun x -> x + 1 else fun x -> x - 1 in
  let rec loop s acc =
    if s = end_ then s :: acc else loop (step s) (s :: acc)
  in
  if start = end_ then [ start ] else loop start []

let show_sexpable = Sexp.to_string
