open! Core
open! Aoc

type obj_type = Rock | Sand

let parse_input lines =
  Angstrom.(
    let coord_p = (fun x y -> (x, y)) <$> integer_p <* char ',' <*> integer_p in
    let segment_line =
      sep_by1 (string " -> ") coord_p >>| fun coords ->
      List.zip_exn (List.drop_last_exn coords) (List.drop coords 1)
    in
    let input_p = sep_by1 end_of_line segment_line >>| List.concat in
    match parse_string ~consume:Consume.Prefix input_p lines with
    | Ok res -> res
    | Error err -> failwith err)

let range start end_ =
  let step = if start < end_ then fun x -> x + 1 else fun x -> x - 1 in
  let rec loop s acc =
    if s = end_ then s :: acc else loop (step s) (s :: acc)
  in
  if start = end_ then [ start ] else loop start []

let mk_init_coord_map segments =
  List.concat_map segments ~f:(fun ((sx, sy), (ex, ey)) ->
      if sx = ex then List.map (range sy ey) ~f:(fun i -> (sx, i))
      else if sy = ey then List.map (range sx ex) ~f:(fun i -> (i, sy))
      else failwith "Unexpected line")
  |> List.sort ~compare:Coord.compare
  |> List.remove_consecutive_duplicates ~equal:Coord.equal
  |> List.map ~f:(fun c -> (c, Rock))
  |> CoordMap.of_alist_exn

let bounding_box coord_map =
  CoordMap.keys coord_map
  |> List.fold
       ~init:((1000, 0), (-1, -1))
       ~f:(fun ((minx, miny), (maxx, maxy)) (x, y) ->
         let nminx = if x < minx then x else minx in
         let nminy = if y < miny then y else miny in
         let nmaxx = if x > maxx then x else maxx in
         let nmaxy = if y > maxy then y else maxy in
         ((nminx, nminy), (nmaxx, nmaxy)))

let bb_contains ((min_x, min_y), (max_x, max_y)) (x, y) =
  x >= min_x && x <= max_x && y >= min_y && y <= max_y

(* let draw_1 coord_map = *)
(*   let (min_x, min_y), (max_x, max_y) = bounding_box coord_map in *)
(*   for y = min_y - 1 to max_y + 1 do *)
(*     for x = min_x - 1 to max_x + 1 do *)
(*       let c = *)
(*         match CoordMap.find coord_map (x, y) with *)
(*         | Some Rock -> '#' *)
(*         | Some Sand -> 'o' *)
(*         | None -> '.' *)
(*       in *)
(*       Out_channel.output_char stdout c *)
(*     done; *)
(*     Out_channel.output_char stdout '\n' *)
(*   done *)

let candidates (x, y) = [ (x, y + 1); (x - 1, y + 1); (x + 1, y + 1) ]

exception End_of_loop

let drop_sand_part_1 bb coord_map =
  let rec loop pos =
    if not (bb_contains bb pos) then raise End_of_loop
    else
      match
        List.find (candidates pos) ~f:(fun c -> not @@ CoordMap.mem coord_map c)
      with
      | None -> pos
      | Some next_pos -> loop next_pos
  in
  let resting_pos = loop (500, 0) in
  CoordMap.add_exn coord_map ~key:resting_pos ~data:Sand

let get_floor (_, (_, max_y)) = max_y + 1

let drop_sand_part_2 bb coord_map =
  let floor = get_floor bb in
  let rec loop pos =
    match
      List.find (candidates pos) ~f:(fun c -> not @@ CoordMap.mem coord_map c)
    with
    | None -> pos
    | Some (x, y) when y = floor -> (x, y)
    | Some pos -> loop pos
  in
  let resting_pos = loop (500, 0) in
  if phys_equal resting_pos (500, 0) then raise End_of_loop
  else CoordMap.add_exn coord_map ~key:resting_pos ~data:Sand

let solve coord_map drop_fn =
  let rec loop state i =
    try loop (drop_fn state) (i + 1) with End_of_loop -> i
  in
  loop coord_map 0

let () =
  let lines = In_channel.read_all "./input14.txt" in
  let segments = parse_input lines in
  let coord_map = mk_init_coord_map segments in
  let bb = bounding_box coord_map in
  printf "\nSolution Part 1: %d\n" (solve coord_map (drop_sand_part_1 bb));
  printf "Solution Part 2: %d\n" (solve coord_map (drop_sand_part_2 bb) + 1)
