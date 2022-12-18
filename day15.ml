open! Core
open! Aoc

type sensor_report = { sensor : Coord.t; beacon : Coord.t }
let radius { sensor; beacon } = Coord.manhattan sensor beacon
let is_in_radius c report = Coord.manhattan c report.sensor <= radius report

let parse_input lines =
  Angstrom.(
    let line_parser =
      (fun x1 y1 x2 y2 -> { sensor = (x1, y1); beacon = (x2, y2) })
      <$> string "Sensor at x=" *> integer_parser
      <*> string ", y=" *> integer_parser
      <*> string ": closest beacon is at x=" *> integer_parser
      <*> string ", y=" *> integer_parser
      <* end_of_line
    in
    match parse_string ~consume:Consume.Prefix (many1 line_parser) lines with
    | Ok res -> res
    | Error err -> failwith err)

let solve_1 reports y =
  let beacon_set =
    List.map reports ~f:(fun sr -> sr.beacon) |> CoordSet.of_list
  in
  let sensor_lst = List.map reports ~f:(fun sr -> sr.sensor) in
  let largest_radius =
    List.map reports ~f:radius |> List.fold ~init:0 ~f:Int.max
  in
  let (minx, _), (maxx, _) = Coord.bounding_box sensor_lst in
  let in_range_of_beacon c = List.exists reports ~f:(is_in_radius c) in
  let line_to_scan =
    List.map
      (range (minx - largest_radius) (maxx + largest_radius))
      ~f:(fun x -> (x, y))
  in
  List.fold line_to_scan ~init:0 ~f:(fun acc c ->
      if in_range_of_beacon c && not (CoordSet.mem beacon_set c) then acc + 1
      else acc)

exception Range_disjoint of int

let solve_2 reports maxxy =
  let combine_sorted_ranges (s1, e1) (s2, e2) =
    if s1 > s2 then failwith "ranges not sorted"
    else if s2 > e1 then
      (* should use an either here but i'm lazy *)
      raise @@ Range_disjoint (e1 + 1)
    else (s1, max e1 e2)
  in
  let trunc_range (s, e) =
    let ns = if s < 0 then 0 else if s > maxxy then maxxy else s in
    let ne = if e < maxxy then e else maxxy in
    (ns, ne)
  in
  let range_on_line y report =
    let r = radius report in
    let bx, by = report.sensor in
    let delta = r - Int.abs (y - by) in
    if delta < 0 then None else Some (bx - delta, bx + delta)
  in
  let find_x y =
    try
      List.filter_map reports ~f:(fun r ->
          Option.map (range_on_line y r) ~f:trunc_range)
      |> List.sort ~compare:(fun (x1, _) (x2, _) -> Int.compare x1 x2)
      |> List.fold ~init:(0, 0) ~f:(fun acc r -> combine_sorted_ranges acc r)
      |> const None
    with Range_disjoint x -> Some x
  in
  let solution = ref (-1) in
  for y = 0 to maxxy do
    match find_x y with None -> () | Some x -> solution := (x * 4000000) + y
  done;
  !solution

let () =
  let lines = In_channel.read_all "./input15.txt" in
  let reports = parse_input lines in
  printf "\nSolution Part 1: %d\n" @@ solve_1 reports 2000000;
  printf "Solution Part 2: %d\n" @@ solve_2 reports 4000000
