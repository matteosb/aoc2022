open! Core

type mat = int array array [@@deriving show]
type position = int * int [@@deriving show, sexp]

let position_cmp (a1, b1) (a2, b2) =
  let a_cmp = Int.compare a1 a2 in
  if a_cmp = 0 then Int.compare b1 b2 else a_cmp

module Position = struct
  type t = int * int [@@deriving compare, sexp]
end

module PositionComparator = struct
  type t = position [@@deriving sexp]

  let compare = position_cmp
end

module PositionOrder = struct
  include Position
  include Comparator.Make (Position)
end

module PositionSet = Set.Make (PositionOrder)
module PositionMap = Map.Make (PositionOrder)

let mk_matrix lines =
  let dimy = List.length lines in
  let dimx = String.length @@ List.nth_exn lines 0 in
  let matrix = Array.make_matrix ~dimx ~dimy 0 in
  let starting_pos = ref (-1, -1) in
  let end_pos = ref (-1, -1) in
  let parse coord = function
    | 'E' ->
        end_pos := coord;
        int_of_char 'z' - 97
    | 'S' ->
        starting_pos := coord;
        int_of_char 'a' - 97
    | x -> int_of_char x - 97
  in
  for x = 0 to dimx - 1 do
    for y = 0 to dimy - 1 do
      matrix.(x).(y) <- parse (x, y) (String.get (List.nth_exn lines y) x)
    done
  done;
  (!starting_pos, !end_pos, matrix)

let get_val (x, y) matrix = matrix.(x).(y)

let neighbors_inv (x, y) matrix =
  let maxy = Array.length matrix in
  let maxx = Array.length matrix.(0) in
  let source_val = get_val (x, y) matrix in
  [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]
  |> List.filter ~f:(fun (nx, ny) ->
         nx >= 0 && ny >= 0 && nx < maxy && ny < maxx)
  |> List.filter ~f:(fun n -> source_val - get_val n matrix < 2)

let vertex_set matrix =
  PositionSet.of_array
  @@ Array.concat_mapi matrix ~f:(fun x y_array ->
         Array.mapi y_array ~f:(fun y _ -> (x, y)))

(* Note: we treat the end posisiton as the start and compute the shortest path in reverse,
   which gives us a map of distances from the endpoint *)
let shortest_dist end_pos start matrix =
  let vertices = ref @@ vertex_set matrix in
  let v_list = Set.to_list !vertices in
  let distances =
    ref @@ PositionMap.of_alist_exn
    @@ List.map v_list ~f:(fun v ->
           if position_cmp start v = 0 then (v, 0) else (v, 10000))
  in
  let prev =
    ref @@ PositionMap.of_alist_exn @@ List.map v_list ~f:(fun v -> (v, None))
  in
  let dist x = PositionMap.find_exn !distances x in
  let find_min_dist_node () =
    PositionSet.fold !vertices ~init:None ~f:(fun acc pos ->
        match acc with
        | None -> Some pos
        | Some pos2 ->
            let dist1 = dist pos in
            let dist2 = dist pos2 in
            if dist2 < dist1 then acc else Some pos)
  in
  while not @@ PositionSet.is_empty !vertices do
    let v = Option.value_exn @@ find_min_dist_node () in
    vertices := PositionSet.remove !vertices v;
    List.iter (neighbors_inv v matrix) ~f:(fun n ->
        let new_dist = dist v + 1 in
        if PositionSet.mem !vertices n then
          if new_dist < dist n then (
            distances := PositionMap.set !distances ~key:n ~data:new_dist;
            prev := PositionMap.set !prev ~key:n ~data:(Some v)))
  done;
  (dist end_pos, !distances)

let part_2 matrix distances start =
  let all_indexs =
    Array.concat_mapi matrix ~f:(fun x y_array ->
        Array.mapi y_array ~f:(fun y _ -> (x, y)))
  in
  Array.filter all_indexs ~f:(fun pos ->
      get_val pos matrix = 0 && not (phys_equal pos start))
  |> Array.map ~f:(fun s -> PositionMap.find_exn distances s)
  |> Array.min_elt ~compare:Int.compare

let () =
  let lines = In_channel.read_lines "./input12.txt" in
  let start, end_pos, matrix = mk_matrix lines in
  let part_1, distances = shortest_dist start end_pos matrix in
  printf "\nSolution Part 1: %d\n" part_1;
  printf "Solution Part 2: %d\n"
  @@ Option.value_exn
  @@ part_2 matrix distances start
