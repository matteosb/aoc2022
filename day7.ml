open! Core
module AdjList = Map.Make (String)

type fs_node = Dir of string | File of string * int
type parsed_line = Cd of string | Ls | LsOutput of fs_node

let extract_fs_node = function
  | LsOutput fsn -> fsn
  | _ -> failwith "Not an ls output inst"

let parse_line line =
  let split = String.split ~on:' ' line in
  match split with
  | [ "$"; "ls" ] -> Ls
  | [ "$"; "cd"; cd_path ] -> Cd cd_path
  | [ "dir"; dir_name ] -> LsOutput (Dir dir_name)
  | [ size; file_name ] -> LsOutput (File (file_name, Int.of_string size))
  | _ -> failwith "Unable to parse line"

let mk_dir_name l = String.concat ~sep:"/" (List.rev l)

let rec build_adjacency_list inpt adj_list dir_stack =
  let is_ls_out = function LsOutput _ -> true | _ -> false in
  match inpt with
  | [] -> adj_list
  | Ls :: rest ->
      let ls_out = List.take_while ~f:is_ls_out rest in
      let nodes = List.map ~f:extract_fs_node ls_out in
      let al2 = Map.add_exn ~key:(mk_dir_name dir_stack) ~data:nodes adj_list in
      build_adjacency_list (List.drop_while ~f:is_ls_out rest) al2 dir_stack
  | Cd ".." :: rest ->
      build_adjacency_list rest adj_list (List.drop dir_stack 1)
  | Cd dir :: rest -> build_adjacency_list rest adj_list (dir :: dir_stack)
  | _ -> failwith "Shut up"

let rec dir_size name adj_list =
  let node_size = function
    | File (_, size) -> size
    | Dir dir_name -> dir_size (name ^ "/" ^ dir_name) adj_list
  in
  AdjList.find_exn adj_list name
  |> List.fold_right ~init:0 ~f:(fun n acc -> acc + node_size n)

let () =
  let lines = In_channel.read_lines "./input7.txt" in
  let parsed = List.map ~f:parse_line lines in
  let adj_list =
    build_adjacency_list (List.drop parsed 1) AdjList.empty [ "/" ]
  in
  let dir_size_map =
    AdjList.mapi ~f:(fun ~key ~data:_ -> dir_size key adj_list) adj_list
  in
  let part1 =
    AdjList.filter ~f:(fun x -> x <= 100000) dir_size_map
    |> AdjList.fold ~init:0 ~f:(fun ~key:_ ~data acc -> acc + data)
  in
  let total_used_space = AdjList.find_exn dir_size_map "/" in
  let space_needed = 30000000 - (70000000 - total_used_space) in
  let () = printf "t: %d, n: %d\n" total_used_space space_needed in
  let part2 =
    AdjList.data dir_size_map
    |> List.filter ~f:(fun x -> x >= space_needed)
    |> List.min_elt ~compare:Int.compare
    |> Option.value_exn
  in
  printf "\nPart 1: %d\n" part1;
  printf "Part 2: %d\n" part2
