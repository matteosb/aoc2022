open! Core

type packet_data = PInt of int | PList of packet_data list [@@deriving show]
type packet_list = packet_data list [@@deriving show]
type packet_pair = packet_data * packet_data [@@deriving show]
type pair_list = packet_pair list [@@deriving show]

let parse_lines lines =
  Angstrom.(
    let integer_p =
      take_while1 (function '0' .. '9' -> true | _ -> false) >>| fun x ->
      PInt (int_of_string x)
    in
    let lst_of p = char '[' *> sep_by (char ',') p <* char ']' in
    let p_list x = (fun x -> PList x) <$> lst_of x in
    let packet_p = fix @@ fun self -> integer_p <|> p_list self in
    let pair_p = (fun l r -> (l, r)) <$> packet_p <* end_of_line <*> packet_p in
    let pair_list = sep_by (many end_of_line) pair_p in
    match parse_string ~consume:Consume.Prefix pair_list lines with
    | Ok res -> res
    | Error err -> failwith err)

let box_int = function PInt x -> PList [ PInt x ] | lst -> lst

let rec comp_packet = function
  | PInt il, PInt ir -> Int.compare il ir
  | PList lstl, PList lstr -> (
      match (lstl, lstr) with
      | [], [] -> 0
      | [], _ -> -1
      | _, [] -> 1
      | x :: xs, y :: ys ->
          let d = comp_packet (x, y) in
          if d = 0 then comp_packet (PList xs, PList ys) else d)
  | x, y -> comp_packet (box_int x, box_int y)

let is_ordered pair = comp_packet pair < 0

let solve_1 =
  List.foldi ~init:0 ~f:(fun idx acc p ->
      if is_ordered p then acc + idx + 1 else acc)

let divider_packets = [ PList [ PList [ PInt 2 ] ]; PList [ PList [ PInt 6 ] ] ]
let flatten_pairs = List.concat_map ~f:(fun (x, y) -> [ x; y ])

let solve_2 packet_pairs =
  let sorted =
    List.append (flatten_pairs packet_pairs) divider_packets
    |> List.sort ~compare:(fun x y -> comp_packet (x, y))
  in
  let find_idx t =
    let idx, _ = List.findi_exn sorted ~f:(fun _ p -> comp_packet (p, t) = 0) in
    idx + 1
  in
  find_idx (List.nth_exn divider_packets 0)
  * find_idx (List.nth_exn divider_packets 1)

let () =
  let lines = In_channel.read_all "./input13.txt" in
  let packets = parse_lines lines in
  printf "\nSolution Part 1: %d\n" @@ solve_1 packets;
  printf "Solution Part 2: %d\n" @@ solve_2 packets
