open! Core
open! Angstrom

type monkey_id = int
type worry_level = Big_int.big_int

type monkey = {
  id : monkey_id;
  items : worry_level list;
  operation : worry_level -> worry_level;
  test : worry_level -> monkey_id;
  n_inspected : int;
}

type state = monkey array

let integer_string_p = take_while1 (function '0' .. '9' -> true | _ -> false)
let integer_p = integer_string_p >>| int_of_string
let whitespace = take_while1 (function ' ' -> true | _ -> false)
let item_list_p = sep_by1 (string ", ") integer_p
let monkey_id_p = string "Monkey " *> integer_p <* char ':' <* end_of_line

let starting_items_p =
  List.map ~f:Big_int.big_int_of_int
  <$> (whitespace *> string "Starting items: " *> item_list_p <* end_of_line)

let operand_p =
  whitespace *> string "Operation: new = old " *> (char '+' <|> char '*')
  <* char ' '

let op_p =
  map2 operand_p
    (string "old" <|> integer_string_p <* end_of_line)
    ~f:(fun op target ->
      let f =
        match op with
        | '+' -> Big_int.add_big_int
        | '*' -> Big_int.mult_big_int
        | _ -> failwith "impossible"
      in
      match target with
      | "old" -> fun x -> f x x
      | y -> fun x -> f x (Big_int.big_int_of_string y))

let test_div_p =
  whitespace *> string "Test: divisible by " *> integer_p <* end_of_line

let test_t =
  whitespace *> string "If true: throw to monkey " *> integer_p <* end_of_line

let test_f =
  whitespace *> string "If false: throw to monkey " *> integer_p <* end_of_line

let test_p =
  map3 test_div_p test_t test_f ~f:(fun div t f worry ->
      if
        Big_int.eq_big_int
          (Big_int.mod_big_int worry (Big_int.big_int_of_int div))
          (Big_int.big_int_of_int 0)
      then t
      else f)

let monkey_p =
  map4 monkey_id_p starting_items_p op_p test_p ~f:(fun mid si op test ->
      { id = mid; items = si; operation = op; test; n_inspected = 0 })

let monkey_list_p = sep_by1 end_of_line monkey_p

let parse_monkey_list s =
  match parse_string ~consume:Consume.All monkey_list_p s with
  | Ok mlist -> mlist
  | Error msg -> failwith msg

let turn monkey state op =
  List.iter monkey.items ~f:(fun item ->
      let worry = op (monkey.operation item) in
      let target = monkey.test worry in
      let to_update = Array.get state target in
      let updated =
        { to_update with items = List.append to_update.items [ worry ] }
      in
      Array.set state target updated);
  Array.set state monkey.id
    {
      monkey with
      items = [];
      n_inspected = monkey.n_inspected + List.length monkey.items;
    }

let run_sim state rounds op =
  let n_monkeys = Array.length state in
  for _ = 1 to rounds do
    for i = 0 to n_monkeys - 1 do
      turn (Array.get state i) state op
    done
  done

let calc_monkey_biz state =
  Array.stable_sort state ~compare:(fun m1 m2 ->
      Int.compare m2.n_inspected m1.n_inspected);
  (Array.get state 0).n_inspected * (Array.get state 1).n_inspected

let () =
  let input = In_channel.read_all "./input11.txt" in
  let state1 = Array.of_list @@ parse_monkey_list input in
  let state2 = Array.of_list @@ parse_monkey_list input in
  run_sim state1 20 (fun x -> Big_int.div_big_int x @@ Big_int.big_int_of_int 3);
  printf "\nSolution Part 1: %d\n" @@ calc_monkey_biz state1;
  run_sim state2 10000 (fun x ->
      Big_int.mod_big_int x
      @@ Big_int.big_int_of_int (5 * 7 * 13 * 11 * 3 * 2 * 17 * 19));
  printf "Solution Part 2: %d\n" @@ calc_monkey_biz state2
