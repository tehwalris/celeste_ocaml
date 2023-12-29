type t = T of (Pico_number.t * Pico_number.t) [@@deriving show, ord]

let of_number n = T (n, n)
let assert_valid (T (low, high)) = assert (low <= high)

(* TODO this probably needs overflow checks *)

let add a b =
  assert_valid a;
  assert_valid b;
  let (T (a_low, a_high)) = a in
  let (T (b_low, b_high)) = b in
  T (Pico_number.add a_low b_low, Pico_number.add a_high b_high)

let sub a b =
  assert_valid a;
  assert_valid b;
  let (T (a_low, a_high)) = a in
  let (T (b_low, b_high)) = b in
  T (Pico_number.sub a_low b_high, Pico_number.sub a_high b_low)

let to_number_opt v =
  assert_valid v;
  let (T (low, high)) = v in
  if low = high then Some low else None
