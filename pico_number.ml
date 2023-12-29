(* TODO WARNING some of these pico_number functions probably have mistakes*)

type t = Int32.t [@@deriving show, ord]
type outer_t = t [@@deriving show, ord]

(*
Binary representation of PICO-8 numbers:
-1.0 0xffff_0000
-0.5 0xffff_8000
-0.1 0xffff_E667
-0.0 0x0000_0000
 0.0 0x0000_0000
 0.1 0x0000_1999
 0.5 0x0000_8000
 1.0 0x0001_0000
*)

let whole_int_of (n : t) : int = Int32.shift_right n 16 |> Int32.to_int

let fraction_int_of (n : t) : int =
  Int32.shift_right_logical (Int32.shift_left n 16) 16 |> Int32.to_int

let float_of (n : t) : float =
  let whole = Float.of_int @@ whole_int_of n in
  let fraction = Float.of_int @@ fraction_int_of n in
  whole +. (fraction /. 65536.)

let pp (f : Format.formatter) (n : t) =
  Format.fprintf f "(pico_number %d + (%d / 65536))" (whole_int_of n)
    (fraction_int_of n)

let of_ints (whole_n : int) (fraction_n : int) : t =
  assert (whole_n >= -32768 && whole_n < 32768);
  assert (fraction_n >= 0 && fraction_n < 65536);
  let pico_n =
    Int32.logor
      (Int32.shift_left (Int32.of_int whole_n) 16)
      (Int32.of_int fraction_n)
  in
  assert (whole_int_of pico_n = whole_n);
  assert (fraction_int_of pico_n = fraction_n);
  pico_n

let of_int (n : int) : t = of_ints n 0

let rec of_float (n : float) : t =
  if n < 0. then Int32.neg @@ of_float (-.n)
  else of_ints (int_of_float n) (int_of_float ((n -. floor n) *. 65536.))

(* TODO this is probably not accurate *)
let of_string n = n |> float_of_string |> of_float

let int_of (n : t) : int =
  assert (fraction_int_of n = 0);
  whole_int_of n

let zero = of_int 0
let equal = Int32.equal
let below (v : t) = Int32.sub v (Int32.of_int 1)
let above (v : t) = Int32.add v (Int32.of_int 1)
let flr (n : t) : t = of_ints (whole_int_of n) 0
let ceil (n : t) : t = flr @@ Int32.add n @@ below @@ of_int 1

let flr_bits (b : int) (n : t) : t =
  assert (b >= 0 && b <= 16);
  let result = Int32.logand n @@ Int32.shift_left Int32.minus_one b in
  assert (result <= n);
  result

let ceil_bits (b : int) (n : t) : t =
  assert (b >= 0 && b <= 16);
  let mask = Int32.shift_left Int32.minus_one b in
  let flr = Int32.logand n mask in
  let ceil = Int32.logor flr @@ Int32.lognot mask in
  assert (ceil >= n);
  ceil

let min (a : t) (b : t) : t = Int32.min a b
let max (a : t) (b : t) : t = Int32.max a b
let add (a : t) (b : t) : t = Int32.add a b
let sub (a : t) (b : t) : t = Int32.sub a b

let mul (a : t) (b : t) : t =
  let result_high = Int64.mul (Int64.of_int32 a) (Int64.of_int32 b) in
  let result_low = Int64.shift_right result_high 16 in
  Int64.to_int32 result_low

let div (a : t) (b : t) : t =
  let a_high = Int64.shift_left (Int64.of_int32 a) 16 in
  let result = Int64.div a_high (Int64.of_int32 b) in
  Int64.to_int32 result

let modulo (a : t) (b : t) : t =
  (* TODO not sure if this is correct *)
  let a_whole = whole_int_of a in
  assert (a_whole >= 0);
  let a_fraction = fraction_int_of a in
  assert (a_fraction >= 0);
  let b = int_of b in
  assert (b > 0);
  of_ints (a_whole mod b) a_fraction

let neg (n : t) : t = Int32.neg n

let abs (n : t) : t =
  if n >= Int32.zero then n
  else
    let neg_n = neg n in
    if Int32.compare neg_n n <= 0 then
      failwith "abs not well defined for this number";
    neg_n
