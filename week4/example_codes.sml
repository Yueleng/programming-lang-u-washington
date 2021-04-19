
signature COUNTER =
sig
    type t = int
    val newCounter : int -> t
    (* val increment : t -> t *)
    val first_larger : t * t -> bool
end

structure NoNegativeCounter :> COUNTER = 
struct

exception InvariantViolated

type t = int

fun newCounter i = if i <= 0 then 1 else i

fun increment i = i + 1

fun first_larger (i1,i2) =
    if i1 <= 0 orelse i2 <= 0
    then raise InvariantViolated    else (i1 - i2) > 0

end;



fun foo f x y z =
    if x >= y
    then (f z)
    else foo f y x (tl z);


   
fun baz f a b c d e = (f (a ^ b))::(c + d)::e;
