# Part A Practice Test

## Question 1

Check a box if and only if it is an accurate description of ML

Function arguments are evaluated before being passed to functions.[Correct]

ML is dynamically scoped. [Wrong]

All functions can be called recursively.[Wrong]

Functions are first-class expressions.[Correct]

## Question 2

Check a box if and only if it is an example of unnecessary function wrapping

```ml
fun increment x = x + 1;
```

```ml
(* pick this *)
fun map x y = List.map x y;
```

```ml
fun foo f xs = 1 +
        foldr (fn (x,y) => x * (y+1))  0 (map f xs)
```

```ml
fun bar xs = if xs = []
             then 0
             else 1 + bar xs
```

## Question 3

Lexical scoping is a crucial part of code execution in many programming languages, including ML. 

For each statement below, check the box if and only if the statement is true regarding this ML code.

Consider each statement after the identified line is executed.

```ml
1-   val x = 50
2-   val y = 3
3-   val z = 10
4-   val f = fn z => z
5-   val a = 
6-   let 
7-     val x = 3*x
8-     val z = y*z
9-   in
10-    x*z
11-  end
12-  fun f x z = x + y + z
13 - 
```

On line 4, the variable `z` inside the function body is bound to `10`. [Wrong]

On line 7, `x` is bound to `150`. [Right]

On line 8, `z` is bound to `30`. [Right]

On line 10, `z` is bound to `10`. [Wrong]

On line 12, the variable `x` inside the function body is bound to `5.[Wrong]

On line 12, the variable `y` inside the function body is bound to `3`. [Right]

On line 13, `x` is bound to `50`. [Right]

## Question 4

For each type below, check the box if and only if the type is a valid type for the function `foo`. Do not only select the most general type, also select less general types.

```ml
fun foo f x y z = 
    if x >= y
    then (f z)
    else foo f y x (tl z)
```

`(int -> real) -> int -> int -> int -> real`

`(string list -> bool list) -> int -> int -> string list -> bool list` [Correct]

`(’a list -> ’b list) -> int -> int -> ’a list -> ’b list` [Correct]

`(int list -> ’b list) -> int -> int -> ’b list -> int list`

`(’a list -> string list) -> int -> int -> ’a list -> ’a option list`

## Question 5

Question 5
Several correct implementations of the factorial function appear below.  Check the box next to a definition if and only if all recursive functions calls (possibly including recursive helper functions) are tail calls.

```ml
fun factorial i = 
  if i = 0 
  then 1 
  else i * factorial (i - 1)
```

```ml
(* Pick this: tail recursive *)
fun factorial i = 
let 
  fun factorialhelper (accum,i) = 
    if i = 0 
    then accum 
    else factorialhelper (accum*i, i-1) 
in 
  factorialhelper (1,i)
end 
```

```ml
fun factorial i = 
let 
  fun factorialhelper (start,i) = 
    if start <> i
    then start * factorialhelper (start+1, i)
    else start 
in 
  if i=0
  then 1
  else factorialhelper (1,i)
end 
```

```ml
fun factorial i = 
  case i of 
      0 => 1 
    | x => x * factorial (i-1)
```

## Question 6

Partial application involves passing less than the full number of arguments to a curried function. 

Given the curried function below, check the box if and only if the given function call is paired with a correct type for the returned function.

```ml
fun baz f a b c d e = (f (a ^ b))::(c + d)::e
```

```ml
(* pick this *)
Call: baz (fn z => 3)
Return type: string -> string -> int -> int -> int list -> int list
```

```ml
(* pick this *)
Call: baz (fn z => 10) "foo"
Return type: string -> int -> int -> int list -> int list
```

```ml
Call: baz (fn z => 10) "foo"
Return type: int -> string -> int -> int list -> int list
```

```ml
(* pick this *)
Call: baz (fn z => 10) "foo" "bar"
Return type: int -> int -> int list -> int list
```

## Question 7

Consider the two functions `maybeEven` and `maybeOdd` below, which are mutually recursive. For each statement below, check the box if and only if the statement is true regarding this ML code. Notice that these functions have some unconventional behaviour.

```ml
fun maybeEven x = 
	if x = 0 
	then true
	else
	if x = 50
	then false
	else maybeOdd (x-1)

and maybeOdd y =
	if y = 0
	then false
	else 
	if y = 99
	then true
	else maybeEven (y-1)
```

Evaluation of the call maybeEven 50 requires 25 calls to maybeOdd. [Wrong]

The call maybeOdd ~1 does not terminate. [Correct]

The call `maybeEven` 1 does not terminate. [Wrong]

Evaluation of the call `maybeOdd` 6 requires 3 calls to `maybeEven`. [Wrong]

Every call from `maybeEven` to `maybeOdd` or from `maybeOdd` to `maybeEven` is a tail call. [Correct]

Evaluating any call to maybeEven will always involve a call to maybeOdd. [Wrong]

The functions `maybeEven` and `maybeOdd` have the same type. [Correct]

For input x > 50, `maybeEven` always returns false. [Wrong]

The return types of maybeEven and maybeOdd are different. [Wrong]

## Question 8

The next three questions, including this one, relate to this situation: Types are often abstract representations for real world values. For each problem below, decide which type is the best choice to represent the given data.

This problem: Values of the type will represent multiple country names.

`int`

`string`

`int list`

`string list` [Pick This]

`(string * int) list`

## Question 9

This problem: Values of the type will hold a person's last name.

`int`

`string` [Pick This]

`int list`

`string list`

`(string * int) list`

## Question 10

This problem: Values of the type will hold a collection of student names and their grades on an assignment.

`int`

`string`

`int list`

`string list`

`(string * int) list` [Pick This]

## Question 11

The next 5 questions, including this one, are similar. Each question uses a slightly different definition of an ML signature `DIGIT` with the same structure definition `Digit` below. The `Digit` structure implements one-digit numbers that wrap around when you increment or decrement them.

```ml
structure Digit :> DIGIT =
struct
type digit = int
exception BadDigit
exception FailTest
fun make_digit i = if i < 0 orelse i > 9 then raise BadDigit else i
fun increment d = if d=9 then 0 else d+1
fun decrement d = if d=0 then 9 else d-1
val down_and_up = increment o decrement (* recall o is composition *)
fun test d = if down_and_up d = d then () else raise FailTest
end
```

In each problem, the definition of `DIGIT` matches the structure definition `Digit`, but different signatures let clients use the structure in different ways. You will answer the same question for each `DIGIT` definition by choosing the best description of what it lets clients do. In this question, the definition of `DIGIT` is:

```ml
signature DIGIT = 
sig
type digit = int
val make_digit : int -> digit
val increment : digit -> digit
val decrement : digit -> digit
val down_and_up : digit -> digit
val test : digit -> unit
end
```

The type-checker prevents the client from calling `Digit.test` with the expression `Digit.test e`, for any expression `e` that evaluates to a value `v`.

There are calls by clients to `Digit.test` that can type-check, but `Digit.test 10` does not type-check.

The client call `Digit.test 10` type-checks and causes the `Digit.FailTest` exception to be raised. [Correct]

The client call `Digit.test 10` type-checks and evaluates without raising an exception.

## Question 12

In this question, the definition of DIGIT is:
```ml
signature DIGIT = 
sig
type digit = int
val make_digit : int -> digit
val increment : digit -> digit
val decrement : digit -> digit
val down_and_up : digit -> digit
end
```
The type-checker prevents the client from calling `Digit.test` with the expression `Digit.test e`, for any expression `e` that evaluates to a value `v`.[Correct]

There are calls by clients to `Digit.test` that can type-check, but `Digit.test 10` does not type-check.

The client call `Digit.test 10` type-checks and causes the `Digit.FailTest` exception to be raised.

The client call `Digit.test 10` type-checks and evaluates without raising an exception.

## Question 13

In this question, the definition of `DIGIT` is:

```ml
signature DIGIT = 
sig
type digit = int
val make_digit : int -> digit
val increment : digit -> digit
val decrement : digit -> digit
val test : digit -> unit
end
```

The type-checker prevents the client from calling `Digit.test` with the expression `Digit.test e`, for any expression `e` that evaluates to a value `v`.

There are calls by clients to `Digit.test` that can type-check, but `Digit.test 10` does not type-check.

The client call `Digit.test 10` type-checks and causes the `Digit.FailTest` exception to be raised. [Correct]

The client call `Digit.test 10` type-checks and evaluates without raising an exception.

## Question 14

In this question, the definition of `DIGIT` is:

```ml
signature DIGIT = 
sig
type digit
val make_digit : int -> digit
val increment : digit -> digit
val decrement : digit -> digit
val down_and_up: digit -> digit
val test : digit -> unit
end
```

The type-checker prevents the client from calling `Digit.test` with the expression `Digit.test e`, for any expression `e` that evaluates to a value `v`.

There are calls by clients to `Digit.test` that can type-check, but `Digit.test 10` does not type-check. [Correct]

The client call `Digit.test 10` type-checks and causes the `Digit.FailTest` exception to be raised. 

The client call `Digit.test 10` type-checks and evaluates without raising an exception.

## Question 15

In this question, the definition of `DIGIT` is:

```ml
signature DIGIT = 
sig
type digit
val make_digit : digit -> digit
val increment : digit -> digit
val decrement : digit -> digit
val down_and_up: digit -> digit
val test : digit -> unit
end
```

The type-checker prevents the client from calling `Digit.test` with the expression `Digit.test e`, for any expression `e` that evaluates to a value `v`. [Correct]

There are calls by clients to `Digit.test` that can type-check, but `Digit.test 10` does not type-check.

The client call `Digit.test 10` type-checks and causes the `Digit.FailTest` exception to be raised. 

The client call `Digit.test 10` type-checks and evaluates without raising an exception.