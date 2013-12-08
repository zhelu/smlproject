(* This structure provides a counter for counting things using a simple
 * interface. *)
signature COUNTER = sig

  (* Maintains a count of items. See function getCountOf *)
  type 'a counter

  (* Create an empty counter *)
  val emptyCounter : 'a counter

  (* increment takes an item of type ''a and a counter on ''a and increases the
   * count for the input object by the specified integer *)
  val increment : ''a -> int -> ''a counter -> ''a counter

  (* Take two counters and merge their counts
   * Algebraic laws:
   * a merge b = b merge a
   * a merge empty = a *)
  val mergeCounters : ''a counter * ''a counter -> ''a counter

  (* Given a value and a counter, return the count of the input parameter *)
  val getCountOf : ''a -> ''a counter -> int

  (* Given a counter of a's, return a list of pairs of a's and their counts *)
  val counterToList : 'a counter -> ('a * int) list

  (* Given a list of pairs of a's and their counts, return a counter of a's *)
  val listToCounter : (''a * int) list -> ''a counter

end

structure Counter :> COUNTER = struct

  type 'a counter = ('a * int) list

  (* see signature *)
  fun getCountOf x [] = 0
    | getCountOf x ((y, c) :: rest) =
        if x = y then c else getCountOf x rest

  (* see signature *)
  fun increment x i [] = [(x, i)]
    | increment x i ((y, count) :: rest) =
        if x = y then
          (y, count + i) :: rest
        else (y, count) :: increment x i rest

  (* see signature *)
  val emptyCounter = []

  (* see signature *)
  fun mergeCounters (a, b) =
    List.foldl (fn ((x, i), acc) => increment x i acc) b a

  (* see signature *)
  fun counterToList cs = cs

  (* see signature *)
  fun listToCounter xs =
    List.foldr (fn ((x,i), acc) => increment x i acc) emptyCounter xs
end
