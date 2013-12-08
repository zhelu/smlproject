(* This structure contains utilities for converting data into graphs. *)
signature JGRAPH = sig

  (* Given a counter of lines, write to file, in jgraph format, a histogram
   * showing the fraction of lines at least as wide as the numbers in the low
   * to hi interval. *)
  val writeWidthHistogramToFile : int Counter.counter -> string -> (int * int)
                                    -> unit

end

structure JGraph :> JGRAPH = struct

  (* see signature TODO *)
  fun writeWidthHistogramToFile ctr file (lo, hi) =
    let
      val header = ""
      val footer = ""
      val diff = if hi >= lo then hi - lo + 1 else raise List.Subscript
    in
      ()
    end

end
