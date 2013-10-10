use "parsefile.sml";

signature JGRAPH = sig

  (* take a counter and produce a string for a Zipf plot in jgraph format *)
  val counterToZipf : ''a ParseFile.counter -> string
end

structure JGraph :> JGRAPH = struct

  (* see signature *)
  fun counterToZipf decs =
    let val counts = map (fn (_, x) => x) decs
        val sortedCounts = ListMergeSort.sort (op <) counts
        val coords = List.foldl (fn (x, acc) => ((length acc) + 1, x) :: acc)
                       [] sortedCounts
        val ptsString = List.foldr
                         (fn ((r, c), acc) =>
                           let val c = if c = 0 then 1 else c
                           in (Real.toString (Math.ln (Real.fromInt r))) ^ " " ^
                                (Real.toString (Math.ln (Real.fromInt c))) ^
                                  "\n" ^ acc
                           end) "" coords
        val header = "newgraph\nnewcurve pts\n"
    in header ^ ptsString
    end
end
