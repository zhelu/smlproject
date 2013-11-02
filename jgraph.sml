use "counter.sml";

signature JGRAPH = sig

  (* take a counter, a title, and a filename to write jgraph format of a Zipf
   * file to file*)
  val counterToZipf : ''a Counter.counter -> string -> string -> unit
end

structure JGraph :> JGRAPH = struct

  (* see signature *)
  fun counterToZipf (f, decs) title file =
    let
      val counts = map (fn (_, x) => x) decs
      val sortedCounts = ListMergeSort.sort (op <) counts
      val coords = List.foldl (fn (x, acc) => ((length acc) + 1, x) :: acc)
                     [] sortedCounts
      val ptsString = List.foldr
                        (fn ((r, c), acc) =>
                          let
                            val c = if c = 0 then 1 else c
                          in
                            (Real.toString (Math.ln (Real.fromInt r))) ^ " " ^
                              (Real.toString (Math.ln (Real.fromInt c))) ^
                                "\n" ^ acc
                          end) "" coords
      val header = "newgraph\nxaxis label : log rank\n" ^
                     "yaxis label : log count\n" ^
                     "title fontsize 16 : " ^ title ^ "\n" ^
                     "newcurve pts\n"
      val outstrm = TextIO.openOut file
    in
      TextIO.output (outstrm, header ^ ptsString)
        before TextIO.closeOut outstrm
    end
end
