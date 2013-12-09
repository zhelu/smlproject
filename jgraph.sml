(* This structure contains utilities for converting data into graphs. *)
signature JGRAPH = sig

  (* Given a list of filenames, a pair indicating a lower limit and an upper
   * limit (both inclusive), a title, and a file name, write to that file
   * a histogram showing the fraction of lines at least as wide as the numbers
   * in the low-to-high interval. *)
  val writeWidthHistogramToFile : string list -> (int * int) -> string
                                    -> string -> unit

end

structure JGraph :> JGRAPH = struct

  (* see signature TODO *)
  fun writeWidthHistogramToFile files (lo, hi) title filename =
    let
      val totalLines = ManyFiles.countLines files
      val outstrm = TextIO.openOut filename
      val diff = if hi >= lo then hi - lo + 1 else raise General.Subscript
      val footer = "yaxis label : Fraction of lines\ntitle : Fraction of " ^
                      "Lines With Minimum Width (" ^ title ^ ")"
      fun createSeq lo hi acc =
        if lo = hi then lo :: acc else createSeq lo (hi - 1) (hi :: acc)
      val seq = createSeq lo hi []
      fun fractionToString size =
            Real.toString (Real.fromInt size / Real.fromInt totalLines)
      fun graph1Bar limit size = (Int.toString limit) ^ " " ^
                                    (fractionToString size) ^ "\n"
      val counts = Counter.counterToList (ManyFiles.getLineWidthCounts files)
      fun getCountsGreaterThan limit =
            List.foldl (fn ((_, x), a) => x + a) 0
              (List.filter (fn (x, _) => x >= limit) counts)
      val body = String.map (fn #"~" => #"-" | x => x)
                   (String.concat
                     (map (fn x => graph1Bar x (getCountsGreaterThan x)) seq))
      val header = "newgraph\nxaxis\n" ^ "min " ^
                      (Real.toString (Real.fromInt lo - 0.9)) ^
                      " max " ^ (Real.toString (Real.fromInt (hi + 1) - 0.1)) ^
                      "\nsize 3.5\nhash 1 mhash 0 no_auto_hash_labels\nyaxis\n" ^
                        "min 0 max " ^
                        (fractionToString (2 * getCountsGreaterThan (hd seq))) ^
                        "\nsize 2\nprecision 3\nnewcurve\n" ^ 
                        "marktype xbar cfill 1 0 0 marksize 0.8\npts\n"
      fun limitToLabel limit =
        let
          val l = Int.toString limit
        in
          "hash_label at " ^ l ^ " : " ^ l ^ "\n"
        end
      val labels = "xaxis\n" ^
                     String.concat (map limitToLabel seq) ^
                     "hash_labels hjl vjc font Helvetica rotate -90\n"
    in
      TextIO.output (outstrm, header ^ body ^ labels ^ footer)
        before TextIO.closeOut outstrm
    end

end
