(* This structure contains utilities for converting data into graphs. *)
signature JGRAPH = sig

  (* Given a list of filenames, a pair indicating a lower limit and an upper
   * limit (both inclusive), a flag (true for log scale),
   * a title, and a file name, write to that file
   * a histogram showing the fraction of lines at least as wide as the numbers
   * in the low-to-high interval. *)
  val writeWidthHistogramToFile : string list -> (int * int) -> bool -> string
                                    -> string -> unit

end

structure JGraph :> JGRAPH = struct

  (* see signature *)
  fun writeWidthHistogramToFile files (lo, hi) logFlag title filename =
    let
      val totalLines = ManyFiles.countLines files
      val outstrm = TextIO.openOut filename
      (* throw an error if the lo > hi *)
      val _ = if hi >= lo then hi - lo + 1 else raise General.Subscript
      (* prints a label for the left y-axis and a title *)
      val footer = "yaxis label : Fraction of lines\ntitle : " ^
                      (if logFlag then "Log Plot of " else "") ^
                      "Fraction of " ^
                      "Lines With Minimum Width (" ^ title ^ ")\n"
      (* A sequence of values for the x-axis *)
      val seq = 
        let
          fun createSeq lo hi acc =
            if lo = hi then lo :: acc else createSeq lo (hi - 1) (hi :: acc)
        in
          createSeq lo hi []
        end
      (* converts a size to a string representing the fraction *)
      fun fractionToString size =
            Real.toString (size / Real.fromInt totalLines)
      (* graph a single bar *)
      fun graph1Bar limit size = "  " ^ (Int.toString limit) ^ " " ^
                                    (fractionToString (Real.fromInt size))
                                    ^ "\n"
      (* the counter with data *)
      val counts = Counter.counterToList (ManyFiles.getLineWidthCounts files)
      (* get the count of all items greater than or equal to argument *)
      fun getCountsGreaterThan limit =
            List.foldl (fn ((_, x), a) => x + a) 0
              (List.filter (fn (x, _) => x >= limit) counts)
      (* print the data for the bars *)
      val body = String.map (fn #"~" => #"-" | x => x)
                   (String.concat
                     (map (fn x => graph1Bar x (getCountsGreaterThan x)) seq))
      (* maximum for the left axis (fraction of lines) *)
      val height = 1.1 * Real.fromInt (getCountsGreaterThan (hd seq))
      (* minimum height for the axis in a log plot
       * (log 0 = -inf, so we can't print that...) *)
      val minHeight =
        let
          val l = List.last seq
        in
          0.5 * Real.fromInt (getCountsGreaterThan l)
        end
      (* maximum for the right axis (number of lines) *)
      val absHeight =
        let
          fun roundDownToZeros x i =
            if x div (10 * i) = 0 then
              x - (x mod i)
            else roundDownToZeros x (10 * i)
        in
          roundDownToZeros (getCountsGreaterThan (hd seq)) 1
        end
      (* This prints out axes for the fractions and the x-axis *)
      val header = "newgraph\nxaxis\n" ^ "min " ^
                      (Real.toString (Real.fromInt lo - 0.9)) ^
                      " max " ^ (Real.toString (Real.fromInt (hi + 1) - 0.1)) ^
                      "\n  size 3.5\n  hash 1 mhash 0 no_auto_hash_labels\n" ^
                      "yaxis\n  min " ^
                      (if logFlag then fractionToString minHeight else "0") ^
                      " max " ^
                      (fractionToString height) ^
                      (if logFlag then "\n  log" else "") ^
                      "\n  size 2\n  precision 2\nnewcurve\n" ^ 
                      "  marktype xbar cfill 1 0 0 marksize 0.8\npts\n"
      (* This prints out labels for the lines on the right y-axis *)
      val rightLabels =
        let
          val prepender = 
            if Real.fromInt absHeight < 0.67 * height then
              "  hash_at " ^
              ((fractionToString o Real.fromInt) (absHeight + absHeight div 2))
              ^ "\n  hash_label at " ^
              ((fractionToString o Real.fromInt) (absHeight + absHeight div 2))
              ^ " : " ^ (Int.toString (absHeight + absHeight div 2))
            else ""
          val hashSeq = if logFlag then
                          List.filter
                            (fn x => absHeight div x > Real.ceil minHeight)
                            [1,4,16,64,256]
                        else [1,2]
          (* put a hash and label at the height divided by the argument *)
          fun makeHash x =
            "hash_at " ^
            ((fractionToString o Real.fromInt) (absHeight div x)) ^
            "\nhash_label at " ^
            ((fractionToString o Real.fromInt) (absHeight div x)) ^
            " : " ^ (Int.toString (absHeight div x)) ^ "\n"
        in
          prepender ^ String.concat (map makeHash hashSeq)
        end
      (* This prints right axis and inserts the labels from above *)
      val rightAxis = "newgraph\nxaxis\n" ^ "  min " ^
                      (Real.toString (Real.fromInt lo - 0.9)) ^
                      " max " ^ (Real.toString (Real.fromInt (hi + 1) - 0.1)) ^
                      " nodraw\n  size 3.5\n  hash 1 mhash 0 " ^
                      "no_auto_hash_labels\nyaxis\n"^
                      "  min " ^
                      (if logFlag then fractionToString minHeight else "0") ^
                      " max " ^
                      (fractionToString height) ^
                      (if logFlag then "\n  log" else "") ^
                      "\n  size 2\n  precision 0\n  draw_at " ^
                      (Real.toString (Real.fromInt (hi + 1) - 0.1)) ^
                      " hash_scale +1.0\n  no_auto_hash_labels\n" ^
                      "  no_auto_hash_marks\n" ^
                        rightLabels ^
                        "  label : Number of Lines\n"
      (* This prints labels for the x axis *)
      val labels =
        let
          fun limitToLabel limit =
            let
              val l = Int.toString limit
            in
              "  hash_label at " ^ l ^ " : " ^ l ^ "\n"
            end
        in
         "xaxis\n" ^ String.concat (map limitToLabel
                                     (List.filter (fn x => x mod 2 = 0) seq)) ^
         "  hash_labels hjl vjc font Helvetica rotate -90\n"
        end
      (* Linear least squares r^2 *)
      val r2 =
        let
          val data = map (fn x => (Real.fromInt x, Math.log10
                                    (Real.fromInt (getCountsGreaterThan x) /
                                      Real.fromInt totalLines))) seq
          val n = Real.fromInt (length data)
          val xy = (List.foldr (fn ((x,y), a) => x * y + a) 0.0 data) / n
          val x = (List.foldr (fn ((x,y), a) => x + a) 0.0 data) / n
          val y = (List.foldr (fn ((x,y), a) => y + a) 0.0 data) / n
          val x2 = (List.foldr (fn ((x,y), a) => x * x+ a) 0.0 data) / n
          val y2 = (List.foldr (fn ((x,y), a) => y * y + a) 0.0 data) / n
        in
          "newstring\nx " ^ (Int.toString ((lo + hi + hi) div 3)) ^
          " y " ^ (fractionToString (height / 2.0)) ^ " : R-squared = " ^
          (Real.fmt (StringCvt.FIX (SOME 4))
             ((xy - x * y) * (xy - x * y) / ((x2 - x * x) * (y2 - y * y))))
        end
    in
      TextIO.output (outstrm, header ^ body ^ labels ^ footer ^ rightAxis ^
                      (if logFlag then r2 else ""))
        before TextIO.closeOut outstrm
    end

end
