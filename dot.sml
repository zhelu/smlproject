signature DOT = sig
  (* Given a counter of paths of nesting for declarations, write the
   * corresponding graph in dot format to a file with the given filename *)
  val levelPathCounterToDotGraph :
    (AstType.dectype list) Counter.counter -> string -> unit

end

structure Dot :> DOT = struct
  local
    val header = "digraph G {\n"
    val footer = "}"
  in
    fun levelPathCounterToDotGraph levelPathCounts (file : string) =
      let
        fun dectypeListComp ((xs, _), (ys, _)) =
          let
            val c = Int.compare (length xs, length ys)
          in
            if c = EQUAL then
              List.collate
                (fn (x,y) =>
                  String.compare
                    (AstType.decTypeToString x, AstType.decTypeToString y))
                  (xs,ys)
            else c
          end
        val counts' = ListMergeSort.uniqueSort dectypeListComp
                        (Counter.counterToList levelPathCounts)
        val edges =
          List.foldl
            (fn ((x,_),a) =>
              if length x = 1 then
                a
              else let
                     val l = length x - 1
                   in
                     a ^ "  " ^
                       String.concat
                         (map AstType.decTypeToString
                           (List.take (x, l))) ^
                       " -> " ^
                       String.concat (map AstType.decTypeToString x) ^
                       ";\n"
                   end)
            "" counts'
        val nodes =
          List.foldl
            (fn ((x,i),a) => a ^ "  " ^
              (String.concat (map AstType.decTypeToString x)) ^
              " [label=\"" ^ (AstType.decTypeToString (List.last x)) ^
              " " ^ (Int.toString i) ^ "\"];\n")
            "" counts'
        val outstrm = TextIO.openOut file
      in
        TextIO.output (outstrm, header ^ edges ^ nodes ^ footer)
          before TextIO.closeOut outstrm
      end
  end
end
