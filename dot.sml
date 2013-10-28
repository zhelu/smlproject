use "manyfiles.sml";

signature DOT = sig
  (* Given a counter of paths of nesting for declarations, write the
   * corresponding graph in dot format to a file with the given filename *)
  val levelPathCounterToDotGraph :
    (ParseFile.dectype list) ParseFile.counter -> string -> unit

end

structure Dot :> DOT = struct
  local
    val header = "digraph G {\n"
    val footer = "}"
  in fun levelPathCounterToDotGraph (_, levelPathCounts) (file : string) =
       let val edges =
             List.foldl
               (fn ((x,_),a) =>
                 if length x = 1 then a
                 else let val l = length x - 1
                      in a ^ "  " ^
                           String.concat
                             (map ParseFile.decTypeToString
                               (List.take (x, l))) ^
                           " -> " ^
                           String.concat (map ParseFile.decTypeToString x) ^
                           ";\n"
                      end)
               "" levelPathCounts
           val nodes =
             List.foldl
               (fn ((x,i),a) => a ^ "  " ^
                 (String.concat (map ParseFile.decTypeToString x)) ^
                 " [label=\"" ^ (ParseFile.decTypeToString (List.last x)) ^
                 " " ^ (Int.toString i) ^ "\"];\n")
               "" levelPathCounts
           val outstrm = TextIO.openOut file
       in TextIO.output (outstrm, header ^ edges ^ nodes ^ footer)
            before TextIO.closeOut outstrm
       end
  end
end
