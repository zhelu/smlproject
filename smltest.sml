use "parse.sml";
val inputFile = "sml.txt";

fun readList (inputFile : string) =
  let val ins = TextIO.openIn inputFile
      fun loop i =
            case TextIO.inputLine i
              of SOME line => line :: loop i
               | NONE => []
  in loop ins before TextIO.closeIn ins
  end;

val files = map (fn s => String.extract (s, 0, SOME (size s - 1)))
              (readList inputFile);

val parseTrees =
      List.foldl
        (fn (f, acc) => let val p = SOME (ParseFile.readFile f) handle _ => NONE
                        in (case p 
                              of SOME x => x :: acc
                               | NONE => acc)
                        end) [] files;

val decList = List.foldl (op @) []
                (map (fn p => ParseFile.findDec ParseFile.allDec p) parseTrees);

val numDecs = length decList;
val decCounts = List.foldl (fn (dtype,acc) =>
                             (dtype, length
                                      (List.filter (fn d' =>
                                                     ParseFile.decToDecType d' =
                                                       dtype)
                                        decList)) :: acc) [] ParseFile.allDec;

val expList = List.foldl (op @) []
                (map (fn p => ParseFile.findExp ParseFile.keyExp p) parseTrees);

val expCounts = List.foldl (fn (etype,acc) =>
                             (etype, length
                                      (List.filter (fn e' =>
                                                     ParseFile.expToExpType e' =
                                                       etype)
                                        expList)) :: acc) [] ParseFile.keyExp;

val numRecFuns = length (List.filter ParseFile.isRecursiveFun decList);
