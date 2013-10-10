use "jgraph.sml";

signature MANYFILES = sig
  (* given a list of line separated file names,
   * product a list of these names *)
  val getListOfFiles : string -> string list

  (* given a list of files and a list of declarations, compile a tally of
   * the selected declaration types *)
  val getDecCount : string list ->
                      ParseFile.dectype list ->
                        ParseFile.dectype ParseFile.counter
end

structure ManyFiles :> MANYFILES = struct
  fun getListOfFiles listFile =
    let fun readList (inputFile : string) =
          let val ins = TextIO.openIn inputFile
              fun loop i =
                case TextIO.inputLine i
                  of SOME line => line :: loop i
                   | NONE => []
          in loop ins before TextIO.closeIn ins
          end
    in map (fn s => String.extract (s, 0, SOME (size s - 1)))
         (readList listFile)
    end

  fun getDecCount fileList decs =
    List.foldl
      (fn (f, acc) =>
        let val decCount = SOME (ParseFile.countDec decs
                             (ParseFile.readFile f)) handle _ => NONE
        in (case decCount
              of SOME dc => List.foldl
                   (fn (dtype, acc) =>
                     ParseFile.increment dtype
                       (ParseFile.getCountOf dtype dc) acc)
                   acc ParseFile.allDec
               | NONE => acc)
        end) ParseFile.emptyCounter fileList
end
