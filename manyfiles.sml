use "jgraph.sml";

signature MANYFILES = sig
  (* given a list of line separated file names,
   * product a list of these names *)
  val getFileList : string -> string list

  (* given a list of files and a list of declarations, compile a tally of
   * the selected declaration types for correctly parsed files *)
  val getDecCount : string list ->
                      ParseFile.dectype list ->
                        ParseFile.dectype ParseFile.counter

  (* Given a list of files, get a count of all top level declarations
   * in correctly parsed files *)
  val countTopLevelDecs : string list -> ParseFile.dectype ParseFile.counter

  (* Count total lines in correctly parsed files *)
  val countLines : string list -> int
end

structure ManyFiles :> MANYFILES = struct

  (* given a filename, produce SOME parse tree if the parsing is successeful
     or NONE if the parsing fails *)
  fun parseFile f = SOME (ParseFile.readFile f) handle _ => NONE

  (* type check *)
  val _ = op parseFile : string -> Ast.dec option

  infix >=

  (* >= sequences operations on option types *)
  fun (SOME s) >= f = SOME (f s)
    | NONE >= f = NONE

  (* type check *)
  val _ = op >= : ('a option * ('a -> 'b)) -> 'b option

  (* see signature *)
  fun getFileList listFile =
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

  (* see signature *)
  fun getDecCount fileList decs =
    List.foldl
      (fn (f, acc) =>
        let val decCount = parseFile f >= (fn p => ParseFile.countDec decs p)
        in (case decCount
              of SOME dc => 
                   ParseFile.mergeCounters (dc, acc)
               | NONE => acc)
        end) ParseFile.emptyCounter fileList

  (* see signature *)
  fun countTopLevelDecs fileList =
    List.foldl
      (fn (f, acc) =>
        let val decCount = parseFile f >=
                           ParseFile.getTopLevelDec >= 
                           (fn dc => map ParseFile.decToDecType dc) >=
                           (fn ds => ParseFile.countList ds
                                       ParseFile.emptyCounter)
        in (case decCount
              of SOME dc =>
                   ParseFile.mergeCounters (dc, acc)
               | NONE => acc)
        end) ParseFile.emptyCounter fileList

  (* see signature *)
  fun countLines fileList =
    let val okFiles = List.filter (fn f => parseFile f <> NONE) fileList
        val counts = map ParseFile.getLineCount okFiles
    in List.foldr (op +) 0 counts
    end
end
