CM.make "compiler/Parse/parser.cm";

signature PARSEFILE = sig
  (* readFile filename returns the parse tree resulting from reading the source
   * in filename *)
  val readFile : string -> Ast.dec
  (* walkTree is a fold-like function for walking a parse tree *)
  val walkTree : (Ast.dec * 'a -> 'a) -> 'a -> Ast.dec -> 'a
  (* algebraic datatype to locally describe the types of declarations *)
  datatype dectype = MARKDEC | OPENDEC | TYPEDEC | OVLDDEC | SEQDEC | FIXDEC
                   | LOCALDEC | FSIGDEC | SIGDEC | FCTDEC | ABSDEC | STRDEC
                   | EXCEPTIONDEC | ABSTYPEDEC | DATAREPLDEC | DATATYPEDEC
                   | FUNDEC | VALDEC | VALRECDEC

  (* all declaration types except for MarkDec and SeqDec *)
  val allDec : dectype list

  (* countDec (curried) takes a list of declaration types (dectype) and a
   * parse tree and returns a list of declaration type / occurrence pairs. *)
  val countDec : dectype list -> Ast.dec -> (dectype * int) list

  (* findDec (curried) takes a list of declaration types (dectype) and a
   * a parse tree and returns a list of all nodes that match the input list
   * of types. *)
  val findDec : dectype list -> Ast.dec -> Ast.dec list

end

structure ParseFile :> PARSEFILE = struct

  val debug = false
  
  (* see signature *)
  fun readFile filename =
    let val src = Source.newSource (filename, TextIO.openIn filename, false,
                                     ErrorMsg.defaultConsumer ())
    in SmlFile.parse src
    end

  (* see signature *)
  fun walkTree f acc dec =
    (case dec
       of Ast.MarkDec (m, r) => walkTree f (f (dec, acc)) m
        | Ast.OpenDec paths =>
            (print (if debug then (Int.toString (List.length paths)
                                    ^ " open statement(s)\n") else "");
             f (dec, acc))
        | Ast.TypeDec types =>
            (print (if debug then (Int.toString (List.length types)
                                    ^ " type dec(s)\n") else "");
             f (dec, acc))
        | Ast.OvldDec (symbol, ty, exps) =>
            (print (if debug then ((Symbol.name symbol) ^ " overloaded\n")
                   else "");
             f (dec, acc))
        | Ast.SeqDec decs =>
            List.foldl (fn (dec, acc') => walkTree f acc' dec) acc decs
        | Ast.FixDec _ => (print (if debug then "fixity dec\n" else "");
                           f (dec, acc))
        | Ast.LocalDec (locals, body) =>
            (print (if debug then "local dec\n" else "");
             let val newAcc = walkTree f (f (dec, acc)) locals
             in walkTree f newAcc body
             end)
        | Ast.FsigDec _ => (print (if debug then "fsig dec\n" else "");
                            f (dec, acc))
        | Ast.SigDec _ => (print (if debug then "sig dec\n" else "");
                           f (dec, acc))
        | Ast.FctDec fctdecs =>
           (print (if debug then "funsig dec\n" else "");
            let fun walkStrExp f acc (Ast.BaseStr d) = walkTree f acc d
                  | walkStrExp f acc (Ast.MarkStr (strexp, _)) =
                      walkStrExp f acc strexp
                  | walkStrExp f acc (Ast.LetStr (letdec, strexp)) =
                      walkStrExp f (walkTree f acc letdec) strexp
                  | walkStrExp f acc _ = acc
                fun walkFctExp f acc (Ast.MarkFct (fctexp, _)) =
                      walkFctExp f acc fctexp
                  | walkFctExp f acc (Ast.BaseFct {body = strexp,...}) = 
                      walkStrExp f acc strexp
                  | walkFctExp f acc (Ast.LetFct (letdec, fctexp)) =
                      walkFctExp f (walkTree f acc letdec) fctexp
                  | walkFctExp f acc _ = acc
                fun walkFctDec f acc (Ast.MarkFctb (fctb, _)) =
                      walkFctDec f acc fctb
                  | walkFctDec f acc (Ast.Fctb {def = fctexp, ...}) =
                      walkFctExp f acc fctexp 
            in List.foldl (fn (dec, acc') => walkFctDec f acc' dec) acc fctdecs
            end)
        | Ast.AbsDec _ => (print (if debug then "abstr struct dec\n" else "");
                           f (dec, acc))
        | Ast.StrDec strdecs =>
           (print (if debug then "structure dec\n" else "");
            let fun walkStrExp f acc (Ast.BaseStr d) = walkTree f acc d
                  | walkStrExp f acc (Ast.MarkStr (strexp, _)) =
                      walkStrExp f acc strexp
                  | walkStrExp f acc (Ast.LetStr (letdec, strexp)) =
                      walkStrExp f (walkTree f acc letdec) strexp
                  | walkStrExp f acc _ = acc
                fun walkStrDec f acc (Ast.Strb {def = strexp, ...}) =
                      walkStrExp f acc strexp
                  | walkStrDec f acc (Ast.MarkStrb (strb, _)) = walkStrDec f acc strb
            in List.foldl (fn (dec, acc') => walkStrDec f acc' dec) acc strdecs
            end)
        | Ast.ExceptionDec _ => (print (if debug then "exn dec\n" else "");
                                 f (dec, acc))
        | Ast.AbstypeDec {body = d, ...} =>
            (print (if debug then "abst type dec\n" else "");
             walkTree f (f (dec, acc)) d) 
        | Ast.DataReplDec _ =>
            (print (if debug then "data replication dec\n" else "");
             f (dec, acc))
        | Ast.DatatypeDec _ =>
            (print (if debug then "datatype dec\n" else "");
             f (dec, acc))
        | Ast.FunDec _ => (print (if debug then "fun dec\n" else "");
                           f (dec, acc))
        | Ast.ValDec _ => (print (if debug then "val dec\n" else "");
                           f (dec, acc))
        | Ast.ValrecDec _ => (print (if debug then "valrec dec\n" else "");
                              f (dec, acc)))

  type 'a counter = ('a * int) list

  datatype dectype = MARKDEC | OPENDEC | TYPEDEC | OVLDDEC | SEQDEC | FIXDEC
                   | LOCALDEC | FSIGDEC | SIGDEC | FCTDEC | ABSDEC | STRDEC
                   | EXCEPTIONDEC | ABSTYPEDEC | DATAREPLDEC | DATATYPEDEC
                   | FUNDEC | VALDEC | VALRECDEC

  (* decToDecType is a essentially a one-to-one mapping between Ast.dec and
   * dectype *)
  fun decToDecType (Ast.ValDec _) = VALDEC
    | decToDecType (Ast.ValrecDec _) = VALRECDEC
    | decToDecType (Ast.FunDec _) = FUNDEC
    | decToDecType (Ast.TypeDec _) = TYPEDEC
    | decToDecType (Ast.DatatypeDec _) = DATATYPEDEC
    | decToDecType (Ast.DataReplDec _) = DATAREPLDEC
    | decToDecType (Ast.AbstypeDec _) = ABSTYPEDEC
    | decToDecType (Ast.ExceptionDec _) = EXCEPTIONDEC
    | decToDecType (Ast.StrDec _) = STRDEC
    | decToDecType (Ast.AbsDec _) = ABSDEC
    | decToDecType (Ast.FctDec _) = FCTDEC
    | decToDecType (Ast.SigDec _) = SIGDEC
    | decToDecType (Ast.FsigDec _) = FSIGDEC
    | decToDecType (Ast.LocalDec _) = LOCALDEC
    | decToDecType (Ast.SeqDec _) = SEQDEC
    | decToDecType (Ast.OpenDec _) = OPENDEC
    | decToDecType (Ast.OvldDec _) = OVLDDEC
    | decToDecType (Ast.FixDec _) = FIXDEC
    | decToDecType (Ast.MarkDec _) = MARKDEC

  (* type check *)
  val _ = op decToDecType : Ast.dec -> dectype

  (* increment takes an item of type ''a and a counter on ''a and increases the
   * count for the input object *)
  fun increment x [] = [(x, 1)]
    | increment x ((y, count) :: rest) =
        if (x = y) then (y, count + 1) :: rest
        else (y, count) :: increment x rest

  (* type check *)
  val _ = op increment : ''a -> ''a counter -> ''a counter

  (* see signature *)
  fun countDec countedDecs parseTree =
    walkTree (fn (dec, acc) =>
               (case (List.find (fn x => x = decToDecType dec) countedDecs)
                  of NONE => acc
                   | SOME d => increment (decToDecType dec) acc)) [] parseTree

  (* type check *)
  val _ = op countDec : dectype list -> Ast.dec -> dectype counter

  (* see signature *)
  fun findDec targetDecs parseTree =
    walkTree (fn (dec, acc) =>
               (case (List.find (fn x => x = decToDecType dec) targetDecs)
                  of NONE => acc
                   | SOME d => dec :: acc)) [] parseTree

  (* type check *)
  val _ = op findDec : dectype list -> Ast.dec -> Ast.dec list

  (* type check *)
  val allDec = [OPENDEC, TYPEDEC, OVLDDEC, FIXDEC, LOCALDEC, FSIGDEC, SIGDEC,
                FCTDEC, ABSDEC, STRDEC, EXCEPTIONDEC, ABSTYPEDEC, DATAREPLDEC,
                DATATYPEDEC, FUNDEC, VALDEC, VALRECDEC]

end
