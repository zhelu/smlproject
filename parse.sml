CM.make "compiler/Parse/parser.cm";

signature PARSEFILE = sig
  (* readFile filename returns the parse tree resulting from reading the source
   * in filename *)
  val readFile : string -> Ast.dec
  (* foldTree is a fold-like function for walking a parse tree *)
  val foldTree : (Ast.dec * 'a -> 'a) -> 'a -> Ast.dec -> 'a
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

  (* Given a declaration of FunDec, return the name of function we're binding *)
  val getFunName : Ast.dec -> string

  (* Given a declaration of FunDec, return true if it's a recursive function.
   * False otherwise. Does not correctly work if you shadow the parent function
   * with a let binding. *)
  val isRecursiveFun : Ast.dec -> bool

  (* Given a parse tree, find all recursive functions *)
  val getRecursiveFun : Ast.dec -> Ast.dec list

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
  fun foldTree f acc dec =
    (case dec
       of Ast.MarkDec (m, r) =>
            let val acc' = f (dec, acc)
            in foldTree f acc' m
            end
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
            List.foldl (fn (dec, acc') => foldTree f acc' dec) acc decs
        | Ast.FixDec _ => (print (if debug then "fixity dec\n" else "");
                           f (dec, acc))
        | Ast.LocalDec (locals, body) =>
            (print (if debug then "local dec\n" else "");
             let val acc' = f (dec, acc)
                 val newAcc = foldTree f acc' locals
             in foldTree f newAcc body
             end)
        | Ast.FsigDec _ => (print (if debug then "fsig dec\n" else "");
                            f (dec, acc))
        | Ast.SigDec _ => (print (if debug then "sig dec\n" else "");
                           f (dec, acc))
        | Ast.FctDec fctdecs =>
           (print (if debug then "funsig dec\n" else "");
                (* walkStrExp extracts declarations from a structure expression
                 * by applying foldTree on any Ast.dec types it finds *)
            let fun walkStrExp f acc (Ast.BaseStr d) = foldTree f acc d
                  | walkStrExp f acc (Ast.MarkStr (strexp, _)) =
                      walkStrExp f acc strexp
                  | walkStrExp f acc (Ast.LetStr (letdec, strexp)) =
                      walkStrExp f (foldTree f acc letdec) strexp
                  | walkStrExp f acc _ = acc
                (* walkFctExp pulls Ast.dec types out of a functor expression
                 * by processing the sub structure expression *)
                fun walkFctExp f acc (Ast.MarkFct (fctexp, _)) =
                      walkFctExp f acc fctexp
                  | walkFctExp f acc (Ast.BaseFct {body = strexp,...}) = 
                      walkStrExp f acc strexp
                  | walkFctExp f acc (Ast.LetFct (letdec, fctexp)) =
                      walkFctExp f (foldTree f acc letdec) fctexp
                  | walkFctExp f acc _ = acc
                (* walkFctDec pulls Ast.dec types out of a functor declaration
                 * by processing the sub functor expression *)
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
                (* walkStrExp extracts declarations from a structure expression
                 * by applying foldTree on any Ast.dec types it finds *)
            let fun walkStrExp f acc (Ast.BaseStr d) = foldTree f acc d
                  | walkStrExp f acc (Ast.MarkStr (strexp, _)) =
                      walkStrExp f acc strexp
                  | walkStrExp f acc (Ast.LetStr (letdec, strexp)) =
                      walkStrExp f (foldTree f acc letdec) strexp
                  | walkStrExp f acc _ = acc
                (* walkStrDec extracts Ast.dec types from a structure
                 * declaration by processing the sub structure expression *)
                fun walkStrDec f acc (Ast.Strb {def = strexp, ...}) =
                      walkStrExp f acc strexp
                  | walkStrDec f acc (Ast.MarkStrb (strb, _)) =
                      walkStrDec f acc strb
                val acc' = f (dec, acc)
            in List.foldl (fn (dec, acc) => walkStrDec f acc dec) acc' strdecs
            end)
        | Ast.ExceptionDec _ => (print (if debug then "exn dec\n" else "");
                                 f (dec, acc))
        | Ast.AbstypeDec {body = d, ...} =>
            (print (if debug then "abst type dec\n" else "");
             foldTree f (f (dec, acc)) d) 
        | Ast.DataReplDec _ =>
            (print (if debug then "data replication dec\n" else "");
             f (dec, acc))
        | Ast.DatatypeDec _ =>
            (print (if debug then "datatype dec\n" else "");
             f (dec, acc))
        | Ast.FunDec (fblist,_) =>
            (print (if debug then "fun dec\n" else "");
                  (* walkExp gets Ast.decs from an expression *)
              let fun walkExp f acc (Ast.LetExp {dec = d, ...}) =
                        foldTree f acc d
                    | walkExp f acc (Ast.MarkExp (exp, _)) =
                        walkExp f acc exp
                    | walkExp f acc (Ast.SeqExp exps) =
                        List.foldl (fn (exp, acc) => walkExp f acc exp)
                          acc exps
                    | walkExp f acc (Ast.FlatAppExp items) =
                        List.foldl
                          (fn ({item = exp, ...}, acc) => walkExp f acc exp)
                          acc items
                    | walkExp f acc _ = acc
                  (* walkClause gets Ast.decs from a clause by
                   * checking its expression for let bindings *)
                  fun walkClause f acc (Ast.Clause {exp = exp, ...}) =
                        walkExp f acc exp
                  (* walkFb gets Ast.dec types from a function definition
                     by processing its clauses for letting bindings *)
                  fun walkFb f acc (Ast.MarkFb (fb, _)) = walkFb f acc fb
                    | walkFb f acc (Ast.Fb (clauses, _)) =
                        List.foldl (fn (clause, acc) => walkClause f acc clause)
                          acc clauses
                  val acc' = f (dec, acc)
              in List.foldl (fn (fb, acc) => walkFb f acc fb) acc' fblist
              end)
        | Ast.ValDec (vblist, _) =>
            (print (if debug then "val dec\n" else "");
                  (* walkExp gets Ast.decs from an expression *)
              let fun walkExp f acc (Ast.LetExp {dec = d, ...}) =
                        foldTree f acc d
                    | walkExp f acc (Ast.MarkExp (exp, _)) =
                        walkExp f acc exp
                    | walkExp f acc (Ast.SeqExp exps) =
                        List.foldl (fn (exp, acc) => walkExp f acc exp)
                          acc exps
                    | walkExp f acc (Ast.FlatAppExp items) =
                        List.foldl
                          (fn ({item = exp, ...}, acc) => walkExp f acc exp)
                          acc items
                    | walkExp f acc _ = acc
                  (* walkVb gets Ast.decs from a val declaration by checking
                   * its expression for lets *)
                  fun walkVb f acc (Ast.MarkVb (vb, _)) = walkVb f acc vb
                    | walkVb f acc (Ast.Vb {exp = e, ...}) = walkExp f acc e
                  val acc' = f (dec, acc)
              in List.foldl (fn (vb, acc) => walkVb f acc vb) acc' vblist
              end)
        | Ast.ValrecDec (rvblist, _) =>
            (print (if debug then "valrec dec\n" else "");
                  (* walkExp gets Ast.decs from an expression *)
              let fun walkExp f acc (Ast.LetExp {dec = d, ...}) =
                        foldTree f acc d
                    | walkExp f acc (Ast.MarkExp (exp, _)) =
                        walkExp f acc exp
                    | walkExp f acc (Ast.SeqExp exps) =
                        List.foldl (fn (exp, acc) => walkExp f acc exp)
                          acc exps
                    | walkExp f acc (Ast.FlatAppExp items) =
                        List.foldl
                          (fn ({item = exp, ...}, acc) => walkExp f acc exp)
                          acc items
                    | walkExp f acc _ = acc
                  (* walkVb gets Ast.decs from a valrec declaration by checking
                   * its expression for lets *)
                  fun walkRvb f acc (Ast.MarkRvb (rvb, _)) = walkRvb f acc rvb
                    | walkRvb f acc (Ast.Rvb {exp = e, ...}) = walkExp f acc e
                  val acc' = f (dec, acc)
              in List.foldl (fn (rvb, acc) => walkRvb f acc rvb) acc' rvblist
              end))

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
    foldTree (fn (dec, acc) =>
               (case (List.find (fn x => x = decToDecType dec) countedDecs)
                  of NONE => acc
                   | SOME d => increment (decToDecType dec) acc)) [] parseTree

  (* type check *)
  val _ = op countDec : dectype list -> Ast.dec -> dectype counter

  (* see signature *)
  fun findDec targetDecs parseTree =
    foldTree (fn (dec, acc) =>
               (case (List.find (fn x => x = decToDecType dec) targetDecs)
                  of NONE => acc
                   | SOME d => dec :: acc)) [] parseTree

  (* type check *)
  val _ = op findDec : dectype list -> Ast.dec -> Ast.dec list

  (* see signature *)
  val allDec = [OPENDEC, TYPEDEC, OVLDDEC, FIXDEC, LOCALDEC, FSIGDEC, SIGDEC,
                FCTDEC, ABSDEC, STRDEC, EXCEPTIONDEC, ABSTYPEDEC, DATAREPLDEC,
                DATATYPEDEC, FUNDEC, VALDEC, VALRECDEC]

  (* see signature *)
  fun getFunName (Ast.FunDec (fb::_, _)) = 
            (* Given an Ast.FunDec, extract the first clause *)
        let fun findFirstClause (Ast.MarkFb (fb, _)) = findFirstClause fb
              | findFirstClause (Ast.Fb (clause::_, _)) = clause
            (* Given an Ast.Clause, extract the first pattern *)
            fun findFirstPattern (Ast.Clause {pats = p::_,...}) = p
            (* Pull the function name from the pattern *)
            fun findName {fixity = SOME (Symbol.SYMBOL (_, name)),
                           item=_, region=_} = name
        in (findName o findFirstPattern o findFirstClause) fb
        end
    | getFunName _ = ""

  (* see signature *)
  fun isRecursiveFun (dec as (Ast.FunDec (fblist, _))) =
        let val name = getFunName dec
               (* Check the list of expressions for a call to this function. *)
        in let fun checkExpForName (Ast.MarkExp (e, _)) = checkExpForName e
                 | checkExpForName (Ast.SeqExp es) =
                     List.foldl (fn (e, acc) => checkExpForName e orelse acc)
                       false es
                 | checkExpForName (Ast.VarExp path) =
                     List.exists (fn (Symbol.SYMBOL (_, n)) => n = name) path
                 | checkExpForName (Ast.FlatAppExp items) =
                     List.foldl (fn ({item = e, fixity = _, region = _}, acc) =>
                                    checkExpForName e orelse acc)
                                    false items
                 | checkExpForName _ = false
               (* In the function application, drill down into the item of
                * the fixity record. *)
               fun checkItemForName {item = e, fixity = _, region = _} =
                     checkExpForName e
               (* for each clause, look at the evaluated expression and
                * look for a function application. *)
               fun walkExp (Ast.LetExp {expr = e,...}) = walkExp e
                 | walkExp (Ast.CaseExp {rules = rules,...}) =
                     List.foldl (fn (Ast.Rule {exp = e,...}, acc) =>
                                  walkExp e orelse acc) false rules
                 | walkExp (Ast.IfExp {thenCase = t, elseCase = e, ...}) =
                     (walkExp t) orelse (walkExp e)
                 | walkExp (Ast.FlatAppExp items) =
                     List.foldl (fn (i, acc) => checkItemForName i orelse acc)
                       false items
                 | walkExp (Ast.MarkExp (e, _)) = walkExp e
                 | walkExp _ = false
               (* check for a call to this function inside all clauses *)
               fun walkClause (Ast.Clause {exp = e,...}) = walkExp e
               (* Take a function declaration, and return true if any of its
                * clauses call this function *)
               fun walkFb (Ast.MarkFb (fb, _)) = walkFb fb
                 | walkFb (Ast.Fb (clauses, _)) = 
                     List.foldl (fn (c, acc) => walkClause c orelse acc) false
                       clauses
           in List.foldl (fn (fb, acc) => walkFb fb orelse acc) false fblist
           end
        end
    | isRecursiveFun _ = false

  (* see signature *)
  fun getRecursiveFun parseTree =
        List.filter isRecursiveFun (findDec [FUNDEC] parseTree)

end
