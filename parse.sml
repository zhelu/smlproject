CM.make "compiler/Parse/parser.cm";

signature PARSEFILE = sig

  (* readFile filename returns the parse tree resulting from reading the source
   * in filename *)
  val readFile : string -> Ast.dec

  (* foldDecs is a fold-like function for walking a parse tree. The tree-
   * traversal is DFS. *)
  val foldDecs : (Ast.dec * 'a -> 'a) -> 'a -> Ast.dec -> 'a

  (* algebraic datatype to locally describe the types of declarations *)
  datatype dectype = MARKDEC | OPENDEC | TYPEDEC | OVLDDEC | SEQDEC | FIXDEC
                   | LOCALDEC | FSIGDEC | SIGDEC | FCTDEC | ABSDEC | STRDEC
                   | EXCEPTIONDEC | ABSTYPEDEC | DATAREPLDEC | DATATYPEDEC
                   | FUNDEC | VALDEC | VALRECDEC

  datatype exptype = VAREXP | FNEXP | FLATAPPEXP | APPEXP | CASEEXP | LETEXP
                   | SEQEXP | INTEXP | WORDEXP | REALEXP | STRINGEXP | CHAREXP
                   | RECORDEXP | LISTEXP | TUPLEEXP | SELECTOREXP 
                   | CONSTRAINTEXP | HANDLEEXP | RAISEEXP | IFEXP | ANDALSOEXP
                   | ORELSEEXP | WHILEEXP | MARKEXP | VECTOREXP

  (* all declaration types except for MarkDec and SeqDec *)
  val allDec : dectype list

  (* countDec (curried) takes a list of declaration types (dectype) and a
   * parse tree and returns a list of declaration type / occurrence pairs. *)
  val countDec : dectype list -> Ast.dec -> (dectype * int) list

  (* findDec (curried) takes a list of declaration types (dectype) and a
   * a parse tree and returns a list of all declaration nodes that match the 
   * input list of types. *)
  val findDec : dectype list -> Ast.dec -> Ast.dec list

  (* Given a declaration of FunDec, return the name of function we're binding *)
  val getFunName : Ast.dec -> string

  (* Given a declaration of FunDec, return true if it's a recursive function.
   * False otherwise. Does not correctly work if you shadow the parent function
   * with a let binding. *)
  val isRecursiveFun : Ast.dec -> bool

  (* Given a parse tree, find all recursive functions *)
  val getRecursiveFun : Ast.dec -> Ast.dec list

  (* foldExprs is a fold-like function on expressions. The order of evaluation
   * is to get all declarations and then examine the declarations for exprs
   * via DFS. *)
  val foldExprs : (Ast.exp * 'a -> 'a) -> 'a -> Ast.dec -> 'a

  (* list of expressions we might be interested in when analyzing code. 
   * Currently includes FnExp, CaseExp, LetExp, SelectorExp,
   * ConstraintExp, HandleExp, RaiseExp, WhileExp, VectorExp. *)
  val keyExp : exptype list

  (* list of expressions with the exception of MarkExp *)
  val allExp : exptype list

  (* findExp (curried) takes a list of expression types (exptype) and a
   * a parse tree and returns a list of all expression nodes that match the 
   * input list of types. *)
  val findExp : exptype list -> Ast.dec -> Ast.exp list

  (* countExp (curried) takes a list of expression types (exptype) and a
   * parse tree and returns a list of expression type / occurrence pairs. *)
  val countExp : exptype list -> Ast.dec -> (exptype * int) list

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
  fun foldDecs f acc dec =
    (case dec
       of Ast.MarkDec (m, r) =>
            let val acc' = f (dec, acc)
            in foldDecs f acc' m
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
            List.foldl (fn (dec, acc') => foldDecs f acc' dec) acc decs
        | Ast.FixDec _ => (print (if debug then "fixity dec\n" else "");
                           f (dec, acc))
        | Ast.LocalDec (locals, body) =>
            (print (if debug then "local dec\n" else "");
             let val acc' = f (dec, acc)
                 val newAcc = foldDecs f acc' locals
             in foldDecs f newAcc body
             end)
        | Ast.FsigDec _ => (print (if debug then "fsig dec\n" else "");
                            f (dec, acc))
        | Ast.SigDec _ => (print (if debug then "sig dec\n" else "");
                           f (dec, acc))
        | Ast.FctDec fctdecs =>
           (print (if debug then "funsig dec\n" else "");
                (* walkStrExp extracts declarations from a structure expression
                 * by applying foldDecs on any Ast.dec types it finds *)
            let fun walkStrExp f acc (Ast.BaseStr d) = foldDecs f acc d
                  | walkStrExp f acc (Ast.MarkStr (strexp, _)) =
                      walkStrExp f acc strexp
                  | walkStrExp f acc (Ast.LetStr (letdec, strexp)) =
                      walkStrExp f (foldDecs f acc letdec) strexp
                  | walkStrExp f acc _ = acc
                (* walkFctExp pulls Ast.dec types out of a functor expression
                 * by processing the sub structure expression *)
                fun walkFctExp f acc (Ast.MarkFct (fctexp, _)) =
                      walkFctExp f acc fctexp
                  | walkFctExp f acc (Ast.BaseFct {body = strexp,...}) = 
                      walkStrExp f acc strexp
                  | walkFctExp f acc (Ast.LetFct (letdec, fctexp)) =
                      walkFctExp f (foldDecs f acc letdec) fctexp
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
                 * by applying foldDecs on any Ast.dec types it finds *)
            let fun walkStrExp f acc (Ast.BaseStr d) = foldDecs f acc d
                  | walkStrExp f acc (Ast.MarkStr (strexp, _)) =
                      walkStrExp f acc strexp
                  | walkStrExp f acc (Ast.LetStr (letdec, strexp)) =
                      walkStrExp f (foldDecs f acc letdec) strexp
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
             foldDecs f (f (dec, acc)) d) 
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
                        foldDecs f acc d
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
                        foldDecs f acc d
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
                        foldDecs f acc d
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

  datatype exptype = VAREXP | FNEXP | FLATAPPEXP | APPEXP | CASEEXP | LETEXP
                   | SEQEXP | INTEXP | WORDEXP | REALEXP | STRINGEXP | CHAREXP
                   | RECORDEXP | LISTEXP | TUPLEEXP | SELECTOREXP 
                   | CONSTRAINTEXP | HANDLEEXP | RAISEEXP | IFEXP | ANDALSOEXP
                   | ORELSEEXP | WHILEEXP | MARKEXP | VECTOREXP

  (* expToExpType is essentially a one-to-one mapping between Ast.exp and
   * exptype *)
  fun expToExpType (Ast.VarExp _) = VAREXP
    | expToExpType (Ast.FnExp _) = FNEXP
    | expToExpType (Ast.FlatAppExp _) = FLATAPPEXP
    | expToExpType (Ast.AppExp _) = APPEXP
    | expToExpType (Ast.CaseExp _) = CASEEXP
    | expToExpType (Ast.LetExp _) = LETEXP
    | expToExpType (Ast.SeqExp _) = SEQEXP
    | expToExpType (Ast.IntExp _) = INTEXP
    | expToExpType (Ast.WordExp _) = WORDEXP
    | expToExpType (Ast.RealExp _) = REALEXP
    | expToExpType (Ast.StringExp _) = STRINGEXP
    | expToExpType (Ast.CharExp _) = CHAREXP
    | expToExpType (Ast.RecordExp _) = RECORDEXP
    | expToExpType (Ast.ListExp _) = LISTEXP
    | expToExpType (Ast.TupleExp _) = TUPLEEXP
    | expToExpType (Ast.SelectorExp _) = SELECTOREXP
    | expToExpType (Ast.ConstraintExp _) = CONSTRAINTEXP
    | expToExpType (Ast.HandleExp _) = HANDLEEXP
    | expToExpType (Ast.RaiseExp _) = RAISEEXP
    | expToExpType (Ast.IfExp _) = IFEXP
    | expToExpType (Ast.AndalsoExp _) = ANDALSOEXP
    | expToExpType (Ast.OrelseExp _) = ORELSEEXP
    | expToExpType (Ast.WhileExp _) = WHILEEXP
    | expToExpType (Ast.MarkExp _) = MARKEXP
    | expToExpType (Ast.VectorExp _) = VECTOREXP

  (* decToDecType is essentially a one-to-one mapping between Ast.dec and
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
    foldDecs (fn (dec, acc) =>
               (case (List.find (fn x => x = decToDecType dec) countedDecs)
                  of NONE => acc
                   | SOME _ => increment (decToDecType dec) acc)) [] parseTree

  (* see signature *)
  fun findDec targetDecs parseTree =
    foldDecs (fn (dec, acc) =>
               (case (List.find (fn x => x = decToDecType dec) targetDecs)
                  of NONE => acc
                   | SOME _ => dec :: acc)) [] parseTree

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
            fun findName {fixity = SOME (symbol),
                           item = _, region = _} = Symbol.name symbol
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

  (* see signature *)
  val keyExp = [FNEXP, CASEEXP, LETEXP, SELECTOREXP, CONSTRAINTEXP,
                HANDLEEXP, RAISEEXP, WHILEEXP, VECTOREXP]

  (* see signature *)
  val allExp = [VAREXP, FNEXP, FLATAPPEXP, APPEXP, CASEEXP, LETEXP, INTEXP,
                WORDEXP, REALEXP, STRINGEXP, CHAREXP, SEQEXP, RECORDEXP,
                LISTEXP, TUPLEEXP, SELECTOREXP, CONSTRAINTEXP, HANDLEEXP,
                RAISEEXP, IFEXP, ANDALSOEXP, ORELSEEXP, WHILEEXP, VECTOREXP]

  (* see signuature *)
  fun foldExprs f acc tree =
    let val decs = findDec allDec tree
        (* drill down into declarations to find top-level expressions in that
         * declaration *)
        fun findExpInDec (Ast.ValDec (vblist, _)) =
              let fun findExpInVb (Ast.Vb {exp = e,...}) = e
                    | findExpInVb (Ast.MarkVb (vb, _)) = findExpInVb vb
              in List.foldl (fn (vb, acc) => findExpInVb vb :: acc) [] vblist
              end
          | findExpInDec (Ast.ValrecDec (rvblist, _)) =
              let fun findExpInRvb (Ast.Rvb {exp = e,...}) = e
                    | findExpInRvb (Ast.MarkRvb (rvb, _)) =
                        findExpInRvb rvb
              in List.foldl (fn (rvb, acc) => findExpInRvb rvb :: acc)
                   [] rvblist
              end
          | findExpInDec (Ast.FunDec (fblist, _)) =
              let fun findExpInFb (Ast.Fb (clauses, _)) =
                    List.foldl (fn (Ast.Clause {exp = e, ...}, acc) => e :: acc)
                      [] clauses
                    | findExpInFb (Ast.MarkFb (fb, _)) = findExpInFb fb
              in List.foldl (fn (fb, acc) => findExpInFb fb @ acc) [] fblist
              end
          | findExpInDec (Ast.OvldDec (_, _, exps)) = exps
          | findExpInDec _ = []
        val topExps = List.foldl (op @) [] (map findExpInDec decs)
        (* DFS on expr to find subexpressions *)
        fun walkExprs f acc e =
          let val acc' = f (e, acc)
          in (case e
                of (Ast.FnExp rules) =>
                     List.foldl (fn (Ast.Rule {exp = exp,...}, acc) =>
                                  walkExprs f acc exp) acc' rules
                 | (Ast.FlatAppExp expFixitems) =>
                     List.foldl (fn ({item = e, fixity = _, region = _}, acc) =>
                                  walkExprs f acc e)
                       acc' expFixitems
                 | (Ast.AppExp {function = fexp, argument = arg}) =>
                     let val acc'' = walkExprs f acc' fexp
                     in walkExprs f acc'' arg
                     end
                 | (Ast.CaseExp {expr = exp, rules = rules}) =>
                     let val acc'' = walkExprs f acc' exp
                     in List.foldl (fn (Ast.Rule {exp = exp,...}, acc) =>
                                     walkExprs f acc exp) acc' rules
                     end
                 | (Ast.LetExp {expr = exp,...}) => walkExprs f acc' exp
                 | (Ast.SeqExp exps) =>
                     List.foldl (fn (e, acc) => walkExprs f acc e) acc' exps
                 | (Ast.RecordExp symbolExpPairs) =>
                     List.foldl (fn ((_, e), acc) => walkExprs f acc e) acc'
                       symbolExpPairs
                 | (Ast.ListExp exps) =>
                     List.foldl (fn (e, acc) => walkExprs f acc e) acc' exps
                 | (Ast.TupleExp exps) =>
                     List.foldl (fn (e, acc) => walkExprs f acc e) acc' exps
                 | (Ast.ConstraintExp {expr = exp,...}) =>
                     walkExprs f acc' exp
                 | (Ast.HandleExp {expr = exp, rules = rules}) =>
                     let val acc'' = walkExprs f acc' exp
                     in List.foldl (fn (Ast.Rule {exp = exp,...}, acc) =>
                                  walkExprs f acc exp) acc' rules
                     end
                 | (Ast.RaiseExp exp) => walkExprs f acc' exp
                 | (Ast.IfExp {test = t, thenCase = tt, elseCase = tf}) =>
                     let val acc1 = walkExprs f acc' t
                         val acc2 = walkExprs f acc1 tt
                     in walkExprs f acc2 tf
                     end
                 | (Ast.AndalsoExp (e1, e2)) =>
                     let val acc1 = walkExprs f acc' e1
                     in walkExprs f acc1 e2
                     end
                 | (Ast.OrelseExp (e1, e2)) =>
                     let val acc1 = walkExprs f acc' e1
                     in walkExprs f acc1 e2
                     end
                 | (Ast.WhileExp {test = t, expr = body}) =>
                     let val acc'' = walkExprs f acc' t
                     in walkExprs f acc'' body
                     end
                 | (Ast.MarkExp (exp,_)) => walkExprs f acc' exp
                 | (Ast.VectorExp exps) =>
                     List.foldl (fn (e, acc) => walkExprs f acc e) acc' exps
                 | _ => acc')
          end
    in List.foldl (fn (e, a) => walkExprs f a e) acc topExps
    end

  (* see signature *)
  fun findExp targetExps parseTree =
    foldExprs (fn (exp, acc) =>
                (case (List.find (fn x => x = expToExpType exp) targetExps)
                   of NONE => acc
                    | SOME _ => exp :: acc)) [] parseTree

  (* see signature *)
  fun countExp countedExps parseTree =
    foldExprs (fn (exp, acc) =>
                (case (List.find (fn x => x = expToExpType exp) countedExps)
                   of NONE => acc
                    | SOME _ => increment (expToExpType exp) acc)) [] parseTree
end
