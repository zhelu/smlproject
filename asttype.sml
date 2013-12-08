(* This structure allows us to refer to values for the ADTs Ast.dec and
 * Ast.exp. It's very difficult to create new values using the value
 * constructors. Instead here, we provide a simple way to refer to
 * the times when a specific value constructor is used. *)
signature ASTTYPE = sig

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

  (* get string equivalent of a dec type *)
  val decTypeToString : dectype -> string

  (* get string equivalent of a exp type *)
  val expTypeToString : exptype -> string

  (* all declaration types except for MarkDec and SeqDec *)
  val allDec : dectype list

  (* list of expressions we might be interested in when analyzing code.
   * Currently includes FnExp, CaseExp, LetExp, SelectorExp,
   * ConstraintExp, HandleExp, RaiseExp, WhileExp, VectorExp. *)
  val keyExp : exptype list

  (* list of expressions with the exception of MarkExp *)
  val allExp : exptype list

  (* expToExpType is essentially a one-to-one mapping between Ast.exp and
   * exptype *)
  val expToExpType : Ast.exp -> exptype

  (* decToDecType is essentially a one-to-one mapping between Ast.dec and
   * dectype *)
  val decToDecType : Ast.dec -> dectype

end

structure AstType :> ASTTYPE = struct

  datatype dectype = MARKDEC | OPENDEC | TYPEDEC | OVLDDEC | SEQDEC | FIXDEC
                   | LOCALDEC | FSIGDEC | SIGDEC | FCTDEC | ABSDEC | STRDEC
                   | EXCEPTIONDEC | ABSTYPEDEC | DATAREPLDEC | DATATYPEDEC
                   | FUNDEC | VALDEC | VALRECDEC

  datatype exptype = VAREXP | FNEXP | FLATAPPEXP | APPEXP | CASEEXP | LETEXP
                   | SEQEXP | INTEXP | WORDEXP | REALEXP | STRINGEXP | CHAREXP
                   | RECORDEXP | LISTEXP | TUPLEEXP | SELECTOREXP
                   | CONSTRAINTEXP | HANDLEEXP | RAISEEXP | IFEXP | ANDALSOEXP
                   | ORELSEEXP | WHILEEXP | MARKEXP | VECTOREXP

  (* see signature *)
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

  (* see signature *)
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

  (* see signature *)
  fun decTypeToString MARKDEC = "MarkDec"
    | decTypeToString OPENDEC = "OpenDec"
    | decTypeToString TYPEDEC = "TypeDec"
    | decTypeToString OVLDDEC = "OvldDec"
    | decTypeToString SEQDEC = "SeqDec"
    | decTypeToString FIXDEC = "FixDec"
    | decTypeToString LOCALDEC = "LocalDec"
    | decTypeToString FSIGDEC = "FsigDec"
    | decTypeToString SIGDEC = "SigDec"
    | decTypeToString FCTDEC = "FctDec"
    | decTypeToString ABSDEC = "AbsDec"
    | decTypeToString STRDEC = "StrDec"
    | decTypeToString EXCEPTIONDEC = "ExceptionDec"
    | decTypeToString ABSTYPEDEC = "AbstypeDec"
    | decTypeToString DATAREPLDEC = "DatareplDec"
    | decTypeToString DATATYPEDEC = "DatatypeDec"
    | decTypeToString FUNDEC = "FunDec"
    | decTypeToString VALDEC = "ValDec"
    | decTypeToString VALRECDEC = "ValrecDec"

  (* see signature *)
  fun expTypeToString VAREXP = "VarExp"
    | expTypeToString FNEXP = "FnExp"
    | expTypeToString FLATAPPEXP = "FlatAppExp"
    | expTypeToString APPEXP = "AppExp"
    | expTypeToString CASEEXP = "CaseExp"
    | expTypeToString LETEXP = "LetExp"
    | expTypeToString SEQEXP = "SeqExp"
    | expTypeToString INTEXP = "IntExp"
    | expTypeToString WORDEXP = "WordExp"
    | expTypeToString REALEXP = "RealExp"
    | expTypeToString STRINGEXP = "StringExp"
    | expTypeToString CHAREXP = "CharExp"
    | expTypeToString RECORDEXP = "RecordExp"
    | expTypeToString LISTEXP = "ListExp"
    | expTypeToString TUPLEEXP = "TupleExp"
    | expTypeToString SELECTOREXP = "SelectorExp"
    | expTypeToString CONSTRAINTEXP = "ConstraintExp"
    | expTypeToString HANDLEEXP = "HandleExp"
    | expTypeToString RAISEEXP = "RaiseExp"
    | expTypeToString IFEXP = "IfExp"
    | expTypeToString ANDALSOEXP = "AndalsoExp"
    | expTypeToString ORELSEEXP = "OrelseExp"
    | expTypeToString WHILEEXP = "WhileExp"
    | expTypeToString MARKEXP = "MarkExp"
    | expTypeToString VECTOREXP = "VectorExp"

  (* see signature *)
  val allDec = [OPENDEC, TYPEDEC, OVLDDEC, FIXDEC, LOCALDEC, FSIGDEC, SIGDEC,
                FCTDEC, ABSDEC, STRDEC, EXCEPTIONDEC, ABSTYPEDEC, DATAREPLDEC,
                DATATYPEDEC, FUNDEC, VALDEC, VALRECDEC]

  (* see signature *)
  val keyExp = [FNEXP, CASEEXP, LETEXP, SELECTOREXP, CONSTRAINTEXP,
                HANDLEEXP, RAISEEXP, SEQEXP, WHILEEXP, VECTOREXP]

  (* see signature *)
  val allExp = [VAREXP, FNEXP, FLATAPPEXP, APPEXP, CASEEXP, LETEXP, INTEXP,
                WORDEXP, REALEXP, STRINGEXP, CHAREXP, SEQEXP, RECORDEXP,
                LISTEXP, TUPLEEXP, SELECTOREXP, CONSTRAINTEXP, HANDLEEXP,
                RAISEEXP, IFEXP, ANDALSOEXP, ORELSEEXP, WHILEEXP, VECTOREXP]
end
