GFuncDef(TInt, "main", {},
  SScope {
    SVarDecl(TIdent("token"), "input"),
    SVarDecl(TIdent("token"), "output"),
    SVarDecl(TInt, "i"),
    SVarDecl(TInt, "j"),
    SVarDef(TPoint(TIdent("channel")), "s_in", ECall("create_buffer_nonblocking", (EInt(4)))),
    SVarDef(TPoint(TIdent("channel")), "s_out", ECall("create_buffer_nonblocking", (EInt(2)))),
    SVarDef(TPoint(TIdent("channel")), "s_1", ECall("create_buffer_nonblocking", (EInt(1)))),
    SVarDef(TPoint(TIdent("channel")), "s_1_delay", ECall("create_buffer_nonblocking", (EInt(1)))),
    SExpr(ECall("writeToken", (EVar("s_1"), EInt(0)))),
    SWhile(EInt(1), SScope {
      SExpr(ECall("actor11SDF", (EInt(2), EInt(1), EVar("s_in"), EVar("s_1"), EVar("f_1"))))

    }),
    SArrayAssign("x", EInt(0), , EInt(3)),
    SArrayAssign("x", EInt(1), "foo", EInt(2)),
    SIf(EBinOp(<, EVar("i"), EVar("n")), SReturn, ),
    SIf(EBinOp(>, EVar("i"), EInt(0)), SReturn(EInt(1)), SReturn(EInt(0))),
    SReturn(EInt(0)),
    SReturn,
    SFor(SVarDef(TInt, "i", EInt(0)), EBinOp(<, EVar("i"), EVar("n")), SExpr(EUnOp(++, EVar("i"))), SScope {
      SExpr(ECall("printf", (EString "%d\n", EVar("i"))))

    }),
    SVarDef(TReference(TIdent("fifo")), "fifo", EVar("f")),
    SExpr(ECallExpr(EVar("printf"), (EString "Hi"))),
    SExpr(ECallExpr(EPointerAccess(EVar("a"), "funca"), ())),
    SExpr(ECallExpr(EMemberAccess(ECallExpr(EMemberAccess(EVar("a"), "getB"), ()), "printB"), ())),
    SExpr(EPointerAccess(EArrayAccess(EVar("p"), EVar("i")), "value")),
    SExpr(ECall("pthread_mutex_lock", (EReference(EPointerAccess(EVar("fifo"), "lock"))))),
    SVarDef(TInt, "value", EDereference(EVar("p"))),
    SAssign(EDereference(EVar("p")), EInt(10)),
    SArrayDecl(TInt, "input", (EInt(2)))

  }

)

GFuncDef(static, TVoid, "actor11SDF", {(TInt, "consum"), (TInt, "prod"), (TPoint(TIdent("channel")), "ch_in"), (TPoint(TIdent("channel")), "ch_out"), (TFuncPoint(TVoid, (TPoint(TIdent("token")), TPoint(TIdent("token")))), "f")},
  SScope {}

)
