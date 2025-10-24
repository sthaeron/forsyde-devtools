IRSystem(
  {"input"}, {"output"},
  {
    IRActor("actor_1", Actor22, "add"),
    IRDelay("delay_1", {0})
  },
  {
    IRSignal("s_in", ("input", 1), ("actor_1", 1)),
    IRSignal("s_1", ("actor_1", 1), ("delay_1", 1)),
    IRSignal("s_2", ("delay_1", 1), ("actor_1", 1)),
    IRSignal("s_out", ("actor_1", 1), ("output", 1))
  },
  {
    IRFunction("add", )
  }
)
