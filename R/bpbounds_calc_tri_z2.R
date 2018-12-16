bpbounds_calc_tri_z2 <- function(p) {
  # assuming that p is in the following order
  # notation for conditional probabilities is p(y,x|z)
  p000 = p[1]
  p100 = p[3]
  p010 = p[2]
  p110 = p[4]
  p001 = p[5]
  p101 = p[7]
  p011 = p[6]
  p111 = p[8]

  # pearl bounds on probabilities
  p10low1 = p101
  p10low2 = p100
  p10low3 = p100 + p110 - p001 - p111
  p10low4 = p010 + p100 - p001 - p011
  p10upp1 = 1 - p001
  p10upp2 = 1 - p000
  p10upp3 = p010 + p100 + p101 + p111
  p10upp4 = p100 + p110 + p011 + p101
  p10low = max(c(p10low1, p10low2, p10low3, p10low4))
  p10upp = min(c(p10upp1, p10upp2, p10upp3, p10upp4))
  p11low1 = p110
  p11low2 = p111
  p11low3 = -p000 - p010 + p001 + p111
  p11low4 = -p010 - p100 + p101 + p111
  p11upp1 = 1 - p011
  p11upp2 = 1 - p010
  p11upp3 = p000 + p110 + p101 + p111
  p11upp4 = p100 + p110 + p001 + p111
  p11low = max(c(p11low1, p11low2, p11low3, p11low4))
  p11upp = min(c(p11upp1, p11upp2, p11upp3, p11upp4))

  p10lower = c(p10low1, p10low2, p10low3, p10low4)
  p10upper = c(p10upp1, p10upp2, p10upp3, p10upp4)
  p11lower = c(p11low1, p11low2, p11low3, p11low4)
  p11upper = c(p11upp1, p11upp2, p11upp3, p11upp4)

  retlist = list(
    "p10low" = p10low,
    "p10upp" = p10upp,
    "p11low" = p11low,
    "p11upp" = p11upp,
    "p10lower" = p10lower,
    "p10upper" = p10upper,
    "p11lower" = p11lower,
    "p11upper" = p11upper
  )

  # bounds on causal risk ratio
  rrlow = p11low / p10upp
  rrupp = p11upp / p10low
  retlist = append(retlist, list("crrlb" = rrlow,
                                 "crrub" = rrupp))

  # monotonicity bounds
  m1 = p000 - p001 >= 0
  m2 = p011 - p010 >= 0
  m3 = p100 - p101 >= 0
  m4 = p111 - p110 >= 0
  mlow = p000 - p001 - p011 - p101
  mupp = p000 + p010 + p110 - p011
  monoinequality = (m1 == TRUE &
                      m2 == TRUE & m3 == TRUE & m4 == TRUE)
  if (monoinequality) {
    retlist = append(retlist, list("monobplb" = mlow,
                                   "monobpub" = mupp))

    # bounds on intervention probabilities assuming monotonicity
    monop10low = p100
    monop10upp = 1 - p000
    monop11low = p111
    monop11upp = 1 - p011
    retlist = append(
      retlist,
      list(
        "monop10lb" = monop10low,
        "monop10ub" = monop10upp,
        "monop11lb" = monop11low,
        "monop11ub" = monop11upp
      )
    )

    # bounds on causal risk ratio assuming monotonicity
    monocrrlow = monop11low / monop10upp
    monocrrupp = monop11upp / monop10low
    retlist = append(retlist,
                     list("monocrrlb" = monocrrlow,
                          "monocrrub" = monocrrupp))
  }
  retlist = append(retlist, list("monoinequality" = monoinequality))
  return(retlist)
}
