bpbounds_calc_tri_z3 <- function(p) {
  # assuming that p is in the following order
  # notation for conditional probabilities is p(y,x|z)
  p000 = p[1]
  p100 = p[2]
  p010 = p[3]
  p110 = p[4]
  p001 = p[5]
  p101 = p[6]
  p011 = p[7]
  p111 = p[8]
  p002 = p[9]
  p102 = p[10]
  p012 = p[11]
  p112 = p[12]

  # bounds on probabilities
  p10low1 = p100
  p10low2 = p101
  p10low3 = p102
  p10low4 = p100 + p110 + p101 + p011 - 1
  p10low5 = p100 + p010 + p101 + p111 - 1
  p10low6 = p101 + p111 + p102 + p012 - 1
  p10low7 = p101 + p011 + p102 + p112 - 1
  p10low8 = p102 + p112 + p100 + p010 - 1
  p10low9 = p102 + p012 + p100 + p110 - 1
  p10upp1 = 1 - p000
  p10upp2 = 1 - p001
  p10upp3 = 1 - p002
  p10upp4 = p100 + p010 + p101 + p111
  p10upp5 = p100 + p110 + p101 + p011
  p10upp6 = p101 + p011 + p102 + p112
  p10upp7 = p101 + p111 + p102 + p012
  p10upp8 = p102 + p012 + p100 + p110
  p10upp9 = p102 + p112 + p100 + p010
  p10low = max(c(
    p10low1,
    p10low2,
    p10low3,
    p10low4,
    p10low5,
    p10low6,
    p10low7,
    p10low8,
    p10low9
  ))
  p10upp = min(c(
    p10upp1,
    p10upp2,
    p10upp3,
    p10upp4,
    p10upp5,
    p10upp6,
    p10upp7,
    p10upp8,
    p10upp9
  ))
  p11low1 = p110
  p11low2 = p111
  p11low3 = p112
  p11low4 = p100 + p110 - p101 - p011
  p11low5 = -p100 - p010 + p101 + p111
  p11low6 = p101 + p111 - p102 - p012
  p11low7 = -p101 - p011 + p102 + p112
  p11low8 = p102 + p112 - p100 - p010
  p11low9 = -p102 - p012 + p100 + p110
  p11upp1 = 1 - p010
  p11upp2 = 1 - p011
  p11upp3 = 1 - p012
  p11upp4 = p100 + p110 - p101 - p011 + 1
  p11upp5 = -p100 - p010 + p101 + p111 + 1
  p11upp6 = p101 + p111 - p102 - p012 + 1
  p11upp7 = -p101 - p011 + p102 + p112 + 1
  p11upp8 = p102 + p112 - p100 - p010 + 1
  p11upp9 = -p102 - p012 + p100 + p110 + 1

  p11low = max(c(
    p11low1,
    p11low2,
    p11low3,
    p11low4,
    p11low5,
    p11low6,
    p11low7,
    p11low8,
    p11low9
  ))
  p11upp = min(c(
    p11upp1,
    p11upp2,
    p11upp3,
    p11upp4,
    p11upp5,
    p11upp6,
    p11upp7,
    p11upp8,
    p11upp9
  ))

  p10lower = c(
    p10low1,
    p10low2,
    p10low3,
    p10low4,
    p10low5,
    p10low6,
    p10low7,
    p10low8,
    p10low9
  )
  p10upper = c(
    p10upp1,
    p10upp2,
    p10upp3,
    p10upp4,
    p10upp5,
    p10upp6,
    p10upp7,
    p10upp8,
    p10upp9
  )
  p11lower = c(
    p11low1,
    p11low2,
    p11low3,
    p11low4,
    p11low5,
    p11low6,
    p11low7,
    p11low8,
    p11low9
  )
  p11upper = c(
    p11upp1,
    p11upp2,
    p11upp3,
    p11upp4,
    p11upp5,
    p11upp6,
    p11upp7,
    p11upp8,
    p11upp9
  )
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
  retlist = append(retlist, list("crrlb" = rrlow, "crrub" = rrupp))

  # monotonicity bounds
  m1 = (p102 <= p101) & (p101 <= p100)
  m2 = (p110 <= p111) & (p111 <= p112)
  m3 = (p010 <= p011) & (p011 <= p012)
  m4 = (p002 <= p001) & (p001 <= p000)
  monoinequality = (m1 == TRUE & m2 == TRUE & m3 == TRUE & m4 == TRUE)
  retlist = append(retlist, list("monoinequality" = monoinequality))
  if (monoinequality == TRUE) {
    mlow = p112 + p000 - 1
    mupp = 1 - p100 - p110
    retlist = append(retlist, list("monobplb" = mlow, "monobpub" = mupp))

    # bounds on intervention probabilities assuming monotonicity
    monop10low = p100
    monop10upp = 1 - p000
    monop11low = p112
    monop11upp = 1 - p012
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
    retlist = append(
      retlist,
      list("monocrrlb" = monocrrlow, "monocrrub" = monocrrupp)
    )
  }
  return(retlist)
}
