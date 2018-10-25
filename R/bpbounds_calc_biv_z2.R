bpbounds_calc_biv_z2 <- function(g, t) {

  # conditional probabilities
  # notation is gyz = P(Y=y|Z=z), txz = P(X=x|Z=z)
  g00 = g[1]
  g10 = g[2]
  g01 = g[3]
  g11 = g[4]
  t00 = t[1]
  t10 = t[2]
  t01 = t[3]
  t11 = t[4]

  # bounds on probabilities
  p10low1 = g11 - t11
  p10low2 = g10 - t10
  p10upp1 = g11 + t11
  p10upp2 = g10 + t10
  p10low = max(c(p10low1, p10low2))
  p10upp = min(c(p10upp1, p10upp2))

  p11low1 = g11 + t11 - 1
  p11low2 = g10 + t10 - 1
  p11upp1 = g11 - t11 + 1
  p11upp2 = g10 - t10 + 1
  p11low = max(c(p11low1, p11low2))
  p11upp = min(c(p11upp1, p11upp2))

  p10lower = c(p10low1, p10low2)
  p10upper = c(p10upp1, p10upp2)
  p11lower = c(p11low1, p11low2)
  p11upper = c(p11upp1, p11upp2)
  retlist = list("p10low" = p10low,
                 "p10upp" = p10upp,
                 "p11low" = p11low,
                 "p11upp" = p11upp,
                 "p10lower" = p10lower,
                 "p10upper" = p10upper,
	               "p11lower" = p11lower,
                 "p11upper" = p11upper)

  # bounds on causal risk ratio
  rrlow = p11low/p10upp
  rrupp = p11upp/p10low
  retlist = append(retlist, list("crrlb" = rrlow,
                                 "crrub" = rrupp))

  # bounds assuming monotonicity
  monoinequality = t00 - t01 >= abs(g00 - g01)
  retlist = append(retlist, list("monoinequality" = monoinequality))

  monolow1 = 2*g00 - g01 + t00 - 2
  monolow2 = g00 - 2*g01 - t01
  monolow3 = g00 + t00 - 2
  monolow4 = -1*g00 - t01
  monolow5 = g00 - g01 + t00 - t01 - 1
  monoupp1 = 2*g00 - g01 - t00 + 1
  monoupp2 = g00 - 2*g01 + t01 + 1
  monoupp3 = g00 - t00 + 1
  monoupp4 = -1*g00 + t01 + 1
  monoupp5 = g00 - g01 - t00 + t01 + 1
  monolow = max(c(monolow1, monolow2, monolow3, monolow4, monolow5))
  monoupp = min(c(monoupp1, monoupp2, monoupp3, monoupp4, monoupp5))
  if (monoinequality) {
    retlist = append(retlist, list("monobplb" = monolow,
                                   "monobpub" = monoupp))
	  monolower = c(monolow1, monolow2, monolow3, monolow4, monolow5)
    monoupper = c(monoupp1, monoupp2, monoupp3, monoupp4, monoupp5)
    retlist = append(retlist, list("monolower" = monolower,
                                   "monoupper" = monoupper))

    # bounds on intervention probs under monotonicity
    monop10low1 = g10 - g11
    monop10low2 = g10 - t10
    monop10upp1 = 1 + g10 - g11
    monop10upp2 = g10 + t10
    monop11low1 = -g10 + g11
    monop11low2 = g11 + t11 - 1
    monop11upp1 = 1 + g11 - t11
    monop11upp2 = 1 - g10 + g11

    monop10lb = max(c(monop10low1, monop10low2))
    monop10ub = min(c(monop10upp1, monop10upp2))
    monop11lb = max(c(monop11low1, monop11low2))
    monop11ub = min(c(monop11upp1, monop11upp2))
    retlist = append(retlist, list("monop10lb" = monop10lb,
	                                 "monop10ub" = monop10ub,
                                   "monop11lb" = monop11lb,
                                   "monop11ub" = monop11ub))

    monop10lower = c(monop10low1, monop10low2)
    monop10upper = c(monop10upp1, monop10upp2)
    monop11lower = c(monop11low1, monop11low2)
    monop11upper = c(monop11upp1, monop11upp2)
    retlist = append(retlist, list("monop10lower" = monop10lower,
                                   "monop10upper" = monop10upper,
                                   "monop11lower" = monop11lower,
                                   "monop11upper" = monop11upper))

    # bounds on causal risk ratio assuming monotonicity
    monocrrlb = monop11lb/monop10ub
    monocrrub = monop11ub/monop10lb
    retlist = append(retlist, list("monocrrlb" = monocrrlb,
                                   "monocrrub" = monocrrub))
  }
  return(retlist)
}
