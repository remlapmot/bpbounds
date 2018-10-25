bpbounds_calc_biv_z3 <- function(g, t){

  # conditional probabilities
  # notation is gyz = P(Y=y|Z=z), txz = P(X=x|Z=z)
  g00 = g[1]
  g10 = g[2]
  g01 = g[3]
  g11 = g[4]
  g02 = g[5]
  g12 = g[6]
  t00 = t[1]
  t10 = t[2]
  t01 = t[3]
  t11 = t[4]
  t02 = t[5]
  t12 = t[6]

  # bounds on probabilities
  p10low1 = g10 - t10
  p10low2 = g11 - t11
  p10low3 = g12 - t12
  p10upp1 = g10 + t10
  p10upp2 = g11 + t11
  p10upp3 = g12 + t12
  p10low = max(c(p10low1, p10low2, p10low3))
  p10upp = min(c(p10upp1, p10upp2, p10upp3))
  p11low1 = g10 + t10 - 1
  p11low2 = g11 + t11 - 1
  p11low3 = g12 + t12 - 1
  p11upp1 = g10 - t10 + 1
  p11upp2 = g11 - t11 + 1
  p11upp3 = g12 - t12 + 1
  p11low = max(c(p11low1, p11low2, p11low3))
  p11upp = min(c(p11upp1, p11upp2, p11upp3))

  p10lower = c(p10low1, p10low2, p10low3)
  p10upper = c(p10upp1, p10upp2, p10upp3)
  p11lower = c(p11low1, p11low2, p11low3)
  p11upper = c(p11upp1, p11upp2, p11upp3)
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
  retlist  = append(retlist, list("crrlb" = rrlow,
                                  "crrub" = rrupp))

  # bounds assuming monotonicity
  monoin1 = -g10 + g11 - g12
  monoin2 = -g10 + g11 + t10 - t11
  monoin3 = -g11 + g12 + t11 - t12
  monoin4 = g11 - g12 + t11 - t12
  monoin5 = g10 - g11 + t10 - t11
  monoin6 = g10 - g11 + g12
  monoinequality = (monoin1 <= 0) & (monoin2 <= 0) & (monoin3 <= 0) & (monoin4 <= 0) & (monoin5 <= 0) & (monoin6 <= 1)
  retlist = append(retlist, list("monoinequality" = monoinequality))
  monolow1 = -g10 - t10
  monolow2 = -g10 - g11+g12 - t10
  monolow3 = -2 *g10 + g12 - t10
  monolow4 = -2*g10 + g11- t10
  monolow5 = -g10 + g12 - t10+ t12 - 1
  monolow6 = g12 + t12 - 2
  monolow7 = -g10 + 2*g12 + t12-2
  monolow8 = -g11 + 2*g12+ t12 - 2
  monolow9 = -g10 + g11 + g12+ t12 - 2
  monoupp1 = 1 + g12 - t12
  monoupp2 = 1 - g10 + t10
  monoupp3 = 1 - 2 *g10 + g11 + t10
  monoupp4 = 1 - 2*g10 + g12+ t10
  monoupp5 = 1 - g10 + g11 + g12- t12
  monoupp6 = 1 - g10 + g12 + t10- t12
  monoupp7 = 1 - g10 - g11 + g12+ t10
  monoupp8 = 1 - g11 + 2*g12 - t12
  monoupp9 = 1 - g10 + 2*g12- t12
  monolow = max(c(monolow1, monolow2, monolow3, monolow4, monolow5, monolow6, monolow7, monolow8, monolow9))
  monoupp = min(c(monoupp1, monoupp2, monoupp3, monoupp4, monoupp5, monoupp6, monoupp7, monoupp8, monoupp9))
  if (monoinequality) {
    retlist = append(retlist, list("monobplb" = monolow,
                                   "monobpub" = monoupp))
    monolower = c(monolow1, monolow2, monolow3, monolow4, monolow5, monolow6, monolow7, monolow8, monolow9)
    monoupper = c(monoupp1, monoupp2, monoupp3, monoupp4, monoupp5, monoupp6, monoupp7, monoupp8, monoupp9)
    retlist = append(retlist, list("monolower" = monolower,
                                   "monoupper" = monoupper))

    # bounds on intervention probabilities assuming monotonicity
    monop10low1 = g11 - g12
    monop10low2 = g10 - g11
    monop10low3 = g10 - g12
    monop10low4 = g10 - t10
    monop10upp1 = g10 + t10
    monop10upp2 = 1 + g10 - g12
    monop10upp3 = 1 + g10 - g11
    monop10upp4 = 1 + g11 - g12
    monop11low1 = -g10 + g12
    monop11low2 = -g10 + g11
    monop11low3 = -g11 + g12
    monop11low4 = g12 + t12-1
    monop11upp1 = 1 + g12 - t12
    monop11upp2 = 1 - g11 + g12
    monop11upp3 = 1 - g10 + g11
    monop11upp4 = 1 - g10 + g12

    monop10lb = max(c(monop10low1, monop10low2, monop10low3, monop10low4))
    monop10ub = min(c(monop10upp1, monop10upp2, monop10upp3, monop10upp4))
    monop11lb = max(c(monop11low1, monop11low2, monop11low3, monop11low4))
    monop11ub = min(c(monop11upp1, monop11upp2, monop11upp3, monop11upp4))
	  retlist = append(retlist, list("monop10lb" = monop10lb,
	                                 "monop10ub" = monop10ub,
	                                 "monop11lb" = monop11lb,
	                                 "monop11ub" = monop11ub))

    monop10lower = c(monop10low1, monop10low2, monop10low3, monop10low4)
    monop10upper = c(monop10upp1, monop10upp2, monop10upp3, monop10upp4)
    monop11lower = c(monop11low1, monop11low2, monop11low3, monop11low4)
    monop11upper = c(monop11upp1, monop11upp2, monop11upp3, monop11upp4)
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
