A_tri_x2y2z2 <- matrix(nrow = 28, ncol = 8)
A_tri_x2y2z2[1, ] <- c(-1, 0, 0, 0, 1, 1, 1, 0)
A_tri_x2y2z2[2, ] <- c(0, 1, 1, 0, 0, 0, 0, 0)
A_tri_x2y2z2[3, ] <- c(1, 2, 2, 0, -1, -1, 0, 0)
A_tri_x2y2z2[4, ] <- c(-1, 1, 1, 0, 1, 1, 0, 0)
A_tri_x2y2z2[5, ] <- c(1, 1, 1, 0, -1, 0, 0, 0)
A_tri_x2y2z2[6, ] <- c(0, 0, 0, 0, 0, 1, 1, 0)
A_tri_x2y2z2[7, ] <- c(-1, -1, 0, 0, 1, 2, 2, 0)
A_tri_x2y2z2[8, ] <- c(1, 1, 0, 0, -1, 1, 1, 0)
A_tri_x2y2z2[9, ] <- c(1, 0, 0, 0, 0, 0, 0, 0)
A_tri_x2y2z2[10, ] <- c(0, -1, 0, 0, 1, 1, 1, 0)
A_tri_x2y2z2[11, ] <- c(1, -1, 0, 1, 1, 1, 0, 0)
A_tri_x2y2z2[12, ] <- c(2, 2, 1, 1, 0, -2, -1, 0)
A_tri_x2y2z2[13, ] <- c(1, 1, 1, 0, 0, -1, 0, 0)
A_tri_x2y2z2[14, ] <- c(0, 0, 0, 0, 1, 0, 0, 0)
A_tri_x2y2z2[15, ] <- c(2, 1, 0, 2, -1, -1, 0, 0)
A_tri_x2y2z2[16, ] <- c(1, 1, 2, 2, 0, -1, -2, 0)
A_tri_x2y2z2[17, ] <- c(1, 1, 0, 1, 0, -1, 0, 0)
A_tri_x2y2z2[18, ] <- c(1, 1, 1, 1, 0, -1, -1, 0)
A_tri_x2y2z2[19, ] <- c(1, 0, 0, 1, 0, 0, 0, 0)
A_tri_x2y2z2[20, ] <- c(1, 0, 1, 1, 0, 0, -1, 0)
A_tri_x2y2z2[21, ] <- c(1, 1, 0, 1, -1, 0, 0, 0)
A_tri_x2y2z2[22, ] <- c(0, 1, 1, 1, 0, 0, -1, 0)
A_tri_x2y2z2[23, ] <- c(0, 0, 0, 0, 0, 1, 0, 0)
A_tri_x2y2z2[24, ] <- c(1, 1, 1, 1, -1, -1, -1, 0)
A_tri_x2y2z2[25, ] <- c(0, 1, 0, 0, 0, 0, 0, 0)
A_tri_x2y2z2[26, ] <- c(0, 0, 0, 1, 0, 0, 0, 0)
A_tri_x2y2z2[27, ] <- c(0, 0, 0, 0, 0, 0, 1, 0)
A_tri_x2y2z2[28, ] <- c(0, 0, 1, 0, 0, 0, 0, 0)

ice_tri_x2y2z2 <- c(1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    0,
                    0,
                    -1,
                    -1,
                    0,
                    0,
                    -1,
                    -1,
                    -1,
                    -1,
                    -1,
                    -1,
                    0,
                    0,
                    0,
                    0,
                    0,
                    0,
                    0,
                    0)

bpbounds_tri_x2y2z2 <-
  function(p, A = A_tri_x2y2z2, ice = ice_tri_x2y2z2) {
    prod <- A %*% p
    ivinequality <- prod[ice == 0]
    low <- -1 * prod[ice == 1]
    upp <- prod[ice == -1]
    inequality <- min(ivinequality) >= 0
    bplow <- max(low)
    bpupp <- min(upp)

    return(
      list(
        "inequality" = inequality,
        "bplow" = bplow,
        "bpupp" = bpupp,
        "bplower" = low,
        "bpupper" = upp
      )
    )
  }
