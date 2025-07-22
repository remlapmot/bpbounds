# Tests for the bpbounds package
# 2018-10-23 Tom Palmer

if (!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr", repos = "https://cloud.r-project.org/")
}
context("Tests for bpbounds package")

# Balke and Pearl, JASA, 1997 examples ----

## Table 1 - note 9665 should be 9663 from paper ----
tab1dat <- data.frame(
  z = c(0, 0, 1, 1, 1, 1, 0, 0),
  x = c(0, 0, 0, 0, 1, 1, 1, 1),
  y = c(0, 1, 0, 1, 0, 1, 0, 1),
  freq = c(74, 11514, 34, 2385, 12, 9665, 0, 0)
)

tab1inddat <- tidyr::uncount(tab1dat, freq)
xt <- xtabs(~ x + y + z, data = tab1inddat)
p <- prop.table(xt, margin = 3)

## Error checks
test_that("Errors", {
  expect_error(bpbounds(p, fmt = "bivariate"))
  expect_error(bpbounds(p, fmt = "anything-you-like"))
})

## Analyses
test_that("Balke and Pearl Table 1 example: trivariate data with 2 category instrument",
          {
            # Using conditional probabilities
            bpres <- bpbounds(p)

            expect_equal(class(bpres), "bpbounds")
            expect_equal(bpres$fmt, "trivariate")
            expect_equal(bpres$nzcats, 2)

            expect_true(bpres$inequality)
            expect_equal(bpres$bplb, -0.1946, tol = 1e-4)
            expect_equal(bpres$bpub, 0.0054, tol = 1e-4)
            expect_equal(bpres$p10low, 0.9936, tol = 1e-4)
            expect_equal(bpres$p10upp, 0.9936, tol = 1e-4)
            expect_equal(bpres$p11low, 0.7990, tol = 1e-4)
            expect_equal(bpres$p11upp, 0.9990, tol = 1e-4)
            expect_equal(bpres$crrlb, 0.8042, tol = 1e-4)
            expect_equal(bpres$crrub, 1.0054, tol = 1e-4)

            expect_true(bpres$monoinequality)
            expect_equal(bpres$monobplb, -0.1946, tol = 1e-4)
            expect_equal(bpres$monobpub, 0.0054, tol = 1e-4)
            expect_equal(bpres$monop10low, 0.9936, tol = 1e-4)
            expect_equal(bpres$monop10upp, 0.9936, tol = 1e-4)
            expect_equal(bpres$monop11low, 0.7990, tol = 1e-4)
            expect_equal(bpres$monop11upp, 0.9990, tol = 1e-4)
            expect_equal(bpres$monocrrlb, 0.8042, tol = 1e-4)
            expect_equal(bpres$monocrrub, 1.0054, tol = 1e-4)

            print(bpres)
            print(bpres, digits = 4)
            bpres

            sbp = summary(bpres)
            expect_equal(class(sbp), "summary.bpbounds")

            print(sbp)
            sbp
            print(sbp, digits = 2)
            print(sbp, digits = 4)

            # Using cell counts
            bpres = bpbounds(xt)
            expect_equal(class(bpres), "bpbounds")
            expect_equal(bpres$fmt, "trivariate")
            expect_equal(bpres$nzcats, 2)

            expect_true(bpres$inequality)
            expect_equal(bpres$bplb, -0.1946, tol = 1e-4)
            expect_equal(bpres$bpub, 0.0054, tol = 1e-4)
            expect_equal(bpres$p10low, 0.9936, tol = 1e-4)
            expect_equal(bpres$p10upp, 0.9936, tol = 1e-4)
            expect_equal(bpres$p11low, 0.7990, tol = 1e-4)
            expect_equal(bpres$p11upp, 0.9990, tol = 1e-4)
            expect_equal(bpres$crrlb, 0.8042, tol = 1e-4)
            expect_equal(bpres$crrub, 1.0054, tol = 1e-4)

            expect_true(bpres$monoinequality)
            expect_equal(bpres$monobplb, -0.1946, tol = 1e-4)
            expect_equal(bpres$monobpub, 0.0054, tol = 1e-4)
            expect_equal(bpres$monop10low, 0.9936, tol = 1e-4)
            expect_equal(bpres$monop10upp, 0.9936, tol = 1e-4)
            expect_equal(bpres$monop11low, 0.7990, tol = 1e-4)
            expect_equal(bpres$monop11upp, 0.9990, tol = 1e-4)
            expect_equal(bpres$monocrrlb, 0.8042, tol = 1e-4)
            expect_equal(bpres$monocrrub, 1.0054, tol = 1e-4)
          })


## Test the bivariate formulation ----
g  = xtabs(~ y + z, data = tab1inddat)
gp = prop.table(g, margin = 2)

t  = xtabs(~ x + z, data = tab1inddat)
tp = prop.table(t, margin = 2)

test_that("Balke and Pearl Table 1 example treated as bivariate data", {
  bpres = bpbounds(p = gp, t = tp, fmt = "bivariate")
  expect_true(bpres$inequality)
  expect_equal(bpres$bplb, -0.1974, tol = 1e-4)
  expect_equal(bpres$bpub, 0.0064, tol = 1e-4)
  expect_equal(bpres$p10low, 0.9936, tol = 1e-4)
  expect_equal(bpres$p10upp, 0.9936, tol = 1e-4)
  expect_equal(bpres$p11low, 0.7962, tol = 1e-4)
  expect_equal(bpres$p11upp, 1.1962, tol = 1e-4)
  expect_equal(bpres$crrlb, 0.8013, tol = 1e-4)
  expect_equal(bpres$crrub, 1.2039, tol = 1e-4)

  expect_true(bpres$monoinequality)
  expect_equal(bpres$monobplb, -0.1974, tol = 1e-4)
  expect_equal(bpres$monobpub, 0.0064, tol = 1e-4)
  expect_equal(bpres$monop10low, 0.9936, tol = 1e-4)
  expect_equal(bpres$monop10upp, 0.9936, tol = 1e-4)
  expect_equal(bpres$monop11low, 0.7962, tol = 1e-4)
  expect_equal(bpres$monop11upp, 1.0026, tol = 1e-4)
  expect_equal(bpres$monocrrlb, 0.8013, tol = 1e-4)
  expect_equal(bpres$monocrrub, 1.0090, tol = 1e-4)

  sbp = summary(bpres)
  print(sbp, digits = 3)
  print(sbp)
})

test_that("Balke and Pearl, bivariate data using cell counts",
          {
            bpres <- bpbounds(p = g, t = t, fmt = "bivariate")
            expect_true(bpres$inequality)
            expect_equal(bpres$bplb, -0.1974, tol = 1e-4)
            expect_equal(bpres$bpub, 0.0064, tol = 1e-4)
            expect_equal(bpres$p10low, 0.9936, tol = 1e-4)
            expect_equal(bpres$p10upp, 0.9936, tol = 1e-4)
            expect_equal(bpres$p11low, 0.7962, tol = 1e-4)
            expect_equal(bpres$p11upp, 1.1962, tol = 1e-4)
            expect_equal(bpres$crrlb, 0.8013, tol = 1e-4)
            expect_equal(bpres$crrub, 1.2039, tol = 1e-4)

            expect_true(bpres$monoinequality)
            expect_equal(bpres$monobplb, -0.1974, tol = 1e-4)
            expect_equal(bpres$monobpub, 0.0064, tol = 1e-4)
            expect_equal(bpres$monop10low, 0.9936, tol = 1e-4)
            expect_equal(bpres$monop10upp, 0.9936, tol = 1e-4)
            expect_equal(bpres$monop11low, 0.7962, tol = 1e-4)
            expect_equal(bpres$monop11upp, 1.0026, tol = 1e-4)
            expect_equal(bpres$monocrrlb, 0.8013, tol = 1e-4)
            expect_equal(bpres$monocrrub, 1.0090, tol = 1e-4)

            sbp <- summary(bpres)
            print(sbp, digits = 3)
            print(sbp)
          })

test_that("Bivariate data with cell counts for one and cond probs other", {
  bpres <- bpbounds(p = gp, t = t, fmt = "bivariate")
  sbp <- summary(bpres)

  expect_equal(class(bpres), "bpbounds")
  expect_equal(bpres$fmt, "bivariate")
  expect_equal(bpres$nzcats, 2)

  expect_true(bpres$inequality)
  expect_equal(bpres$bplb, -0.1974, tol = 1e-4)
  expect_equal(bpres$bpub, 0.0064, tol = 1e-4)
  expect_equal(bpres$p10low, 0.9936, tol = 1e-4)
  expect_equal(bpres$p10upp, 0.9936, tol = 1e-4)
  expect_equal(bpres$p11low, 0.7962, tol = 1e-4)
  expect_equal(bpres$p11upp, 1.1962, tol = 1e-4) # TODO: bug??
  expect_equal(bpres$crrlb, 0.8013, tol = 1e-4)
  expect_equal(bpres$crrub, 1.2039, tol = 1e-4)

  expect_true(bpres$inequality)
  expect_equal(bpres$monobplb, -0.1974, tol = 1e-4)
  expect_equal(bpres$monobpub, 0.0064, tol = 1e-4)
  expect_equal(bpres$monop10low, 0.9936, tol = 1e-4)
  expect_equal(bpres$monop10upp, 0.9936, tol = 1e-4)
  expect_equal(bpres$monop11low, 0.7962, tol = 1e-4)
  expect_equal(bpres$monop11upp, 1.0026, tol = 1e-4) # TODO: bug??
  expect_equal(bpres$monocrrlb, 0.8013, tol = 1e-4)
  expect_equal(bpres$monocrrub, 1.0090, tol = 1e-4)
})

## Balke and Pearl, 1997, Table 2 - 0.001 was 0 in published table ----
tab2cp <- c(.0064, 0, .9936, 0, .0028, 0.001, .1972, .799)
p2 <- array(tab2cp,
           dim = c(2, 2, 2),
           dimnames = list(
             x = c(0, 1),
             y = c(0, 1),
             z = c(0, 1)
           ))
p2 <- as.table(p2)
sum(p2)

test_that("Balke and Pearl Table 2 example: trivariate data with 2 category instrument",
          {
            bpres <- bpbounds(p2, fmt = "trivariate")
            sbp <- summary(bpres)
            print(sbp)

            expect_equal(class(bpres), "bpbounds")
            expect_equal(bpres$fmt, "trivariate")
            expect_equal(bpres$nzcats, 2)

            expect_true(bpres$inequality)
            expect_equal(bpres$bplb, -0.1946, tol = 1e-4)
            expect_equal(bpres$bpub, 0.0054, tol = 1e-4)
            expect_equal(bpres$p10low, 0.9936, tol = 1e-4)
            expect_equal(bpres$p10upp, 0.9936, tol = 1e-4)
            expect_equal(bpres$p11low, 0.7990, tol = 1e-4)
            expect_equal(bpres$p11upp, 0.9990, tol = 1e-4)
            expect_equal(bpres$crrlb, 0.8042, tol = 1e-4)
            expect_equal(bpres$crrub, 1.0054, tol = 1e-4)

            expect_true(bpres$monoinequality)
            expect_equal(bpres$monobplb, -0.1946, tol = 1e-4)
            expect_equal(bpres$monobpub, 0.0054, tol = 1e-4)
            expect_equal(bpres$monop10low, 0.9936, tol = 1e-4)
            expect_equal(bpres$monop10upp, 0.9936, tol = 1e-4)
            expect_equal(bpres$monop11low, 0.7990, tol = 1e-4)
            expect_equal(bpres$monop11upp, 0.9990, tol = 1e-4)
            expect_equal(bpres$monocrrlb, 0.8042, tol = 1e-4)
            expect_equal(bpres$monocrrub, 1.0054, tol = 1e-4)
          })


# Meleady AJCN 2003; Trivariate data with 3 category instrument - Table 3 of paper ----
## Trivariate data
mt3 <- c(.83, .05, .11, .01, .88, .06, .05, .01, .72, .05, .20, .03)
p3 <- array(mt3,
           dim = c(2, 2, 3),
           dimnames = list(
             x = c(0, 1),
             y = c(0, 1),
             z = c(0, 1, 2)
           ))
p3 <- as.table(p3)

test_that("Mendelian randomization with 3 category instrument, trivariate data",
          {
            bpres <- bpbounds(p3)
            expect_true(bpres$inequality)
            expect_equal(bpres$bplb, -0.090, tol = 1e-4)
            expect_equal(bpres$bpub, 0.74, tol = 1e-4)
            expect_equal(bpres$p10low, 0.06, tol = 1e-4)
            expect_equal(bpres$p10upp, 0.12, tol = 1e-4)
            expect_equal(bpres$p11low, 0.03, tol = 1e-4)
            expect_equal(bpres$p11upp, 0.800, tol = 1e-4)
            expect_equal(bpres$crrlb, 0.25, tol = 1e-4)
            expect_equal(bpres$crrub, 13.3333, tol = 1e-4)
            expect_false(bpres$monoinequality)
            sbp <- summary(bpres)
            print(sbp)
          })

## Bivariate data
dat <- data.frame(
  count = c(341, 47, 297, 17, 63, 18, 272, 41, 269, 38, 56, 35),
  z = c(0, 0, 1, 1, 2, 2, 0, 0, 1, 1, 2, 2),
  y = c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1),
  x = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
)
longdat <- tidyr::uncount(dat, weights = count)

gtab <- xtabs(~ y + z, data = longdat)
gp <- prop.table(gtab, margin = 2)
gp

ttab <- xtabs(~ x + z, data = longdat)
tp <- prop.table(ttab, margin = 2)
tp

test_that("Mendelian randomization with 3 category instrument, bivariate data",
          {
            bpres <- bpbounds(p = gp, t = tp, fmt = "bivariate")
            print(bpres)
            expect_true(bpres$inequality)
            expect_equal(bpres$bplb, -0.5720, tol = 1e-4)
            expect_equal(bpres$bpub, 0.5942, tol = 1e-4)
            expect_equal(bpres$p10low, 0.4058, tol = 1e-4)
            expect_equal(bpres$p10upp, 0.5720, tol = 1e-4)
            expect_equal(bpres$p11low, -0.1628, tol = 1e-4)
            expect_equal(bpres$p11upp, 1.2209, tol = 1e-4)
            expect_equal(bpres$crrlb, -0.2846, tol = 1e-4)
            expect_equal(bpres$crrub, 3.009, tol = 1e-4)
            expect_false(bpres$monoinequality)

            sbp <- summary(bpres)
            print(sbp)
          })

## More error checks
test_that("Cond probs and 1 cell count error", {
  cpr <- c(.0064, 0, .9936, 0, .0028, .001, .1972, 20)
  tabpr <- as.table(array(
    cpr,
    dim = c(2, 2, 2),
    dimnames = list(
      x = c(0, 1),
      y = c(0, 1),
      z = c(0, 1)
    )
  ))
  expect_error(bpbounds(tabpr))
})

test_that("Cond probs and 1, giving cond probs sum error", {
  cpr <- c(.0064, 0, .9936, 0, .0028, .001, .1972, 1)
  tabpr <- array(cpr,
                dim = c(2, 2, 2),
                dimnames = list(
                  x = c(0, 1),
                  y = c(0, 1),
                  z = c(0, 1)
                )) |>
    as.table()
  expect_error(bpbounds(tabpr))
})

test_that("Cell counts and one cond prob", {
  cpr <- c(640, 0, 9936, 0, 28, 1, 1972, 0.5)
  tabpr <- array(cpr,
                dim = c(2, 2, 2),
                dimnames = list(
                  x = c(0, 1),
                  y = c(0, 1),
                  z = c(0, 1)
                )) |>
    as.table()
  expect_error(bpbounds(tabpr))
})
