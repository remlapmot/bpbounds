#' bpbounds: Nonparametric bounds for the Average Causal Effect due to Balke and Pearl and extensions
#'
#' This package implements the nonparametric bounds for the average causal effect defined by
#' Balke and Pearl, Bounds on Treatment Effects from Studies with Imperfect Compliance,
#' JASA, 1997; and some extensions. 
#' The functions implement bounds for the situation where each of the outcome,
#' treatment/phenotype, and instrumental variable are binary; and additionally for when the
#' instrument has 3 categories (e.g. a single genotype under an additive model in a
#' Mendelian randomization study). 
#' The package implements bounds for when the three variables are measured in the same study
#' (trivariate data) and when the outcome and instrument are measured in one study and the
#' treatment/phenotype and instrument in another sample (bivariate/two sample data).
#'
#' @name bpbounds
#' @docType package
NULL
