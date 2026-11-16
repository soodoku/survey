## the lonely.psu argument of svyrecvar() must be used (it was ignored in
## favour of options("survey.lonely.psu"))
library(survey)
data(api)
D <- apistrat[c(which(apistrat$stype == "E")[1:6], which(apistrat$stype == "H")[1],
                which(apistrat$stype == "M")[1:4]), ]
d1 <- svydesign(id = ~1, strata = ~stype, weights = ~pw, data = D)  # stratum H has one PSU
x <- as.matrix(D$api00 * weights(d1)); x <- x - mean(x)
op <- options(survey.lonely.psu = "remove")
v_arg <- sapply(c("remove", "adjust", "average"), function(l)
  svyrecvar(x, d1$cluster, d1$strata, d1$fpc, lonely.psu = l))
v_opt <- sapply(c("remove", "adjust", "average"), function(l) {
  options(survey.lonely.psu = l); svyrecvar(x, d1$cluster, d1$strata, d1$fpc) })
options(op)
stopifnot(isTRUE(all.equal(v_arg, v_opt)))
stopifnot(v_arg[["adjust"]] > v_arg[["remove"]])
