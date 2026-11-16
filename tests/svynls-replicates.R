## svynls() with a replicate-weights design must refit the model for every
## replicate: the loop over replicates ran only for the last one, leaving the
## other replicate estimates at 0
library(survey)
data(api)
dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat)
rstrat <- as.svrepdesign(dstrat, type = "JKn")
f <- api00 ~ a * exp(b * ell / 100)
m <- svynls(f, rstrat, start = list(a = 900, b = -0.5), return.replicates = TRUE)
repw <- weights(rstrat, "analysis")
dat <- rstrat$variables
byhand <- t(sapply(seq_len(ncol(repw)), function(i) {
  dat$.w <- repw[, i]
  coef(nls(f, dat, weights = .w, start = coef(m)))
}))
stopifnot(all(m$replicates != 0))
stopifnot(isTRUE(all.equal(unname(m$replicates), unname(byhand), tolerance = 1e-5)))
stopifnot(isTRUE(all.equal(vcov(m), svrVar(byhand, rstrat$scale, rstrat$rscales, coef = coef(m)),
                           check.attributes = FALSE, tolerance = 1e-5)))
## for a model linear in its parameters, the replicate variance equals svyglm's
fl <- api00 ~ a + b * ell
stopifnot(isTRUE(all.equal(sqrt(diag(vcov(svynls(fl, rstrat, start = list(a = 800, b = -1))))),
                           sqrt(diag(vcov(svyglm(api00 ~ ell, rstrat)))), check.attributes = FALSE)))
