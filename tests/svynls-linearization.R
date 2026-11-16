## the linearization variance of svynls() must use unweighted residuals: nls
## scales both the gradient and the residuals by sqrt(weights), and only the
## gradient was unscaled. For a model linear in its parameters, svynls() and
## svyglm() estimate the same thing, so their standard errors must agree.
library(survey)
data(api)
dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat)
dclus  <- svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
fl <- api00 ~ a + b * ell
for (d in list(dstrat, dclus)) {
  m <- svynls(fl, d, start = list(a = 800, b = -1))
  g <- svyglm(api00 ~ ell, d)
  stopifnot(isTRUE(all.equal(coef(m), coef(g), check.attributes = FALSE, tolerance = 1e-6)))
  stopifnot(isTRUE(all.equal(sqrt(diag(vcov(m))), sqrt(diag(vcov(g))), check.attributes = FALSE)))
}
