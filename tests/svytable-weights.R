
library(survey)
data(api)
apisrs$weights<-apisrs$enroll
dsrs<-svydesign(id=~1, weights=~pw, data=apisrs)
dsrs1<-svydesign(id=~1, weight=~pw, data=apisrs[,c("pw","comp.imp","stype")])

stopifnot(all.equal(as.vector(svytable(~comp.imp+stype,design=dsrs)),
          as.vector(svytable(~comp.imp+stype,design=dsrs1))))
