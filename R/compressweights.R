
"dim.repweights_compressed"<-function(x){
  c(length(x$index),ncol(x$weights))
}

"dimnames.repweights_compressed"<-function(x){
  list(names(x$index), colnames(x$weights))
}

"[.repweights_compressed"<-function(x,i,...,drop=FALSE){
  if (!missing(i)){
    x$index<-x$index[i]
    if(!missing(..1))
      x$weights<-x$weights[,..1,drop=FALSE]
  } else{
      ## this is faster than just subscripting x$weights (!)
      x<-list(index=x$index,
              weights=x$weights[,...,drop=FALSE])
      class(x)<-c("repweights_compressed","repweights")
  }
  x
}

"as.matrix.repweights_compressed"<-function(x,...){
  x$weights[x$index,,drop=FALSE]
}

"as.vector.repweights_compressed"<-function(x,...){
  as.vector(x$weights[x$index,])
}

"as.matrix.repweights"<-function(x,...){
  x
}

compressWeights<-function(rw,...){
  UseMethod("compressWeights")
}

"compressWeights.repweights_compressed"<-function(rw,...){
  compressWeights(as.matrix(rw))
}

compressWeights.default<-function(rw,...){
  mat<-as.matrix(rw)
  index<-rep.int(1L,nrow(mat))
  if (nrow(mat)>1L && ncol(mat)>0L){
    ## grouping() rounds doubles, so give it exact integer value codes.
    codes<-lapply(seq_len(ncol(mat)), function(j){
      x<-mat[,j]
      match(x,unique(x))
    })
    grouped<-do.call(grouping,codes)
    ends<-attr(grouped,"ends")
    index[grouped]<-rep.int(seq_along(ends),diff(c(0L,ends)))
    ## Store representatives in their original order of appearance.
    index<-match(index,unique(index))
  }
  rval<-list(weights=mat[!duplicated(index),,drop=FALSE],index=index)
  class(rval)<-c("repweights_compressed","repweights")
  rval
}

compressWeights.svyrep.design<-function(rw,...){
  rw$repweights<-compressWeights(rw$repweights,...)
  rw
}
