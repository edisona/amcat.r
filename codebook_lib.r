codebook.getChildren <- function(hierarchy, code){
  children = as.character(hierarchy[hierarchy$parent==code,]$code)
  allchildren = children
  while(length(children) > 0){
    children = as.character(hierarchy[hierarchy$parent %in% children,]$code)
    allchildren = c(allchildren, children)
  }
  allchildren
}

codebook.aggCode <- function(hits, hierarchy, code){
  columns = c(code, as.character(codebook.getChildren(hierarchy, code)))
  occ = hits[hits$code %in% columns,]
  print(code)
  print(paste("    nr of children =", length(columns)))
  if(nrow(occ) > 0) {
    occ = tapply(occ$hits, occ$id, FUN='sum')
    occ = data.frame(id=as.numeric(names(occ)),agg_hits=occ, code=code, row.names=NULL)
  } else occ = data.frame(id=c(), agg_hits=c(), code=c())
  occ
}

codebook.aggAllCodes <- function(hits, hierarchy, codes=c()){
  if(length(codes)==0) codes = unique(c(as.character(hierarchy$parent),as.character(hierarchy$code)))
  codes = as.character(codes)
  aggscores = data.frame()
  print('Aggregating hits')
  for(code in codes){
    aggscore = codebook.aggCode(hits, hierarchy, code)
    if(nrow(aggscore)==0) next
    aggscores = rbind(aggscores, aggscore)
  }
  print('Done!')
  aggscores
}

codebook.appendAggHits <- function(hits, hierarchy){
  hits = hits[,colnames(hits)[!colnames(hits) == 'agg_hits']]
  agghits = codebook.aggAllCodes(hits, hierarchy)
  agghits = merge(agghits, hits, by=c('id','code'), all.x=T)
  agghits$hits[is.na(agghits$hits)] = 0
  agghits
}


