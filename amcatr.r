library(rjson)
library(RCurl)

amcat.connect <- function(username, passwd, host) {
  list(passwd=passwd, username=username, host=host)
  
}

amcat.getobjects <- function(conn, resource, format='csv', stepsize=50000, filters=list()) {
  limit = stepsize
  url = paste(conn$host, '/api/v4/', resource, '?format=', format, '&page_size=', limit, sep='')
  if (length(filters) > 0) {
    for (i in 1:length(filters))
      url = paste(url, '&', names(filters)[i], '=', filters[[i]], sep='')
  }
  
  opts = list(userpwd=paste(conn$username,conn$passwd,sep=':'), ssl.verifypeer = FALSE, httpauth=1L)
  print(paste("Getting objects from", url))
  page = 1
  result = data.frame()
  while(TRUE){
    subresult = getURL(paste(url, '&page=', as.integer(page), sep=''), .opts=opts)
    subresult = amcat.readoutput(subresult, format=format)
    result = rbind(result, subresult)
    #print(paste("Got",nrow(subresult),"rows, expected", stepsize))
    if(nrow(subresult) < stepsize) break
    page = page + 1
  }
  result
}

amcat.readoutput <- function(result, format){
  if (result == '401 Unauthorized')
    stop("401 Unauthorized")
  if (format == 'json') {
    result = fromJSON(result)
    
  } else  if (format == 'csv') {
    con <- textConnection(result)
    result = tryCatch(read.csv(con), 
                      error=function(e) {warning(e); data.frame()})
  }
  result
}


amcat.runaction <- function(conn, action, format='csv', ...) {
  resource = 'api/action'
  url = paste(conn$host, resource, action, sep="/")
  url = paste(url, '?format=', format, sep="")
  print(paste("Running action at", url))
  opts = list(userpwd=paste(conn$username,conn$passwd,sep=':'), ssl.verifypeer = FALSE, httpauth=1L)
  result = postForm(url, ..., .opts=opts)
  
  if (result == '401 Unauthorized')
    stop("401 Unauthorized")
  if (format == 'json') {
    result = fromJSON(result)
  } else  if (format == 'csv') {
    con <- textConnection(result)
    result = read.csv2(con)
  }
  result
}

amcat.gethierarchy <- function(codebook_id, password, username, host) {
  labels = amcat.getobjects("label",  password, username, host=host, filters=list(language=2))

  hierarchy = amcat.getobjects("codebookcode", password, username, host=host, filters=list(codebook_id=codebook_id))
  hierarchy = hierarchy[,c("X_code", "X_parent")]
  
  hierarchy = merge(hierarchy, labels, by.x="X_code", by.y="code", all.x=T)[,c("X_code", "label", "X_parent")]
  colnames(hierarchy) = c("code_id", "code", "parent_id")
  hierarchy = merge(hierarchy, labels, by.x="parent_id", by.y="code", all.x=T)[,c("code_id", "code", "label")]
  colnames(hierarchy) = c("code_id", "code", "parent_0")
  
  for (i in 1:4) {
    h = hierarchy[, c("code", "parent_0")]
    colnames(h) = c(paste("parent", i-1, sep="_"), paste("parent", i, sep="_"))
    hierarchy = merge(hierarchy, h, all.x=T)
  }
  
  i = 1
  columns = c("code", paste("parent", 0:4, sep="_"))
  hierarchy$cat = NA
  for (i in 1:length(columns)) {
    col = hierarchy[, columns[i]]
    hierarchy$root[!is.na(col)] = as.character(col[!is.na(col)])
    if (i < length(columns)) {
      parent = hierarchy[, columns[i+1]]
      hierarchy$cat[!is.na(parent)] = as.character(col[!is.na(parent)])
    }
  }
  
  hierarchy[, c("code_id", "code", "cat", "root")]
}




