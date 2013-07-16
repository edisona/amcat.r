library(lda)
library(reshape)

lda.create.matrix <- function(wordnrs, freqs, documents) {
  # wordnrs = vector of word indexes into voca
  # freqs = vector of frequencies of the same length as words
  # documents = vector of documents indices that has the samen length as words
  # voca = character vector of words
  
  docs = unique(documents)
  
  n = length(docs)
  corpus = vector("list", n)
  i = 2
  for (i in 1:n) {
    if (i%%100 == 0) print(paste("Document",i," / ", n))
    
    select = documents == docs[i]
    
    corpus[[i]] = matrix(as.integer(c(wordnrs[select] - 1, freqs[select])), nrow=2, byrow=T)
  }
  corpus
}

lda.cluster <- function(corpus, voca, nclusters = 25, niterations=25) {
  lda.collapsed.gibbs.sampler(corpus,
                              nclusters,
                              voca,
                              niterations,
                              0.1,
                              0.1,
                              compute.log.likelihood=TRUE)
}

chi2 <- function(a,b,c,d) {
  # Compute the chi^2 statistic for a 2x2 crosstab containing the values
  # [[a, b], [c, d]]
  ooe <- function(o, e) {(o-e)*(o-e) / e}
  tot = 0.0 + a+b+c+d
  a = as.numeric(a)
  b = as.numeric(b)
  c = as.numeric(c)
  d = as.numeric(d)
  (ooe(a, (a+c)*(a+b)/tot)
   +  ooe(b, (b+d)*(a+b)/tot)
   +  ooe(c, (a+c)*(c+d)/tot)
   +  ooe(d, (d+b)*(c+d)/tot))
}

lda.prepareData <- function(target.set, reference.set, n.thres=5, over.thres=1.5, chi.thres=5, use.pos=c("V","N","A")) {
  tokens.target = amcat.getTokens(conn, target.set, articleids=T)
  tokens.reference = amcat.getTokens(conn, reference.set, articleids=T)
  
  tokens.all = rbind(tokens.target, tokens.reference)
  tokens.all$source = c(rep('target', nrow(tokens.target)), rep('reference', nrow(tokens.reference)))
  
  words = cast(tokens.all, wordid ~ source, value="n", fun.aggregate=sum)
  
  words = words[words$target > n.thres, ]
  w = amcat.getWords(conn, words$wordid)
  words = merge(w, words)
  
  words$chi = chi2(words$target, words$reference, sum(words$target) - words$target, sum(words$reference) - words$reference)
  words$over = (words$target / words$reference) / (sum(words$reference) / sum(words$target))
  
  voca.target = words[words$over > over.thres & words$chi > chi.thres & words$pos %in% use.pos, ]
  voca.target = voca.target[order(voca.target$over), ]
  
  t.target = tokens.target[tokens.target$wordid %in% voca.target$wordid,  ]
  ldamatrix = lda.create.matrix(match(t.target$wordid, voca.target$wordid), t.target$n, t.target$article_id)
  list(matrix=ldamatrix, voca.target=voca.target, article_ids=unique(t.target$article_id))
}
