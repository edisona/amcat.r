source('amcatr.r')
source('amcat_getdata.r')
source('lda_lib.r')

conn = amcat.connect('http://amcat.vu.nl') # AmCAT vraagt om je inloggegevens

target.set = 2467
reference.set = 2474

data = lda.prepareData(target.set, reference.set, n.thres=5, over.thres=1.5, chi.thres=5, use.pos=c("V","N","A"))

m = lda.cluster(data$matrix, data$voca.target$word, nclusters=5, niterations=100)

top.topic.words(m$topics)