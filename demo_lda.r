source('amcatr.r')
source('amcat_getdata.r')
source('lda_lib.r')

conn = amcat.connect('http://amcat-dev.labs.vu.nl') # AmCAT vraagt om je inloggegevens

target.set = 2467
reference.set = 2474

features.target = amcat.getFeatures(conn, target.set)
features.reference = amcat.getFeatures(conn, reference.set)

data = lda.prepareFeatures(features.target, reference.target, n.thres=5, over.thres=1.5, chi.thres=5)
m = lda.cluster(data$matrix, data$voca.target$word, nclusters=5, niterations=100)
top.topic.words(m$topics)
