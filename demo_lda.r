source('amcatr.r')
source('amcat_getdata.r')
source('lda_lib.r')

conn = amcat.connect('http://amcat-dev.labs.vu.nl') # AmCAT vraagt om je inloggegevens

articleset_id = 45 # Columns Youp (n = 1323)
features = amcat.getFeatures(conn, articleset_id)



data = lda.prepareFeatures(features, docfreq.thres=5, docfreq_pct.max=5) # Nog splitten in prepareVocabulary en prepareMatrix
m = lda.cluster(data$matrix, data$voca$word, nclusters=200, niterations=100)

top.topic.words(m$topics)

## Het is nog steeds mogelijk om een reference set te gebruiken om woorden te filteren. Volgens mij is dit echter niet cruciaal als er al gefilterd is op woord-document frequentie en POS tags.
## Zie eventueel amcat_featurestream.r voor aantekeningen over wat amcat.getFeatures nu doet en welke parameters nog toegevoegd kunnen worden.