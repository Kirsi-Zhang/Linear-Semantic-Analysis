library(tm)
docs<-Corpus(DirSource( "~/Desktop/Social Media/specialty3" ))

#SVD
tdm.full <- TermDocumentMatrix(docs, control=list(tolower=T, removePunctuation=T, removeNumbers=T, stripWhitespace=T, stopwords=stopwords("english")))
tdm = removeSparseTerms(tdm.full,0.8)  
X = as.matrix(tdm)
X.svd = svd(X)
D = diag(X.svd$d)
U <- X.svd$u
V <- X.svd$v
X

#Place documents in a 2-dimensional semantics space
dim=2
Uk = U[,seq(1,dim)]
Dk = D[seq(1,dim),seq(1,dim)]
Vk = V[,seq(1,dim)]
rownames(Uk) = rownames(X)
rownames(Vk) = colnames(X)

#projection on semantic space
term.proj = Uk %*% Dk
doc.proj = Dk %*% t(Vk)

#Compare the document clusters with the ground truth
require(ggplot2)
colnames(doc.proj) = gsub("-\\d+" ," ",rownames(Vk))
doc.plot <- data.frame(x=doc.proj[1,], y=doc.proj[2,])
ggplot(doc.plot, aes(x,y)) + geom_point(aes(color = colnames(doc.proj)))


#Cluster with k-dimensional Representation
clusters = 3

set.seed(1234)
km.out = kmeans(t(doc.proj), clusters)
km.out$tot.withinss
library(cluster)
library(fpc)

clusplot(doc.plot, km.out$cluster,color=TRUE, shade=TRUE,
         labels=2, lines=0)
