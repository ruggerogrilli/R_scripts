dir<-getwd()
setwd(dir)
library(igraph)
library(Matrix)



data<-read.table("df_real_credit_network_B0.00_BC5.00_l33.33_day1")
frame<-data.frame(time=data$V1, banks=data$V2,firms=data$V3, weights=data$V4)
ncomp<-mat.or.vec(max(frame$time),1)
for(i in 1:max(frame$time)){
   day<-i
   sub<-subset(frame, time==day, select = c(firms, banks, weights))
   if(length(sub$firms > 0)){
      A<-spMatrix(nrow=length(unique(sub$firms)), ncol=length(unique(sub$banks)), i=as.numeric(factor(sub$firms)), j=as.numeric(factor(sub$banks)), x=sub$weights)
         rownames(A)<-levels(factor(sub$firms))
         colnames(A)<-levels(factor(sub$banks))
         G<-graph.incidence(A, directed=TRUE, mode=c("in"))
         comps<-decompose.graph(G, mode = c("weak"), min.vertices = 2)
         ncomp[i]<-length(comps)   
   }
}
