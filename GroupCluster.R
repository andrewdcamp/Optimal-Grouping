library(dplyr)
library(igraph)
library(purrr)
library(cluster)
library(lpSolve)
library(tidyr)

# read in files
df<-read.csv("/Users/andrewcamp/Downloads/CoreGroupSort.csv") %>%
  mutate(Name = paste(trimws(First.Name), trimws(Last.Name), sep = " "))
DenomInput<-read.csv('/Users/andrewcamp/Documents/R Files/Group_Assignment/churches.denominaton.csv')

# merge in denominations from HomeChurch list
df<-merge(df, DenomInput, by = 'HomeChurch', all.x = TRUE) %>%
  filter(!Name %in% LeaderInput$Leaders)

# clean blanks and NAs
df$Denomination<-ifelse(is.na(df$Denomination), 'None', as.character(df$Denomination))
colLst<-c('AgeGroup', 'HomeChurch', 'Denomination', 'CoreGroupLeader')
df[,colLst]<-as.data.frame(lapply(df[,colLst], function(y) sub("^$", "None", y)))

# Create a distance matrix
createDistMtrx<-function(df1, column, wt){
  df1<-df1 %>% select(Name, column)
  
  column = enquo(column)
  by = set_names(quo_name(column), quo_name(column))
  
  df1<-full_join(df1, df1, by = by) %>% 
    # We no longer need CoreGroupLeader -> remove
    select(-!!column) %>% 
    # Dont allow self-loops
    filter(Name.x != Name.y) %>% 
    # Aggregate duplicate edges: vertices linked using multiple properties
    group_by(Name.x, Name.y) %>% 
    summarise(weight = n())
  
  M<-graph_from_data_frame(df1, directed = TRUE)%>%
    as_adjacency_matrix(attr = "weight")%>%
    as.matrix()*wt
  
  M
}

# create independent distance matrices
LeaderM<-createDistMtrx(df, 'CoreGroupLeader', 10)
AgeM<-createDistMtrx(df, 'AgeGroup', 5)
DenomM<-createDistMtrx(df, 'Denomination', 3)

# matrix combine function
matrix_add<-function(A,B) {
  cAB <- union(colnames(A), colnames(B))
  rAB <- union(rownames(A), rownames(B))
  A1 <- matrix(0, ncol=length(cAB), nrow=length(rAB), dimnames=list(rAB, cAB))
  B1 <- A1
  indxA <- outer(rAB, cAB, FUN=paste) %in% outer(rownames(A), colnames(A), FUN=paste) 
  indxB <- outer(rAB, cAB, FUN=paste) %in% outer(rownames(B), colnames(B), FUN=paste)
  A1[indxA] <- A
  B1[indxB] <- B
  
  A1+B1
}

# add matrices
M<-matrix_add(LeaderM, AgeM) %>%
  matrix_add(DenomM)

# Cluster
clsize = 16

# heirachical bottom cluster function
hcbottom <- function(mat, clsize = 10, method='ward.D'){
  #dmat = as.matrix(dist(mat))
  dmat = as.matrix(mat)
  clsize.rle = rle(as.numeric(cut(1:nrow(mat), ceiling(nrow(mat)/clsize))))
  clsizes = clsize.rle$lengths
  cpt = 1
  lab = rep(NA, nrow(mat))
  for(clss in clsizes[-1]){
    lab.ii = which(is.na(lab))
    hc.o = hclust(as.dist(dmat[lab.ii, lab.ii]), method=method)
    clt = 0
    ct = length(lab.ii)-clss
    while(max(clt)<clss){
      cls = cutree(hc.o, ct)
      clt = table(cls)
      ct = ct - 1
    }
    cl.sel = which(cls == as.numeric(names(clt)[which.max(clt)]))
    lab[lab.ii[head(cl.sel, clss)]] = cpt
    cpt = cpt + 1
  }
  lab[which(is.na(lab))] = cpt
  lab
}

# cluster
lab.hcb = hcbottom(M, clsize, 'ward.D')
ClusteredDf<-merge(data.frame(Name = labels(M)[[1]], Cluster = lab.hcb), 
                   df, 
                   by = 'Name')

write.csv(ClusteredDf, '/Users/andrewcamp/Desktop/ClusteredDf.csv')
