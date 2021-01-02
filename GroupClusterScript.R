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
LeaderInput<-read.csv('/Users/andrewcamp/Desktop/Leaders.csv')

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

#---assign core group leaders
#LeaderInput<-read.csv('/Users/andrewcamp/Desktop/Leaders.csv') #added 8 to equal 36 clusters
nNewLeaders<-seq(length(unique(ClusteredDf$Cluster)) - nrow(LeaderInput), from = 0)[-1]

if(length(nNewLeaders)==0) {
  ExtraLeaders<-NULL
} else if (min(nNewLeaders<1)) {
  ExtraLeaders<-NULL
} else {
  ExtraLeaders<-data.frame(Leaders= paste("Leader_",nNewLeaders),
                         Type="Extra",
                         CoreGroupLeader = NA)
}

# add any addtional leaders to existing leaders
Leaders<-data.frame(Leaders = unique(LeaderInput$Leaders), 
                    Type = 'Standard', 
                    CoreGroupLeader = unique(LeaderInput$Leaders))%>%
  rbind(ExtraLeaders)%>%
  filter(!Leaders=='None')

# if there are more leaders than groups, use the first n leaders for n clusters
if (nrow(Leaders)>length(ClusteredDf$Cluster%>%unique())) {
  Leaders<-Leaders[1:length(ClusteredDf$Cluster%>%unique()),]
}

# merge with cluster output, 
LeaderMerge<-merge(ClusteredDf, Leaders, by = 'CoreGroupLeader', all = TRUE) %>% modify_if(is.character, as.factor)%>%
  filter(!is.na(Cluster))%>%
  filter(!Leaders=='None')

# count the occurances of each leader from last year by cluster - make into distance matrix
LeaderCount<-LeaderMerge%>%
  group_by(Cluster, Leaders, .drop = FALSE)%>%summarise(Count = n())%>%
  merge(Leaders[,1:2], on = Leaders) %>%
  mutate(Count = ifelse(Type=='Extra',10, Count))

# to matrix form
mx<-pivot_wider(LeaderCount%>%select(-Type), names_from = Leaders, values_from = Count)%>%
  as.matrix()
rownames(mx)<-mx[,1]
mx<-t(mx[,-1])

# Run assignment problem - minimize count of people who had leader last year who have same leader this year
lpassign <- lp.assign(mx, direction = "min")

rownames(lpassign$solution)<-rownames(mx)
colnames(lpassign$solution)<-colnames(mx)

Assignments<-as.data.frame(as.table(lpassign$solution))%>%
  filter(!Freq==0)%>%
  select('Var1', 'Var2')%>%
  rename('Leader'='Var1', 'Cluster'='Var2')

OutputDf<-ClusteredDf%>%merge(Assignments, by = 'Cluster')

#write.csv(OutputDf, '/Users/andrewcamp/Desktop/OutputDf.csv')

#---Summary stats
# count in each group
ClusteredDf%>%group_by(Cluster)%>%summarise(n())

# age group Count
AgeGroupCount<-merge(
  ClusteredDf%>%
    group_by(Cluster, AgeGroup)%>%summarise(Count = n()),
  
  ClusteredDf%>%
    group_by(Cluster, AgeGroup)%>%summarise(Count = n())%>%
    group_by(AgeGroup)%>%summarise(AveCount = Count%>%mean()%>%round(0)),
  
  by = 'AgeGroup'
)%>%
  mutate(Difference.From.Ave = AveCount - Count)%>%
  select(-AveCount)

AgeGroupCount<-AgeGroupCount[,c(2,1,3)]
AgeGroupCount %>% group_by(AgeGroup) %>% summarise(ave = mean(Count), stDev = sd(Count))

# Denomination Count
DenomCount<-merge(
  ClusteredDf%>%
    group_by(Cluster, Denomination)%>%summarise(Count = n()),
  
  ClusteredDf%>%
    group_by(Cluster, Denomination)%>%summarise(Count = n())%>%
    group_by(Denomination)%>%summarise(AveCount = Count%>%mean()%>%round(0)),
  
  by = 'Denomination'
)%>%
  mutate(Difference.From.Ave = AveCount - Count)%>%
  select(-AveCount)

DenomCount %>% group_by(Denomination) %>% summarise(ave = mean(Count), stDev = sd(Count))

DenomCount<-DenomCount[,c(2,1,3)]

# ---Create graph
g<-graph_from_edgelist(Assignments%>%as.matrix())
EL<-OutputDf%>%select('Leader', 'Name')%>%filter(Leader=='Cindy Cooper')
g<-graph_from_edgelist(EL%>%as.matrix())

M.Adj<-matrix_add(LeaderM, AgeM)
M.Adj<-matrix_add(M.Adj, DenomM)
g<-graph_from_adjacency_matrix(M.Adj, weighted = TRUE, mode = "undirected")
V(g)$Cluster<-lab.hcb
E(g)$weight <- edge.betweenness(g)

edgeDf<-as_data_frame(g, what = c("edges"))
nodeDf<-as_data_frame(g, what = c("vertices"))

plot(g, vertex.label = V(g)$Cluster, color = V(g)$Cluster)
