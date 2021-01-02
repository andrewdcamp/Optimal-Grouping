library(dplyr)
library(igraph)
library(purrr)
library(cluster)
library(lpSolve)
library(tidyr)

#---assign core group leaders
LeaderInput<-read.csv('/Users/andrewcamp/Desktop/Leaders.csv') #added 8 to equal 36 clusters
ClusteredDf<-read.csv('/Users/andrewcamp/Documents/R Files/Group_Assignment/ClusteredDf.csv')
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
