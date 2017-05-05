#Some links to refer
#http://stackoverflow.com/questions/40122021/converting-a-data-frame-into-adjacency-matrix-edge-list-for-network-analysis
#http://stackoverflow.com/questions/13281303/creating-co-occurrence-matrix
#http://stackoverflow.com/questions/21419507/adjacency-matrix-in-r



library(dplyr)

#read the csv file containing information about Fortune 500 companies from WRDS
#only information about boards and members are read
data <- read.csv("C:/Users/Arko/Downloads/Studies/PersonalResearch/NetworkProject/Fortune500_WRDS_onlyboardsandmembers.csv",
                 header = T)
#subset for only unique combinations of the two columns
data <- unique(data)
#removing unnecessary column
data$X <- NULL
#Optional: storing the unique data for cross-referencing
#write.csv(data,
#          "C:/Users/Arko/Downloads/Studies/PersonalResearch/NetworkProject/testset2.csv")

#Computing Similarity between boards in terms of the Members present
#calculate the jaccard distance
jacc1 <- dist(table(data),method = "binary",diag = TRUE, upper = TRUE)
jacc1 <- as.data.frame(as.matrix(jacc1))
#calculate jaccard similarity
jaccsim1 <- 1- jacc1
View(jaccsim1)
#save the similarity matrix
write.csv(jaccsim1,
          "C:/Users/Arko/Downloads/Studies/PersonalResearch/NetworkProject/Fortune500_WRDS_similarity.csv")

#compute degree of connectedness
#define an empty dataframe to store boards and their degrees
#Initialize the degrees with 0
boards.degrees <- data.frame("BoardName" = unique(data$CIK.Board),"Degree" = 0)
#Indicated by the number of members of a particular board member present in other boards
#get the board names
boards <- as.character(unique(data$CIK.Board))
for(i in 1:length(boards)){
  #get the members belonging to i-th board
  inboard.members <- as.character(filter(data,CIK.Board == boards[i])$DirectorName)
  #get the members NOT belonging to i-th board
  outboard.members <- as.character(filter(data,CIK.Board != boards[i])$DirectorName)
  #find the length of the common members  
  degree <- as.numeric(length(inboard.members[inboard.members %in% outboard.members]))
  #update the boards.degrees dataframe
  boards.degrees[i,]$Degree = degree
}
boards.degrees <- arrange(boards.degrees,-Degree)
#save the updated dataframe
write.csv(boards.degrees,
          "C:/Users/Arko/Downloads/Studies/PersonalResearch/NetworkProject/Fortune500_WRDS_Degree.csv")

library(sna)
library(keyplayer)
#convert to dataframe to a matrix
A1 <- as.matrix(jaccsim1)
P1 <- diag(1/apply(A1, MARGIN=1, FUN=sum)) %*% A1
A1net <- network(A1, directed=T) # make A into a network object
A1net
set.seed(1)
#Visualize the network generated.Saved the plot as NetworkofBoards
plot(A1net, label=colnames(P1)) 
#convert to adjacency matrix
W = as.sociomatrix(A1net)
write.csv(W,"C:/Users/Arko/Downloads/Studies/PersonalResearch/NetworkProject/Fortune500_WRDS_AdjacencyMatrix.csv")
#get the edge list
e1 = as.edgelist(A1net)
#get network size
network.size(A1net)
#network graph without labels
#gplot(A1net)
#calculate degree
d1 <- mreach.degree(W, M = 1)[,1]
#get the betweenness measure
b1 <- betweenness(W)
#get eigenvector centrality scores
B = symmetrize(W)
ev1 <- evcent(B)
A <- W
A[W != 0] <- 1 / W[W != 0]
#measure degree of closeness
c1 <- mreach.closeness(A)[,1]
#identify top 10 keyplayers based on degree
kpset(W, size = 10, type = "degree", cmode = "indegree", binary = TRUE,method = "max")
kpset(A, size = 10, type = "mreach.closeness", cmode = "indegree", M = 1)

netwrk.analysis.data <- as.data.frame(cbind(b1,c1,ev1,d1))
names(netwrk.analysis.data) <- c("BetweenNess","Closeness","EigenValues","Degree")
write.csv(netwrk.analysis.data,
          "C:/Users/Arko/Downloads/Studies/PersonalResearch/NetworkProject/Fortune500_WRDS_NetwrkAnalysisMetrics.csv")
