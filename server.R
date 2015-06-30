library(shiny)
library(ggplot2)
library(igraph)
library(scales)


#load in the data
authors<-read.table("author.table", header=T)
authors$citations<-as.numeric(authors$citations)
#authors<-authors[,c(1,3,2)]
print(mode(authors$citations))


##
# Define a useful function
##
plotg<-function(G,wlim=NULL,col="black",cex=0.4,lwd=.5,bg="white",seg.col="black",...){
  
  #e=get.edgelist(GG,names=F)+1
  e=get.edgelist(G$G,names=F)
  w=E(G$G)$weight
  if (!is.null(wlim)) {e=e[w>wlim,]; w=w[w>wlim]}
  X0=G$L[e[,1],1]
  Y0=G$L[e[,1],2]
  X1=G$L[e[,2],1]
  Y1=G$L[e[,2],2]
  par(bg=bg)
  plot(range(G$L[,1]),range(G$L[,2]),xlab="",ylab="",axes=F,type="n",)
  brv=col
  segments(X0,Y0,X1,Y1,lwd=lwd,col=seg.col)
  points(G$L,pch=1,cex=cex+0.1,col="black",...)
  points(G$L,pch=19,cex=cex,col=alpha(G$C, 0.5),...)
  par(bg="white")
}

MakeGraphObject<-function(edgeList,minDegree=1, geneNames,geneCols, delete = .5) {
  
  #Set up some necessary params  
  #len<-length(unique(c(edgeList[,1], edgeList[,2])))
  names<-geneNames
  #create the graph
  g<-graph.empty(n=0, directed=FALSE)
  #add vertices, one for each gene
  g<-add.vertices(g,nv=length(names), name=names)
  #add edges
  #print(edgeList[,3])
  g[from=as.character(edgeList[,1]),to=as.character(edgeList[,2]),attr="weight"]<-as.numeric(edgeList[,3])
  g<-set.edge.attribute(g,"weight",index=E(g),value=as.numeric(edgeList[,3]))
  #remove nodes with fewer edges than "minDegree"
  #g<-delete.vertices(g, which(degree(g) < minDegree))
  #simplify
  g<-simplify(g)
  #calcualte node layout
  coord <- layout.fruchterman.reingold(g, dim = 3)
  #print(get.edge.attribute(g, "weight", index=E(g)))
  GL<-list("G"=g, "L"=coord,"C"=colors)
  return(GL)
}#MakeGraphObject

function(input, output) {
  
  dataset <- reactive({
    authors[authors$citations >= input$minCitations,]
  })
output$plot <- renderPlot({ 
  names<-c(unique(c(as.character(dataset()$name1))))
  print(dataset())
  #print(names)
  G<-MakeGraphObject(dataset(),minDegree=1,geneNames=names, geneCols="black",delete= .5)
  
  #print(names)
  #plotg(G)
  rownames(G$L)<-names
  e=get.edgelist(G$G,names=T)
   #print(e)
   X0=G$L[e[,1],1]
   Y0=G$L[e[,1],2]
   X1=G$L[e[,2],1]
   Y1=G$L[e[,2],2]
   plot(G$L, cex = 4, pch=16, col="red", type="n", axes=FALSE, xlab="", ylab="")
   segments(X0,Y0,X1,Y1,lwd=get.edge.attribute(graph=G$G,name="weight", index=E(G$G))/5,col="grey")
   points(G$L, cex = 2, pch=16, col="orange")
   points(G$L, cex = 2, pch=1, col="black")
   text(G$L, names, cex=1, col = "black")
   	}, height=700)
}