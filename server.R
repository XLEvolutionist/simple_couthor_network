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

MakeGraphObject<-function(edgeList,minDegree=1, geneNames,geneCols, delete = .5, dims=3) {
  
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
  coord <- layout.fruchterman.reingold(g, dim = dims)
  #print(get.edge.attribute(g, "weight", index=E(g)))
  GL<-list("G"=g, "L"=coord,"C"=colors)
  return(GL)
}#MakeGraphObject

function(input, output) {
  
  #render some UI
  #output$choose_dataset<-selectInput('select', 'trim to vertices connected to..', choices=unique(authors$name1,authors$name2),
  #                                   selected=unique(authors$name1,authors$name2)[1])
  
  dataset <- reactive({
    authors[authors$citations >= input$minCitations,]
  })
  
      output$plot <- renderPlot({ 
        input$goButton
        names<-c(unique(c(as.character(dataset()$name1))))
  
        G<-MakeGraphObject(dataset(),minDegree=1,geneNames=names, geneCols="black",delete= .5,dims=input$dims)
        #are.connected(G$G, v1=input$select,v2="NULL")
        rownames(G$L)<-names
        e=get.edgelist(G$G,names=T)
        X0=G$L[e[,1],1]
        Y0=G$L[e[,1],2]
        X1=G$L[e[,2],1]
        Y1=G$L[e[,2],2]
        plot(G$L, cex = 4, pch=16, col="red", type="n", axes=FALSE, xlab="", ylab="")
        segments(X0,Y0,X1,Y1,lwd=get.edge.attribute(graph=G$G,name="weight", index=E(G$G))/(20/input$lwd),col=c("darkgrey","black",rainbow(50))[input$colE])
        points(G$L, cex = 2, pch=16, col=alpha(rainbow(n=24)[input$col],input$trans))
        points(G$L, cex = 2, pch=1, col="black")
        text(G$L-input$off, names, cex=input$textSize, col = "black")
   	  }, height=1000, width = 1000)
}