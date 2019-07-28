require('igraph')
require('sets')
plotPoolGraphFromTupleLists=function(
        internalConnections
        ,inBoundConnections
        ,outBoundConnections
){

    #internalConnections<-list(tuple(1,2),tuple(2,3),tuple(3,1),tuple(3,4))
    #inBoundConnections<-list(1,3)
    #outBoundConnections<-list(4)
    realId=function(i){paste('',i,sep='')}
    realSize=20
    virtualSize=0
    
    virtualInNodes <- lapply(
    	inBoundConnections
        ,function(i){
            data.frame(
                 id=paste('v_in_',i,sep='')
                ,size=virtualSize
                ,visible=FALSE
            )
        }
    )
    realInNodes <- lapply(
        inBoundConnections
        ,function(i){
            data.frame(
                 id=realId(i)
                ,size=realSize
                ,visible=TRUE
            )
        }
    )
    
    virtualOutNodes <- lapply(
         outBoundConnections
        ,function(i){
            data.frame(
                 id=paste('v_out_',i,sep='')
                ,size=virtualSize
                ,visible=FALSE
            )
        }
    )
    realOutNodes <- lapply(
        outBoundConnections
        ,function(i){
            data.frame(
                 id=realId(i)
                ,size=realSize
                ,visible=TRUE
            )
        }
    )
    
    InternalStartNodes <- lapply(
        internalConnections
        ,function(t){
            data.frame(
                id=realId(t[1])
                ,size=realSize
                ,visible=TRUE
            )
        }
    )
    InternalEndNodes <- lapply(
        internalConnections
        ,function(t){
            data.frame(
                id=realId(t[2])
                ,size=realSize
                ,visible=TRUE
            )
        }
    )
    
    # combine all the small dataframes to the one used for the graph
    nodes<-Reduce(
         rbind
        ,unique(
          c(
             virtualInNodes		
            ,realInNodes		
            ,virtualOutNodes		
            ,realOutNodes		
            ,InternalStartNodes	
            ,InternalEndNodes	
          )
        )
    )
    
    
    InEdges=lapply(
    	1:length(virtualInNodes)
    	,function(i){
    		data.frame(
    			    source=virtualInNodes[[i]][['id']]
    			   ,target=realInNodes[[i]][['id']]
    		)
    	}
    )
    InternalEdges=lapply(
    	1:length(InternalStartNodes)
    	,function(i){
    		data.frame(
    			    source=InternalStartNodes[[i]][['id']]
    			   ,target=InternalEndNodes[[i]][['id']]
    		)
    	}
    )
    OutEdges=lapply(
    	1:length(virtualOutNodes)
    	,function(i){
    		data.frame(
    			    source=realOutNodes[[i]][['id']]
    			   ,target=virtualOutNodes[[i]][['id']]
    		)
    	}
    )
    edges<-Reduce(
         rbind
        ,c(
             InEdges
            ,InternalEdges
            ,OutEdges
        )
    )
    #n3=data.frame(id=1:4,n=c(rep(1,3),20),label=c("a","b","c","invisible"),shapes=c(rep('circle',3),'rectangle')
    #e3=data.frame(from=c(1:3,1:3),to=c(rep(4,3),2:4))
    #net<-graph_from_data_frame(d = e3, vertices = n3,directed = TRUE)
    #V(net)$size <- V(net)$n*50
    #plot(net,vertex.shape='circle',vertex.size=n3$size,alpha=0.9)
    
    net<-graph_from_data_frame(d = edges, vertices = nodes,directed = TRUE)
    #graph_attr(net, "layout") <- layout_with_lgl
    graph_attr(net, "layout") <- layout_with_sugiyama(net)$layout
    plot(
        net
        ,vertex.shape='circle'
        ,vertex.size=nodes$size
        ,vertex.label=ifelse(nodes$visible,as.character(nodes$id),NA)
        ,alpha=0.1
        ,edge.arrow.size=.4
        ,edge.arrow.width=.8
    )
}
