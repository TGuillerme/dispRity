#Generating random tree parameters (birth death)
gen.param.tree<-function(x=1) {
    lambda<-runif(x)
    mu<-runif(x,0,lambda)
    return(cbind(lambda, mu))
}

#Generates a Q matrix
Q.matrix<-function(states, rate) {
    #Creating the empty matrix
    Q_mat<-matrix(nrow=states, ncol=states, data=0)
    colnames(Q_mat)<-rownames(Q_mat)<-0:(states-1)
    #Adding the rates to the matrix (rates are equal)
    Q_mat[upper.tri(Q_mat)]<-Q_mat[lower.tri(Q_mat)]<-rate/(states-1)
    diag(Q_mat)<- -(states-1)*(rate/(states-1))
    return(Q_mat)
}


#Generates a fully random matrix
random.mat<-function(tree, characters, states, include.nodes) {
    #include nodes?
    if(include.nodes == TRUE) {
        rows<-Ntip(tree)+Nnode(tree)
    } else {
        rows<-Ntip(tree)
    }
    #creates and empty matri
    rand_matrix<-matrix(nrow=rows, ncol=characters, data=NA)
    #loop through the characters
    for (character in 1:characters) {
        rand_matrix[,character]<-sample(0:(states[character]-1), size=rows, replace=TRUE)
    }
    #Add rownames
    if(include.nodes == TRUE) {
        rownames(rand_matrix)<-c(tree$tip.label, tree$node.label)
    } else {
        rownames(rand_matrix)<-tree$tip.label
    }

    return(rand_matrix)
}

#Renaming the column names for all the matrices
renaming.rows<-function(matrix, names) {
    rownames(matrix)<-names
    return(matrix)
}

#Generates a simulated matrix
simulate.mat<-function(tree, matrix_characters, matrix_states, max.rate, include.nodes) {
    #include nodes?
    if(include.nodes == TRUE) {
        rows<-Ntip(tree)+Nnode(tree)
    } else {
        rows<-Ntip(tree)
    }    
    #Creating the empty matrix
    rand_matrix<-matrix(nrow=rows, ncol=matrix_characters, data=NA)
    #character loop
    for(character in 1:matrix_characters) {
        if(matrix_states[character] == 2) {
            #binary
            nodes_states<-attr(tips_states<-sim.character(tree, rep(runif(1, 0, max.rate),2), x0=sample(0:1, 1), model="mk2"), "node.state")
            if(include.nodes == TRUE) {
                rand_matrix[,character]<-c(tips_states, nodes_states)
            } else {
                rand_matrix[,character]<-tips_states
            }
            
        } else {
            #Create a Q matrix
            Q_mat<-Q.matrix(matrix_states[[character]], runif(1, 0, max.rate))
            #Change the states from 0:k to 1:k+1
            rownames(Q_mat)<-colnames(Q_mat)<-as.numeric(colnames(Q_mat))+1
            #not binary
            nodes_states<-attr(tips_states<-sim.character(tree, Q_mat, x0=sample(1:matrix_states[character], 1), model="mkn")-1, "node.state")-1
            if(include.nodes == TRUE) {
                rand_matrix[,character]<-c(tips_states, nodes_states)
            } else {
                rand_matrix[,character]<-tips_states
            }
        }
    }
    #Add rownames
    if(include.nodes == TRUE) {
        rownames(rand_matrix)<-c(tree$tip.label, tree$node.label)
    } else {
        rownames(rand_matrix)<-tree$tip.label
    }
    return(rand_matrix)
}