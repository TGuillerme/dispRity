##########################
#Ancestral uncertainty
##########################
#Transforms the ancestral state matrix by excluding all infered states below a certain threshold.
#1.0
#Update: Fixed sanitizing to match with the new anc.matrix type (+rate)
#Update: Synchronized with the anc.state function v1.0
##########################
#SYNTAX :
#<anc.matrix> the ancestral matrix (a list of states and probabilities returned from anc.state)
#<threshold> the probability threshold (a value between 0.5 and 1)
#<missing> any missing character symbol to use
##########################
#----
#guillert(at)tcd.ie 29/09/2014
##########################

anc.unc<-function(anc.matrix, threshold=0.5, missing="?") {
    #SANITIZING
    #anc.matrix
    #class
    check.class(anc.matrix, "list", " must be a list containing at least two elements: \'state\' and \'prob\'.\nUse anc.state() function for proper formating.")
    #length
    if(length(anc.matrix ) < 2) {
        stop("anc.matrix must be a list containing at least two elements: \'state\' and \'prob\'.\nUse anc.state() function for proper formating.")
    }
    #check names
    if(length(grep("state" ,names(anc.matrix))) == 0) {
        stop("anc.matrix must be a list containing at least two elements: \'state\' and \'prob\'.\nUse anc.state() function for proper formating.")
    }
    if(length(grep("prob", names(anc.matrix))) == 0) {
        stop("anc.matrix must be a list containing at least two elements: \'state\' and \'prob\'.\nUse anc.state() function for proper formating.")
    }

    #threshold
    check.class(threshold, 'numeric', ' must be a numerical value between 0.5 and 1.')
    check.length(threshold, 1, ' must be a numerical value between 0.5 and 1.')
    if(threshold < 0.5) {
        stop('threshold must be a numerical value between 0.5 and 1.') 
    }
    if(threshold > 1) {
        stop('threshold must be a numerical value between 0.5 and 1.') 
    }

    #missing
    check.length(missing, 1, ' must be a single symbol.')

    #REPLACING CHARACTER STATES BY "?" IN THE MATRIX IF STATE PROBABILITY < threshold
    for (character in 1:ncol(anc.matrix$prob)) {
        for (taxa in 1:nrow(anc.matrix$prob)) {
            if(anc.matrix$prob[taxa, character] < threshold) {
                anc.matrix$state[taxa, character] <- missing
            }
        }
    }

    #Return
    return(anc.matrix)
}