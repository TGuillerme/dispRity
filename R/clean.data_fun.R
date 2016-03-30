#Cleaning a tree so that the species match with the ones in a table
clean.tree.table <- function(tree, data) {

    #create a dummy data
    dummy_data <- as.data.frame(data)

    #Adding a row called taxa_names_column containing the taxa names
    dummy_data[,ncol(data)+1] <- rownames(data)
    names(dummy_data)[ncol(data)+1] <- "taxa_names_column"

    #run caper::comparative.data to check the non matching columns/rows
    missing <- caper::comparative.data(tree, dummy_data, "taxa_names_column", vcv = FALSE, vcv.dim = 2, na.omit = TRUE, force.root = FALSE, warn.dropped = FALSE, scope = NULL)$dropped

    #Dropping tips (if necessary)
    if(length(missing$tips) != 0) {
        #drop the missing tips
        tree_tmp <- drop.tip(tree, missing$tips)
        #save the missing tips names
        dropped_tips <- missing$tips
    } else {
        #No drop needed!
        tree_tmp <- tree
        dropped_tips <- NA
    }

    #Dropping rows (if necessary)
    if(length(missing$unmatched.rows) != 0) {
        #Drop the unmatched rows
        data_tmp <- data[-match(missing$unmatched.rows, dummy_data[,ncol(data)+1]),]
        #save the dropped rows names
        dropped_rows<-missing$unmatched.rows
    } else {
        #No drop needed!
        data_tmp<-data
        dropped_rows<-NA
    }

    return(list("tree"=tree_tmp, "data"=data_tmp, "dropped_tips"=dropped_tips, "dropped_rows"=dropped_rows))
}