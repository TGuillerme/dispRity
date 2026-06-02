## note that changepoint is TIME BEFORE PRESENT (i.e Ma geological time)


## sanitizing function for changepoint + time.window
check.time <- function(object, class, msg, type, tree) {
    match_call <- match.call()

    # class_object <- class(object)[1]
    # length_class <- length(class)
    if (type == "changepoint"){
        check.class(object, c("numeric", "integer", "character"))
        check.length(object, length = 1, paste(" argument must be a single numeric/integer or character string: `detect`.\n"))
        if (class(object) == "numeric" || class(object) == "integer") {
            tree_span <- c(max(node.depth.edgelength(tree)), min(node.depth.edgelength(tree)))
            if(!object < tree_span[1] && object > tree_span[2]) {
                stop()
            }
        }
    }

    if (type == "time.window") {
        if (object == 1){
            stop(match_call$object, "must be larger than 1...\n")
        }
    }
}


make.deltatronic <- function(data, changepoint, concatenate = TRUE) {
    disp_vals <- t(as.data.frame(get.disparity(data, concatenate = as.logical(concatenate)), check.names = FALSE))
    colnames(disp_vals) <- "disparity"
    numeric_time <- as.numeric(rownames(disp_vals))

    delta_df <- data.frame(
        time = numeric_time,
        time_elapsed =  max(numeric_time) - numeric_time,
        disparity = as.numeric(disp_vals[,"disparity"]),
        impact = as.numeric(numeric_time <= changepoint)
    )

    delta_df$time_post_cp <- ifelse(delta_df$impact == 0, 0, delta_df$time_elapsed - changepoint)

    return(delta_df)
}

set.time.window <- function(delta_df, tree, time.window, changepoint) {
    if(class(time.window) ==  "numeric" && length(time.window) == 1 && time.window > 1) { ## choose n = time.time.window datapoints either side
        data_pre_impact <- delta_df[delta_df$impact == 0,]
        data_post_impact <- delta_df[delta_df$impact == 1,]
        kept_data_pre <- data_pre_impact[(nrow(data_pre_impact)-(time.window - 1)):nrow(data_pre_impact), ]
        kept_data_post <- data_post_impact[1:time.window, ] 
    }

    if(class(time.window) == "numeric" && length(time.window) == 2) { ## time window around changepoint. note that this time should be going from past to present in Ma style time
        pre_time <- max(time.window)
        post_time  <- min(time.window)

        


    }   



}














itsa.dispRity <- function(disparity_object, changepoint, time){
    if (!inherits(disparity_object, "dispRity")) {
        stop("Disparity object must be of class `dispRity`...\n")
    }

    run.itsa <- function(disparity_object, changepoint, time) {
        disp_list <- get.disparity(disparity_object)
        disp_vals <- data.frame(
            time       = as.numeric(names(disp_list)),
            disparity  = unlist(disp_list)
        )

        disp_vals <- disp_vals[disp_vals$time %in% time, ]

        changepoint_index <- which(disp_vals$time == changepoint)

        disp_vals$time_elapsed <- max(disp_vals$time) - disp_vals$time

        disp_vals$dummy <- c(rep(0, changepoint_index - 1), 
                        rep(1, nrow(disp_vals) - changepoint_index + 1))

        disp_vals$time_post_changepoint <- c(
        rep(0, changepoint_index - 1),
        changepoint - disp_vals$time[changepoint_index:nrow(disp_vals)]
        )

        model <- lm(disparity ~ time_elapsed + dummy + time_post_changepoint, data = disp_vals)

        counterfactual_df <- data.frame(
            time_elapsed = disp_vals$time_elapsed,
            dummy = 0,
            time_post_changepoint = 0
        )

        counterfactual_predicts <- predict(model, newdata = counterfactual_df, interval = "confidence")

        disp_vals$counter_mean_ci  <- counterfactual_predicts[, "fit"]
        disp_vals$counter_lower_ci <- counterfactual_predicts[, "lwr"]
        disp_vals$counter_upper_ci <- counterfactual_predicts[, "upr"]

        return(list(data = disp_vals, model = model))
    }


    if(length(changepoint) > 1) {
        output <- list()
        for (i in seq_along(changepoint)){
            output[[i]] <- run.itsa(disparity_object, changepoint[[i]], time)
        }
    } else {
        output <- run.itsa(disparity_object, changepoint, time)
    }
    return(output)
}



area.disparity <- function(disparity_object, changepoint, time) {
    
    itsa <- itsa.disparity(disparity_object, changepoint, time)

    data <- itsa$data
    changepoint_index <- which.min(abs(data$time - changepoint)) ## uses which.min to find closest value to changepoint

    post_intervention_dat <- data[changepoint_index:nrow(data),]

    ## extracts amount of disparity that falls outside of CI envelope, otherwise put as 0
    exceed_upper <- pmax(post_intervention_dat$disparity - post_intervention_dat$counter_upper_ci, 0) 
    exceed_lower <- pmax(post_intervention_dat$counter_lower_ci - post_intervention_dat$disparity, 0)

    time_intervals <- diff(post_intervention_dat$time_elapsed)

    area_above <- sum(((exceed_upper[-length(exceed_upper)] + exceed_upper[-1]) / 2) * time_intervals, na.rm = TRUE)
    area_below <- sum(((exceed_lower[-length(exceed_lower)] + exceed_lower[-1]) / 2) * time_intervals, na.rm = TRUE)

    return(c(area_above = area_above, area_below = area_below))
}



paint.branches <- function(tree, changepoint) {

    if(is.null(tree$root.time)) {
        root_age <- (set.root.time(tree))$root.time
    } else {
        root_age <- tree$root.time
    }
    
    node_ages <- root_age - ape::node.depth.edgelength(tree)

    N_edges <- nrow(tree$edge)
    mapped_edge <- matrix(0, nrow = N_edges, ncol = 2)
    colnames(mapped_edge) <- c("Pre_Intervention", "Post_Intervention")
    rownames(mapped_edge) <- paste(tree$edge[,1], tree$edge[,2], sep=",")
    
    maps <- list()
    
    for(i in 1:N_edges) {
        parent_node <- tree$edge[i, 1]
        child_node  <- tree$edge[i, 2]
        
        age_parent <- node_ages[parent_node]
        age_child  <- node_ages[child_node]
        
        if(age_child >= changepoint) {
            mapped_edge[i, "Pre_Intervention"] <- tree$edge.length[i]
            maps[[i]] <- c("Pre_Intervention" = tree$edge.length[i])
        }
        else if(age_parent <= changepoint) {
            mapped_edge[i, "Post_Intervention"] <- tree$edge.length[i]
            maps[[i]] <- c("Post_Intervention" = tree$edge.length[i])
        }
        else {
            pre_time <- age_parent - changepoint
            post_time <- changepoint - age_child
            
            mapped_edge[i, "Pre_Intervention"] <- pre_time
            mapped_edge[i, "Post_Intervention"] <- post_time
            
            maps[[i]] <- c("Pre_Intervention" = pre_time,
                           "Post_Intervention" = post_time)
        }
    }
    
    tree$mapped.edge <- mapped_edge
    tree$maps <- maps
    class(tree) <- c("simmap", "phylo")
    
    return(tree)
}


make.control.painted <- function(disparity_object, changepoint, replicates,...) {

    data <- get.matrix(disparity_object)
    tree <- get.tree(disparity_object)

    painted_tree <- paint.branches(tree, changepoint)
    if(!inherits(painted_tree, "simmap")){
        stop("Tree did not paint...\n")
    }
    
    sim_parameters <- replicate(ncol(data), list(root_value = NULL, sig_sq = NULL), simplify = FALSE) ## create empty list structure for storing trait parameters

    # branch_length_ratio <- sum(tree$edge.length) / sum(pre_tree$edge.length)

    pre_tree <- slice.tree(tree, age = tree$root.time - changepoint, model = "acctran", keep.all.ancestors = TRUE)
    n <- length(pre_tree$tip.label)
    tip_mat <- data[tree$tip.label, , drop = FALSE]
    for(i in 1:ncol(data)) {
        fit_bm <- mvMORPH::mvBM(tree = painted_tree, data = tip_mat[, i], model = "BMM", echo = FALSE, diagnostic = FALSE) ## taken out error, can put back in
        sim_parameters[[i]]$sig_sq <- fit_bm$sigma[, , "Pre_Intervention"] * n / (n - 1)   ## REML correction, see Revell http://www.phytools.org/***SanJuan2016/ex/5/Fitting-BM.html.
        sim_parameters[[i]]$root_value <- as.numeric(fit_bm$theta)
    }
    ## TODO use slice.tree and gradual.split. use get.tree for the tree in each subset.

    control_traits <- lapply(sim_parameters, function(axis) {
        root_value <- axis$root_value
        sig_sq <- axis$sig_sq
        treats::make.traits(process = BM.process, start = root_value, n = 1, process.args = list(Sigma = sig_sq))
    })
    mapped_control <- replicate(replicates, {do.call(cbind, lapply(lapply(control_traits, treats::map.traits, tree = tree), function(x){x$data}))}, simplify = FALSE) ## produces however many differnt BM simulations as controls


    # chrono_output <- chrono.subsets(mapped_control, tree = tree, method = "c", model = slice.model, time = subsets) ## TODO recycle it from original call. look at get.time.slice.

    return(mapped_control)
}




ols.deltatronic.itsa <- function(empirical, control, changepoint, times = NULL, alpha = 0.05, normalise = TRUE) { 


    run.model <- function(real, cont,cp, cp_index){

        if (!nrow(real) == nrow(cont)){
            stop("Empirical and control disparity dataframes are not same size...\n")
        }

        disp_df <- data.frame(
            times = rep(as.numeric(rownames(real)), 2),
            disparity = c(real[, "disparity"], cont[, "disparity"]),
            real_vs_control = c(rep(1, nrow(real)), rep(0, nrow(cont))),
            time_elapsed = rep(max(as.numeric(rownames(real))) - as.numeric(rownames(real)), 2),
            intervention = rep(c(rep(0, cp_index - 1), 
                    rep(1, nrow(real) - cp_index + 1)), 2)
        )


        disp_df$time_post_changepoint <- ifelse(
        disp_df$intervention == 0,
        0, 
        cp - disp_df$times
        ) ## add times post changepoint column

        # disp_df$time_reversed <- max(disp_df$times) - disp_df$times ## reverse times so gradients make sense

        model <- tryCatch({
                lm(
                disparity ~ time_elapsed + intervention + real_vs_control + time_post_changepoint +
                            time_elapsed:real_vs_control + 
                            intervention:real_vs_control + 
                            time_post_changepoint:real_vs_control,
                data = disp_df,
                # correlation = corAR1(form = ~ time_index | real_vs_control), ## checked and this causes more type i/ii errors
                # weights = varIdent(form = ~ 1 | real_vs_control),
                # method = "REML",
                na.action = na.omit
            )}, error = function(e){
                warning(paste0("Model failed to converge:", e$message))
                return(NULL)
        })
        
        return(model$coefficients)

        # immediate_jump <- lapply(model_results, function(model){
        #     as.numeric(model$model$coefficients["intervention:real_vs_control"])
        # })

        # change_in_slope <- lapply(model_results, function(model_obj) {
        #     coef_mat <- model_obj$model$coefficients
        #     as.numeric(coef_mat["real_vs_control:time_post_changepoint", "Estimate"])
        # })
        # coefficients_dist <- unlist(change_in_slope)
    }

    ## sanitizing
    if (!inherits(control,"dispRity")) {
        stop("control must be class `dispRity`...\n")
    }

    # if (!inherits(empirical, "dispRity")) {
    #     stop("empirical must be class 'dispRity'\n")
    # }

    if (!is.null(times)){
        if(!inherits(changepoint, "numeric") || !(changepoint < max(times) &&  changepoint > min(times))) {
            stop("changepoint must be a single numeric value\n")
        }
    }

    if (!inherits(empirical, "dispRity")){
        stop("empirical must be class `dispRity`...\n")
    }

    if (as.numeric(empirical$call$subsets["matrices"][[1]]) > 1) {
        got_disparity <- t(as.data.frame(get.disparity(empirical), check.names = FALSE))
        real_values <- list()
        for(i in 1:ncol(got_disparity)){
            real_values[[i]] <- as.data.frame(got_disparity[ ,i])
            colnames(real_values[[i]]) <- "disparity"
        }
        if (!length(unique(sapply(real_values, nrow))) == 1){
            stop("Length of empirical tables do not match..\n")
        } else {
        real_changepoint_index <- which.min(abs(as.numeric(rownames(real_values[[1]])) - changepoint))
        } ## uses which.min to find closest value to changepoint


    } else {
        real_values <- list(as.matrix(unlist(get.disparity(empirical))))
        if(!is.null(times)) {
            real_values <- real_values[rownames(real_values) %in% times, , drop = FALSE]
        }
        real_values <- lapply(real_values, function(x) {
            colnames(x) <- "disparity"
            return(x)
        })
        real_changepoint_index <- which.min(abs(as.numeric(rownames(real_values[[1]])) - changepoint)) ## uses which.min to find closest value to changepoint
    }

    if (as.numeric(control$call$subsets["matrices"][[1]]) > 1) {
        got_disparity <- t(as.data.frame(get.disparity(control), check.names = FALSE))
        ctrl_values <- list()
        for(i in 1:ncol(got_disparity)){
            ctrl_values[[i]] <- as.data.frame(got_disparity[ ,i])
            colnames(ctrl_values[[i]]) <- "disparity"
        }
        if (!length(unique(sapply(ctrl_values, nrow))) == 1){
            stop("Length of empirical tables do not match..\n")
        } else {
        real_changepoint_index <- which.min(abs(as.numeric(rownames(ctrl_values[[1]])) - changepoint))
        } ## uses which.min to find closest value to changepoint
    }

    if(isTRUE(normalise)) {
        if (length(real_values) > 1 ){ ## anchor so that final disparity value pre-intervention matches with empirical.
            for (i in 1:length(ctrl_values)){
            emp_int_val <- as.numeric(real_values[[i]][(real_changepoint_index-1), ])
            ctrl_int_val <- as.numeric(ctrl_values[[i]][(real_changepoint_index-1), ])
            ctrl_values[[i]] <- ctrl_values[[i]] * (emp_int_val / ctrl_int_val)
            }
        } else {
            for (i in 1:length(ctrl_values)){
            emp_int_val <- as.numeric(real_values[[1]][(real_changepoint_index-1), ])
            ctrl_int_val <- as.numeric(ctrl_values[[i]][(real_changepoint_index-1), ])
            ctrl_values[[i]] <- ctrl_values[[i]] * (emp_int_val / ctrl_int_val)
            }
        }
    }

    # if (isTRUE())
    # control_list <- lapply(seq_along(control), function(i) {
    #     if (inherits(control[[i]], "dispRity")) {
    #         unlisted <- as.data.frame(unlist(get.disparity(control[[i]])))
    #     } else if (inherits(control[[i]], "list")) { ## if `get.dispRity` has already been applied to control
    #         unlisted <- as.data.frame(unlist(control[[i]]))
    #         colnames(unlisted) <- "disparity"

    #     }
    #     if(!is.null(times)) {
    #         unlisted <- unlisted[rownames(unlisted) %in% times, , drop = FALSE]
    #     }
    #     return(unlisted)
    # })

    # # Combine all controls into one data frame
    # control_df <- do.call(rbind, control_list)

    # # Get changepoint from first control (assuming same structure)
    # control_changepoint_index <- which(rownames(control_list[[1]]) == changepoint)

    # if (isTRUE(normalise)){
    #     real_values <- lapply(real_values, function(x) x$disparity <- (x$disparity - min(x$disparity)) / (max(x$disparity) - min(x$disparity)))
    #     ctrl_values <- lapply(ctrl_values, function(x) x$disparity <- (x$disparity - min(x$disparity)) / (max(x$disparity) - min(x$disparity)))
    # }


    if(inherits(real_values, "list") && length(real_values) > 1) {

    slope_dist <- Map(
        function(real, cont) {
            run.model(real, cont, changepoint, real_changepoint_index)
        },
        sample(real_values, replace = FALSE), # randomly pairs the real and control curves
        sample(ctrl_values, replace = FALSE)
    )
    } else if (inherits(real_values, "list") && length(real_values) == 1) {
        slope_dist <- lapply(ctrl_values, function(cont){
            run.model(real_values[[1]], cont, changepoint, real_changepoint_index)
        })
    }



    did_change_in_slope <- lapply(slope_dist, function(x) x["real_vs_control:time_post_changepoint"])
    ci_95  <-  quantile(unlist(did_change_in_slope), c(0.025, 0.975))

    # did_level_change <- lapply(slope_dist, function(x) x["intervention:real_vs_control"])
    # ctrl_change_in_slope <- lapply(slope_dist, function(x) x["time_post_changepoint"])
    # # emp_change_in_slope <- lapply(slope_dist, function(x) x["real_vs_control:time_post_changepoint"] +  x["time_post_changepoint"])
    # emp_pre_slope <- lapply(slope_dist, function(x) x["time_elapsed"] + x["time_elapsed:real_vs_control"])
    # es <- compute.citsa.effect(unlist(did_change_in_slope), unlist(ctrl_change_in_slope))
    # lapply(time_post_changepoint:real_vs_control
    return(ci_95)
}

