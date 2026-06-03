## note that changepoint is TIME BEFORE PRESENT (i.e Ma geological time)


## sanitizing function for changepoint + time.window
# check.time <- function(object, class, msg, type, tree) {
#     match_call <- match.call()

#     # class_object <- class(object)[1]
#     # length_class <- length(class)
#     if (type == "changepoint"){

#         if (is(object, "numeric") || is(object, "integer") ) {
#             tree_span <- c(max(node.depth.edgelength(tree)), min(node.depth.edgelength(tree)))
#             if(!object < tree_span[1] && object > tree_span[2]) {
#                 stop.call(match_call$object, " changepoint is either larger or smaller than the tree span...\n")
#             }
#         }
#         return(set.changepoint(object))
#         if (is(object, "character") && !object == "detect"){
#             stop.call(match_call$object, " must be `detect` if character string or a numeric value...\n")
#         }
#     }

#     if (type == "time.window") {
#         if (object == 1){
#             stop.call(match_call$object, " must be either vector of times, a numeric >1 or =<0.5...\n")
#         }
#     }
# }

make.deltatronic.list <- function(changepoint, data){

        changepoint <- as.numeric(changepoint)
        disp_vals <- t(as.data.frame(get.disparity(data), check.names = FALSE))
        # colnames(disp_vals) <- paste0("disparity", seq_len(ncol(disp_vals)))    
        numeric_time <- as.numeric(rownames(disp_vals))
        delta_df <- list(
            time = as.matrix(numeric_time),
            time_elapsed =  as.matrix(max(numeric_time) - numeric_time),
            impact = as.matrix(as.numeric(numeric_time <= changepoint)),
            disparity= as.matrix(disp_vals)
        )

        delta_df$time_post_cp <- as.matrix(ifelse(delta_df$impact == 0, 0,  changepoint - delta_df$time))
        return(delta_df)
}


make.deltatronic <- function(data, changepoint, time.window) {
    match_call <- match.call()
    if (!is(changepoint, "list")){
        changepoint  <- set.changepoint(changepoint)
    }

    if (changepoint == "detect"){
        changepoint <- as.list((names(data$subsets)))
        names(changepoint)  <- names(data$subsets)
        changepoint[[length(changepoint)]] <- NULL
        changepoint[[1]] <- NULL
    } #@@@  check if still works even if changepoint is not an actual datapoint - should do
    if (inherits(data, "dispRity")) {
        delta_df <- lapply(changepoint, make.deltatronic.list, data = data)
    } else if (inherits(data, "list") && inherits(data[[1]], "dispRity")) { ## has already been generated into list
        delta_df <- Map(make.deltatronic.list, changepoint, data) ## for doing the control
    }

    if(!is.null(time.window)){
        delta_df <- lapply(delta_df, set.time.window, time.window)
    }
    return(delta_df)
}

set.time.window <- function(delta_df, time.window) { ## @@@ decide what the minimum number of points can be; start with 2 either side, and what if changepoint is on a datapoint

    match_call <- match.call()
    if(is(time.window, "numeric") && length(time.window) == 1 && time.window > 1) { ## choose n = time.time.window datapoints either side
        data_pre_impact <- lapply(delta_df, function(x) x[delta_df$impact == 0, , drop = FALSE])
        data_post_impact <- lapply(delta_df, function(x) x[delta_df$impact == 1, , drop = FALSE])
        kept_data_pre <- lapply(data_pre_impact, function(x) x[(length(data_pre_impact$time)-(time.window - 1)):length(data_pre_impact$time), , drop = FALSE])
        kept_data_post <- lapply(data_post_impact, function(x) x[1:time.window, , drop = FALSE]) 
        kept_data <- Map(rbind,kept_data_pre,kept_data_post)
    }

    if(is(time.window,"numeric") && length(time.window) == 2) { ## time window around changepoint. note that this time should be going from past to present in Ma style time
        pre_time <- max(time.window)
        post_time  <- min(time.window)
        kept_data_pre <- lapply(delta_df, function(x) x[delta_df$time <= pre_time & delta_df$impact == 0, , drop = FALSE])
        kept_data_post <- lapply(delta_df, function(x) x[delta_df$time >= post_time & delta_df$impact == 1, , drop = FALSE])
        kept_data <- Map(rbind,kept_data_pre,kept_data_post)
    }

    if(is(time.window, "numeric") && length(time.window) ==1 && time.window <= 0.5) { ## calculates percentage of data to keep either side of impact
        data_pre_impact <- lapply(delta_df, function(x) x[delta_df$impact == 0, , drop = FALSE])
        data_post_impact <- lapply(delta_df, function(x) x[delta_df$impact == 1, ,drop = FALSE])
        rows_to_keep_pre <- ceiling((length(data_pre_impact$time) * (time.window*2)))
        rows_to_keep_post <- ceiling(length(data_post_impact$time) * (time.window*2))
        kept_data_pre <- lapply(data_pre_impact, function (x) x[(nrow(x)-(rows_to_keep_pre - 1)):nrow(x), , drop = FALSE])
        kept_data_post <- lapply(data_post_impact, function (x)x[1:rows_to_keep_post, , drop = FALSE])
        kept_data <- Map(rbind,kept_data_pre,kept_data_post)
    }

    if (sum(kept_data$impact == 0) < 2 || sum(kept_data$impact == 1) < 2) {
        stop.call(match_call$time.window, " window is too small. Needs at least 2 datapoints either side of the impact to run the function...\n")
    }

    return(kept_data)
}

set.changepoint  <- function(changepoint){
    if (is(changepoint, "numeric") || is(changepoint, "integer")){
        changepoint <- as.list(changepoint)
        names(changepoint) <- changepoint
        return(changepoint)
    } else if(changepoint == "detect"){
        ## do something here for detect
        return(changepoint)
    }
}


average.method <- function(delta_df, test = stats::t.test, ...) {
    # if (!c("disparity", "impact") %in% names(delta_df)){
    #     stop()
    # }
    t <- test(delta_df$disparity ~ delta_df$impact, ...)
}



itsa.method <- function(delta_df) {

    itsa_dat <- list(
        disparity    = delta_df$disparity, #@@@ might be multi column 
        time_elapsed = as.numeric(delta_df$time_elapsed),
        impact       = as.numeric(delta_df$impact),
        time_post_cp = as.numeric(delta_df$time_post_cp)
    )

    model <- lm(disparity ~ time_elapsed + impact + time_post_cp, data = itsa_dat)

    counterfactual_df <- data.frame(
                time_elapsed = itsa_dat$time_elapsed,
                impact       = 0,
                time_post_cp = 0
        )
    
    counterfactual_predicts <- predict(model, newdata = counterfactual_df, interval = "confidence")

    delta_df$counter_mean_ci  <- as.matrix(counterfactual_predicts[, "fit"])
    delta_df$counter_lower_ci <- as.matrix(counterfactual_predicts[, "lwr"])
    delta_df$counter_upper_ci <- as.matrix(counterfactual_predicts[, "upr"])

    
    return(list(
        data = delta_df,
        model = model
    ))
}


area.method <- function(itsa, time) {
    
    data <- itsa$data
    
    post_intervention_dat <- lapply(data, function(x)  x[data$impact == 1, ,drop =FALSE])

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


make.control <- function(changepoint, data, nsim = 1000, paint = TRUE, slice.model = NULL, ...) {
    metric <- get.metric.from.call(data, 2) ## store for later
    slices <- as.numeric(names(data$subsets)) # store for later
    slice_call <- data$call$subsets
    tree <- get.tree(data)
    mat <- get.matrix(data)

    painted_tree <- paint.branches(tree, changepoint)

    sim_parameters <- replicate(ncol(mat), list(root_value = NULL, sig_sq = NULL), simplify = FALSE) ## create empty list structure for storing trait parameters

    if (paint) {
        n <- length(tree$tip.label)
        tip_mat <- mat[tree$tip.label, , drop = FALSE]
        painted_tree <- paint.branches(tree, changepoint)

        for(i in 1:ncol(mat)) {
            fit_bm <- mvMORPH::mvBM(tree = painted_tree, data = tip_mat[, i], model = "BMM", echo = FALSE, diagnostic = FALSE) ## taken out error, can put back in
            sim_parameters[[i]]$sig_sq <- fit_bm$sigma[, , "Pre_Intervention"] * n / (n - 1)   ## REML correction, see Revell http://www.phytools.org/***SanJuan2016/ex/5/Fitting-BM.html.
            sim_parameters[[i]]$root_value <- as.numeric(fit_bm$theta)
        }

    } else { ## use tree slicing

        if (slice.model == "proximity" || slice.model == "acctran" || slice.model == "deltran") {
            pre_tree <- slice.tree(tree, age = changepoint, model = slice.model, keep.all.ancestors = TRUE)
            pre_mat <- mat[pre_tree$tip.label, ] ## can just prune the matrix as it is if using punctuated model
        }

        if (slice.model == "gradual.split" || slice.model == "equal.split") {
            pre_tree <- slice.tree(tree, age = changepoint, model = "acctran", keep.all.ancestors = TRUE)
            slice_vals <- slice.tree(tree, age = changepoint, model = slice.model, keep.all.ancestors = TRUE)
            pre_mat <- matrix(NA, nrow = length(pre_tree$tip.label), ncol = ncol(mat))
            rownames(pre_mat) <- as.character(slice_vals[, 2])
            for (i in seq_along(pre_tree$tip.label)) {
                if(slice_vals[i, 3] == "0"){
                    pre_mat[i, ] <- mat[slice_vals[i, 2], ]
                } else {
                    t0 <- mat[slice_vals[i, 1], ]
                    t1 <- mat[slice_vals[i, 2], ]
                    slice_position <- as.numeric(slice_vals[i, 3]) ## get slice position across branch
                    pre_mat[i, ] <- (slice_position * t0) + ((1-slice_position) * t1) ## weighted average
                }
            }
        }
        n <- length(pre_tree$tip.label)
        for(i in 1:ncol(pre_mat)) {
            fit_bm <- mvMORPH::mvBM(tree = pre_tree, data = pre_mat[, i], model = "BM1", echo = FALSE, diagnostic = FALSE) ## taken out error, can put back in
            n <- length(pre_tree$tip.label)
            sim_parameters[[i]]$sig_sq <- fit_bm$sigma * n / (n - 1)  ## REML correction, see Revell http://www.phytools.org/***SanJuan2016/ex/5/Fitting-BM.html.
            sim_parameters[[i]]$root_value <- as.numeric(fit_bm$theta)
        }
    }

    ## TODO use slice.tree and gradual.split. use get.tree for the tree in each subset.

    control_traits <- lapply(sim_parameters, function(axis) {
        root_value <- axis$root_value
        sig_sq <- axis$sig_sq
        treats::make.traits(process = treats::BM.process, start = root_value, n = 1, process.args = list(Sigma = sig_sq))
    })
    mapped_control <- replicate(nsim, {do.call(cbind, lapply(lapply(control_traits, treats::map.traits, tree = tree), function(x){x$data}))}, simplify = FALSE) ## produces however many differnt BM simulations as controls


    chrono <- chrono.subsets(mapped_control, tree, method = slice_call[1], model = slice_call[2], bind.data = as.logical(slice_call["bind"]), inc.nodes = TRUE, time = slices)


    metric.fun <- function(mat){
        metric[[1]](metric[[2]](mat))
    }
    disp <- dispRity(chrono, metric = metric.fun)
    disp$call$disparity$metrics <- data$call$disparity$metrics

    return(disp)
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

