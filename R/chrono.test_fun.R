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

make.deltatronic.list <- function(changepoint, data, dimension.level, is.multi.matrix) {


    disp_vals <- get.disparity(data, concatenate = FALSE)
    disp_vals_list <- delta_df <- list()

    if (is.multi.matrix > 1 && dimension.level == 1) {

        # Filling this placeholder so that we have the results in a matrix        
        temp_list <-do.call(rbind, get.disparity(data, concatenate = FALSE))
        disp_vals_list <- apply(temp_list, 2, function(x) matrix(x, ncol = 1, dimnames = list(rownames(temp_list))), simplify = FALSE)



        ## Initiating the deltratonic list

        for (i in 1:is.multi.matrix){
            numeric_time <- as.numeric(rownames(disp_vals_list[[i]]))
            delta_df[[i]] <- list(
                time = as.matrix(numeric_time),
                time_elapsed =  as.matrix(max(numeric_time) - numeric_time),
                impact = as.matrix(as.numeric(numeric_time <= changepoint)),
                disparity= as.matrix(disp_vals_list[[i]])
            )
            delta_df[[i]]$time_post_cp <- as.matrix(ifelse(delta_df[[i]]$impact == 0, 0,  changepoint - delta_df[[i]]$time))
        }

    } else if (is.multi.matrix > 1 && dimension.level > 1){
        
        ## Initi
            for (i in 1:is.multi.matrix) {
                disp_vals_list[[i]] <- lapply(disp_vals, function(x) x[,i]) ## extract each column (i.e matrix replicate)
                disp_vals_list[[i]] <- t(as.data.frame(disp_vals_list[[i]],, check.names = FALSE)) ## transpose to data.frame
            }
        
        delta_df <- list()
        for (i in 1:is.multi.matrix){
            numeric_time <- as.numeric(rownames(disp_vals_list[[i]]))
            delta_df[[i]] <- list(
            time = as.matrix(numeric_time),
            time_elapsed =  as.matrix(max(numeric_time) - numeric_time),
            impact = as.matrix(as.numeric(numeric_time <= changepoint)),
            disparity= as.matrix(disp_vals_list[[i]])
            )
            delta_df[[i]]$time_post_cp <- as.matrix(ifelse(delta_df[[i]]$impact == 0, 0,  changepoint - delta_df[[i]]$time))
        }
    
    } else { ## here dimension.level can be >1 but function still works.
        changepoint <- as.numeric(changepoint)
        disp_vals <- t(as.data.frame(get.disparity(data, concatenate = FALSE), check.names = FALSE))
        # colnames(disp_vals) <- paste0("disparity", seq_len(ncol(disp_vals)))    
        numeric_time <- as.numeric(rownames(disp_vals))
        delta_df <- list(
            time = as.matrix(numeric_time),
            time_elapsed =  as.matrix(max(numeric_time) - numeric_time),
            impact = as.matrix(as.numeric(numeric_time <= changepoint)),
            disparity= as.matrix(disp_vals)
        )

        delta_df$time_post_cp <- as.matrix(ifelse(delta_df$impact == 0, 0,  changepoint - delta_df$time))
    }
        return(delta_df)
}


make.deltatronic <- function(data, changepoint, time.window, dimension.level, is.multi.matrix) {

    match_call <- match.call()

    # if (is.multi.matrix) {
    #     multi_dis <- replicate(length(data$matrix), list(data))
    # }

    if (identical(changepoint,"detect")) {
        changepoint <- as.list((names(data$subsets)))
        names(changepoint)  <- names(data$subsets)
        changepoint[[length(changepoint)]] <- NULL
        changepoint[[1]] <- NULL
    }  #@@@  check if still works even if changepoint is not an actual datapoint - should do


    if (!is(changepoint, "list") && is.numeric(changepoint)){
        changepoint  <- set.changepoint(changepoint)
    }

    if (inherits(data, "dispRity")) {
        delta_df <- lapply(changepoint, make.deltatronic.list, data = data, dimension.level, is.multi.matrix)
    } else if (inherits(data, "list") && inherits(data[[1]], "dispRity")) {
        # data <- data$disparity ## has already been generated into list
        delta_df <- Map(make.deltatronic.list, changepoint, data) ## for doing the control
    }

    if(!is.null(time.window)){
        if (is.multi.matrix >1){
            delta_df <- lapply(delta_df, lapply, set.time.window, time.window)
        } else {
        delta_df <- lapply(delta_df, set.time.window, time.window)
        }
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


average.method <- function(delta_df, test = stats::t.test, dimension.level,  ...) {
    # if (!c("disparity", "impact") %in% names(delta_df)){
    #     stop()
    # }

    if(dimension.level == 1){
        t <- test(delta_df$disparity ~ delta_df$impact, ...)
    } else if(dimension.level > 1 ){
        ## need a multivariate test, eg permanova? or t.test across each axis
        # do permanova here
        t <- list()
        for (i in 1:dimension.level){
            t[[i]] <- test(delta_df$disparity[, i] ~ delta_df$impact)
        }
    }
    return(t)
}






itsa.method <- function(delta_df,  dimension.level, ...) {

    # if(itsa.type) ##@@@ allow gls to be used with autocorrelation

    # if (dimension.level > 1){ 
        
    # }

    itsa_dat <- list(
        disparity    = delta_df$disparity, #@@@ might be multi column
        time_elapsed = as.numeric(delta_df$time_elapsed),
        impact       = as.numeric(delta_df$impact),
        time_post_cp = as.numeric(delta_df$time_post_cp)
    )

    if (dimension.level > 1){  ## if a multidimensional dispRity.metric for loop over each axis
        model <- list()
        delta_df$counter_mean_ci <- matrix(NA, nrow = nrow(itsa_dat$disparity), ncol = ncol(itsa_dat$disparity))
        delta_df$counter_lower_ci <- matrix(NA, nrow = nrow(itsa_dat$disparity), ncol = ncol(itsa_dat$disparity))
        delta_df$counter_upper_ci <- matrix(NA, nrow = nrow(itsa_dat$disparity), ncol = ncol(itsa_dat$disparity))

        counterfactual_df <- data.frame(
                        time_elapsed = itsa_dat$time_elapsed,
                        impact       = 0,
                        time_post_cp = 0
            )

        for (i in 1:dimension.level){
            model[[i]] <- lm(disparity[, i] ~ time_elapsed + impact + time_post_cp, data = itsa_dat)
            counterfactual_predicts <- predict(model[[i]], newdata = counterfactual_df, interval = "confidence")
            delta_df$counter_mean_ci[, i] <- as.matrix(counterfactual_predicts[, "fit"])
            delta_df$counter_lower_ci[,i] <- as.matrix(counterfactual_predicts[, "lwr"])
            delta_df$counter_upper_ci[,i] <- as.matrix(counterfactual_predicts[, "upr"])
        }
    } else {

        model <- lm(disparity ~ time_elapsed + impact + time_post_cp, data = itsa_dat, ...)

        counterfactual_df <- data.frame(
                    time_elapsed = itsa_dat$time_elapsed,
                    impact       = 0,
                    time_post_cp = 0
        )
        
        counterfactual_predicts <- predict(model, newdata = counterfactual_df, interval = "confidence")

        delta_df$counter_mean_ci  <- as.matrix(counterfactual_predicts[, "fit"])
        delta_df$counter_lower_ci <- as.matrix(counterfactual_predicts[, "lwr"])
        delta_df$counter_upper_ci <- as.matrix(counterfactual_predicts[, "upr"])

    }

    return(list(
        data = delta_df,
        model = model
    ))
}
calculate.angular.effect <- function(itsa) {

    model <- itsa$model
    delta_df <- itsa$data

    m1 <- coef(model)["time_elapsed"] ## basline slope
    m_diff <- coef(model)["time_post_cp"] ## change in slope

    m2 <- m1 + m_diff ## post-impact slope

    sd_time <- sd(delta_df$time_elapsed, na.rm = TRUE) ## stdev of time
    sd_disp <- sd(delta_df$disparity, na.rm = TRUE) ## stdev of disparity across **whole curve** (think that is right)

    if (is.na(sd_disp) || sd_disp == 0 || is.na(sd_time) || sd_time == 0) {
    return(NA)
    }

    beta1 <- m1 * (sd_time / sd_disp) ## standardise by stdev of time and stdev of disparity
    beta2 <- m2 * (sd_time / sd_disp)


    ## note that using atan() is non-linear, therefore it is harder to get a large effect size if the baseline angle is already steep, than if the baseline was narrow.
    theta1 <- atan(beta1) * (180 / pi) ## convert to geometric angles from radians
    theta2 <- atan(beta2) * (180 / pi) ## same here
    angular_effect_size <- (theta2 - theta1) / 180

    return(list(
    baseline_angle_deg = theta1,
    post_impact__angle_deg   = theta2,
    angle_delta_deg    = theta2 - theta1,
    effect_size   = angular_effect_size
    ))
}

calculate.slope.effect <- function(itsa) {

    model <- itsa$model
    delta_df <- itsa$data

    # m1 <- coef(model)["time_elapsed"] ## basline slope
    # m_diff <- coef(model)["time_post_cp"] ## change in slope

    # m2 <- m1 + m_diff ## post-impact slope

    # sd_time <- sd(delta_df$time_elapsed, na.rm = TRUE) ## stdev of time
    # sd_disp <- sd(delta_df$disparity, na.rm = TRUE) ## stdev of disparity across **whole curve** (think that is right)

    # if (is.na(sd_disp) || sd_disp == 0 || is.na(sd_time) || sd_time == 0) {
    # return(NA)
    # }

    # standardised_delta <- m_diff * (sd_time / sd_disp)

    co <- summary(model)$coefficients

    tval <- co["time_post_cp", "t value"]
    df <- model$df.residual

    effect_size <- tval^2 / (tval^2 + df)

    # effect_size_0_1 <- tanh(abs(standardised_delta))

    return(list(
    # baseline_angle_deg = theta1,
    # post_impact__angle_deg   = theta2,
    # angle_delta_deg    = theta2 - theta1,
    effect_size   = effect_size
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
    colnames(mapped_edge) <- c("pre_impact", "post_impact")
    rownames(mapped_edge) <- paste(tree$edge[,1], tree$edge[,2], sep=",")
    
    maps <- list()
    
    for(i in 1:N_edges) {
        parent_node <- tree$edge[i, 1]
        child_node  <- tree$edge[i, 2]
        
        age_parent <- node_ages[parent_node]
        age_child  <- node_ages[child_node]
        
        if(age_child >= changepoint) {
            mapped_edge[i, "pre_impact"] <- tree$edge.length[i]
            maps[[i]] <- c("pre_impact" = tree$edge.length[i])
        }
        else if(age_parent <= changepoint) {
            mapped_edge[i, "post_impact"] <- tree$edge.length[i]
            maps[[i]] <- c("post_impact" = tree$edge.length[i])
        }
        else {
            pre_time <- age_parent - changepoint
            post_time <- changepoint - age_child
            
            mapped_edge[i, "pre_impact"] <- pre_time
            mapped_edge[i, "post_impact"] <- post_time
            
            maps[[i]] <- c("pre_impact" = pre_time,
                           "post_impact" = post_time)
        }
    }
    
    tree$mapped.edge <- mapped_edge
    tree$maps <- maps
    class(tree) <- c("simmap", "phylo")
    
    return(tree)
}


make.control <- function(changepoint, data, nsim = 100, paint = TRUE, slice.model = NULL, ...) { ## change slice.model name for clarity

    if (paint) {
        slice.model  <- NULL
    }

    if (!paint && is.null(slice.model)){
        stop("`slice.model` argument needs to be inputted if paint = FALSE...\n")
    }

    if (!paint && !is.null(slice.model)) {
        all_models <- c("acctran", "deltran", "random", "proximity", "equal.split", "gradual.split")
        check.length(slice.model, 1, paste(" argument must be one of the following: ", paste(all_models, collapse = ", "), ".", sep = ""))
        check.method(slice.model, all_models, "slice.model argument")
    }

    metric <- get.metric.from.call(data, 2) ## store for later
    slices <- as.numeric(names(data$subsets)) # store for later
    slice_call <- data$call$subsets
    tree <- get.tree(data)
    mat <- get.matrix(data)


    sim_parameters <- replicate(ncol(mat), list(root_value = NULL, sig_sq = NULL), simplify = FALSE) ## create empty list structure for storing trait parameters

    if (paint) {

        painted_tree <- paint.branches(tree, changepoint)
        n <- length(tree$tip.label)
        tip_mat <- mat[tree$tip.label, , drop = FALSE]
        painted_tree <- paint.branches(tree, changepoint)

        for(i in 1:ncol(mat)) {
            fit_bm <- mvMORPH::mvBM(tree = painted_tree, data = tip_mat[, i], model = "BMM", echo = FALSE, diagnostic = FALSE)
            sim_parameters[[i]]$sig_sq <- fit_bm$sigma[, , "pre_impact"] * n / (n - 1)   ## REML correction, see Revell http://www.phytools.org/***SanJuan2016/ex/5/Fitting-BM.html.
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




    # metric.fun <- function(mat){
    #     metric[[1]](metric[[2]](mat)) ## add if in case there is 1 or 3 metric functions applied
    # } ##@@@ this needs to get fixed, works for now but thomas will fix it.

    
    disp <- dispRity(chrono, metric = metric)
    # disp$disparity <- t(disp$disparity)
    disp$call$disparity$metrics <- data$call$disparity$metrics ## reattach metric fun info

    return(disp)
}


bind.delta <- function(delta_df, control_delta_df, dimension.level) {
    if (multi) {

    }

    delta_df <- lapply(delta_df, as.numeric)
    delta_df <- do.call(cbind, delta_df)
    control_df <- lapply(control_delta_df, as.numeric)
    control_df <- do.call(cbind, control_df)
    full_df <- as.data.frame(rbind(control_df, delta_df))


}


###@@@ see thomas photo on how to relativise, using triangle. the coefficients are extracted, the maximum change is 1 which is a straight line upwards, everything else is a proportion of that change in angle.


citsa.method <- function(full_df){

        # if(dimension.level > 1) {

        # }
        # delta_df <- lapply(delta_df, as.numeric)
        # delta_df <- do.call(cbind, delta_df)

        # control_df <- lapply(control_delta_df, as.numeric)
        # control_df <- do.call(cbind, control_df)

        # full_df <- as.data.frame(rbind(control_df, delta_df))

    
        model <- tryCatch({
                lm(
                disparity ~ time_elapsed + impact + emp_vs_null + time_post_cp +
                            time_elapsed:emp_vs_null + 
                            impact:emp_vs_null + 
                            time_post_cp:emp_vs_null,
                data = full_df,
                # correlation = corAR1(form = ~ time_index | real_vs_control), ## checked and this causes more type i/ii errors
                # weights = varIdent(form = ~ 1 | real_vs_control),
                # method = "REML",
                na.action = na.omit
            )}, error = function(e){
                warning(paste0("Model failed to converge:", e$message))
                return(NULL)
        })
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

