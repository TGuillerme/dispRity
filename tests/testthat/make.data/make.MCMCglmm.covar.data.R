
# ## Loading data and packages
# load(file = "covar_char_data.rda")
# load(file = "covar_tree_data.rda")
# library(MCMCglmm)

# ################################################################################################
# # Step 3 - MCMCglmm !!!!
# # There are many ways we can model covariances in MCMCglmm, including how we separate 
# # phylogenetic and non-phylogenetic effects. Below we will try a range of models, starting simply with
# # no phylogenetic effects and building up to more complex models.
# #
# ################################################################################################

# ntraits <- 3 #this refers to the number of traits in the covariance matrix. Here we are using 3 througout


# # The simplest model simply estimates the overal covariance matrix for the whole data set
# model1 <- MCMCglmm(cbind(PC1, PC2, PC3) ~ trait-1, family = rep("gaussian", ntraits),
#                       rcov=
#                        ~ us(trait):units,
#                       data = covar_char_data, nitt = 110000, burnin = 10000, thin = 100) 


# # The next model, still without phylogeny, involves modelling clade effect
# # Note that we are using clade both in the fixed effects and in the random effects
# # The random effects relate to covariances and main effects to means
# model2<- MCMCglmm(cbind(PC1, PC2, PC3) ~ trait:clade-1, family = rep("gaussian", ntraits),
#                       rcov=
#                        ~ us(at.level(clade,1):trait):units
#                       + us(at.level(clade,2):trait):units
#                       + us(at.level(clade,3):trait):units,
#                       data = covar_char_data, nitt = 110000, burnin = 10000, thin = 100) 


# # Now we will add phylogeny. To begin with we will not include the clade (gull, plover, sandpiper) effect
# # and instead just partition covariances into phylogenetic and non-phylogenetic effects.
# # At this point the model is becoming more complex and we need to specific a prior.
# priorRG1 <- list(R=list(
#   R1 = list(V = diag(ntraits), nu = 0.002)),
#   G=list(
#     G1 = list(V = diag(ntraits), nu = 0.002)))


# model3 <- MCMCglmm(cbind(PC1, PC2, PC3) ~ trait-1, family = rep("gaussian", ntraits),
#                        random =
#                          ~ us(trait):animal,
#                       rcov=
#                        ~ us(trait):units,
#                        prior= priorRG1, pedigree = covar_tree_data, data = covar_char_data, nitt = 110000, burnin = 10000, thin = 100) 
                       
                       
# # Now we will add phylogeny with the clade (gull, plover, sandpiper) effect
# # This model has a single overall phylogenetic effects and clade specific non-phylogenetic effects
# # Note the changes to the prior specification as well as the main model
# # This is the model that I would suggest using to test for changes in clade covariances relative to phylogeny to test which clades innovate/elaborate

# priorRG2 <- list(R=list(
#       R1 = list(V = diag(ntraits), nu = 0.002),
#       R2 = list(V = diag(ntraits), nu = 0.002),
#       R3 = list(V = diag(ntraits), nu = 0.002)), 
#     G=list(
#       G1 = list(V = diag(ntraits), nu = 0.002)))


# model4 <- MCMCglmm(cbind(PC1, PC2, PC3) ~ trait:clade-1, family = rep("gaussian", ntraits),
#                        random =
#                          ~ us(trait):animal,
#                       rcov=
#                          ~ us(at.level(clade,1):trait):units
#                          + us(at.level(clade,2):trait):units
#                          + us(at.level(clade,3):trait):units,
#                        prior= priorRG2, pedigree = covar_tree_data, data = covar_char_data, nitt = 110000, burnin = 10000, thin = 100) 



# # Now we will add phylogeny with phylogenetic and non-phylogenetic clade (gull, plover, sandpiper) effects
# # This model has a single overall phylogenetic effects and clade specific non-phylogenetic effects
# # Note the changes to the prior specification as well as the main model
# # This is the model that I would suggest using to test for changes in clade covariances relative to phylogeny to test which clades innovate/elaborate

# priorRG3 <- list(R=list(
#       R1 = list(V = diag(ntraits), nu = 0.002),
#       R2 = list(V = diag(ntraits), nu = 0.002),
#       R3 = list(V = diag(ntraits), nu = 0.002)), 
#     G=list(
#       G1 = list(V = diag(ntraits), nu = 0.002),
#       G2 = list(V = diag(ntraits), nu = 0.002),
#       G3 = list(V = diag(ntraits), nu = 0.002)))


# model5 <- MCMCglmm(cbind(PC1, PC2, PC3) ~ trait:clade-1, family = rep("gaussian", ntraits),
#                        random =
#                          ~ us(at.level(clade,1):trait):animal
#                          + us(at.level(clade,2):trait):animal
#                          + us(at.level(clade,3):trait):animal,
#                       rcov=
#                          ~ us(at.level(clade,1):trait):units
#                          + us(at.level(clade,2):trait):units
#                          + us(at.level(clade,3):trait):units,
#                        prior= priorRG3, pedigree = covar_tree_data, data = covar_char_data, nitt = 110000, burnin = 10000, thin = 100) 




# # Now we will add phylogeny with phylogenetic and non-phylogenetic clade (gull, plover, sandpiper) effects and an overall phylogenetic effect
# # This may well stretch the limits of the data in many cases

# priorRG4 <- list(R=list(
#       R1 = list(V = diag(ntraits), nu = 0.002),
#       R2 = list(V = diag(ntraits), nu = 0.002),
#       R3 = list(V = diag(ntraits), nu = 0.002)), 
#     G=list(
#       G1 = list(V = diag(ntraits), nu = 0.002),
#       G2 = list(V = diag(ntraits), nu = 0.002),
#       G3 = list(V = diag(ntraits), nu = 0.002),
#       G4 = list(V = diag(ntraits), nu = 0.002)))


# model6 <- MCMCglmm(cbind(PC1, PC2, PC3) ~ trait:clade-1, family = rep("gaussian", ntraits),
#                        random =
#                          ~ us(at.level(clade,1):trait):animal
#                          + us(at.level(clade,2):trait):animal
#                          + us(at.level(clade,3):trait):animal
#                          + us(trait):animal,
#                       rcov=
#                          ~ us(at.level(clade,1):trait):units
#                          + us(at.level(clade,2):trait):units
#                          + us(at.level(clade,3):trait):units,
#                        prior= priorRG4, pedigree = covar_tree_data, data = covar_char_data, nitt = 110000, burnin = 10000, thin = 100) 


# # Finally, a possible alternative to model 4. Here we have a background phylogenetic component, clade specific phylogenetic components, and a general non-phylogenetic effect
# # I _think_ this might be correct model for clade to global comparison.

# priorRG5 <- list(R=list(
#       R1 = list(V = diag(ntraits), nu = 0.002)), 
#     G=list(
#       G1 = list(V = diag(ntraits), nu = 0.002),
#       G2 = list(V = diag(ntraits), nu = 0.002),
#       G3 = list(V = diag(ntraits), nu = 0.002),
#       G4 = list(V = diag(ntraits), nu = 0.002)))


# model7 <- MCMCglmm(cbind(PC1, PC2, PC3) ~ trait:clade-1, family = rep("gaussian", ntraits),
#                        random =
#                          ~ us(at.level(clade,1):trait):animal
#                          + us(at.level(clade,2):trait):animal
#                          + us(at.level(clade,3):trait):animal
#                          + us(trait):animal,
#                       rcov=
#                          ~ us(trait):units,
#                        prior= priorRG5, pedigree = covar_tree_data, data = covar_char_data, nitt = 110000, burnin = 10000, thin = 100) 


# #TG: models run smooth, check with more clades and more traits
# #TG: one way to increase things would be to specify more constrained priors?
# covar_model_list <- list(model1, model2, model3, model4, model5, model6, model7)
# save(covar_model_list, file = "covar_model_list.Rda")