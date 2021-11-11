require(mvtnorm)
require(tidyr)
require(dplyr)

rep_data_frame <-  function(d, n) d[rep(seq_len(nrow(d)), n), ]

get_sim_fit_gllvm <- function(mod_gllvm, pred_frame, num_sims = 500){
# Basic naming of dimensions of covariance matrix to make life easier [optional]
colnames(mod_gllvm$Hess$cov.mat.mod) <- rownames(mod_gllvm$Hess$cov.mat.mod) <- names(mod_gllvm$TMBfn$par[mod_gllvm$Hess$incl])

# Begin advanced naming
getbeta_names <- data.frame(intercept = mod_gllvm$params$beta0, mod_gllvm$params$Xcoef) %>% 
    rownames_to_column(var = "measurements") %>%
    pivot_longer(-measurements) %>% 
    dplyr::select(measurements:name) %>% 
    apply(., 1, function(x) paste0(x, collapse = ":"))   
num_beta <- length(getbeta_names)

colnames(mod_gllvm$Hess$cov.mat.mod)[1:num_beta] <- rownames(mod_gllvm$Hess$cov.mat.mod)[1:num_beta] <- getbeta_names
# 
# # Check names are OK by confirming standard errors match
# mod_gllvm$Hess$cov.mat.mod %>% 
#     diag %>% 
#     sqrt %>% 
#     {.[1:num_beta]}
# 
# cbind(mod_gllvm$sd$beta0, mod_gllvm$sd$Xcoef)


# Simulate from approximate large sample distributon of beta/Xcoef
# Due to normality and the fact that your predictions are based on setting the LVs to zero, then it does not better than you only generate a subset of all the parameters. 
# Other types of predictions would be more complicated though.
#num_sims <- 500
simcoefs <- rmvnorm(num_sims, mean = mod_gllvm$TMBfn$par[mod_gllvm$Hess$incl][1:num_beta], sigma = mod_gllvm$Hess$cov.mat.mod[1:num_beta, 1:num_beta])
colnames(simcoefs) <- getbeta_names
pred_model_mat <- model.matrix(mod_gllvm$formula, pred_frame)

# Construct predictions for each simulated set of parameters to get a set of simulated predictions
all_preds <- array(NA, dim = c(num_sims, nrow(pred_frame), ncol(mod_gllvm$y)), dimnames = list(sims = 1:num_sims, unit = 1:nrow(pred_frame), measurement = colnames(mod_gllvm$y))) 
for(k0 in 1:num_sims) {
    cwcoefs_mat <- matrix(simcoefs[k0,], nrow = ncol(mod_gllvm$y), byrow = TRUE)
    all_preds[k0,,] <- tcrossprod(pred_model_mat, cwcoefs_mat)
}

ptab <- rep_data_frame(pred_frame, 
                       n = dim(all_preds)[1] *dim(all_preds)[3]) %>%
    as_tibble() 

bind_cols(ptab, 
          all_preds %>%
              as_tibble(all_preds) %>%
              mutate(sim = 1:n()) %>%
              pivot_longer(-sim) %>%
              mutate(name = gsub("^([[:digit:]])+\\.", "", name))#get rid of digits
) 
    

}

# Get mean and quantiles of the predictions. 
# If one want differences or other jazz, then you need to do 
# some manipulation before taking quantiles. 
#But hopefully one gets the gist
# 
# all_preds <- get_sim_fit_gllvm(mod_gllvm, pred_frame)
# apply(all_preds, c(2,3), mean)
# apply(all_preds, c(2,3), quantile, prob = c(0.025, 0.975))

# 
# a <- as_tibble(all_preds) %>%
#     mutate(sim = 1:n()) %>%
#     pivot_longer(-sim) %>%
#     mutate(name = gsub("^([[:digit:]])+\\.", "", name))
# 
# b <- rep_data_frame(pred_frame, dim(all_preds)[1] *dim(all_preds)[3])
# 
# p <- bind_cols(a,b)
# 
# #posthoc calc
# diffs <- p %>%
#     group_by(sim, name, species) %>%
#     arrange(sex) %>%
#     summarize(diff = value[1] - value[2]) %>%
#     ungroup() %>%
#     group_by(species, name) %>%
#     summarize(mean_diff = mean(diff),
#               lq = quantile(diff, prob = 0.025),
#               uq = quantile(diff, prob = 0.975))
# 
# ggplot(diffs,
#        aes(x = species,
#            y = mean_diff,
#            ymin = lq,
#            ymax = uq)) +
#     geom_point() +
#     geom_linerange() +
#     coord_flip() +
#     facet_wrap(vars(name), scale = "free") +
#     geom_hline(yintercept = 0, lty = 2) 
#     
# 
# 
# ggplot(penguins,
#        aes(x = species, y = body_mass_g, color = sex)) +
#     geom_jitter()
