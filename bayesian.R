
#bayesian 
library(brms)
library(rstan)

form <- HCB ~ urban_degree_dic + (1|Cohort)
brm(form, data = pest_pest1_nm)

brm(formula =HCB ~ urban_degree_dic ,
    data = pest_pest1_nm, brmsfamily("gaussian"),
    prior = c(prior(student_t(3, 2.2, 2.5), class = "Intercept"),
              prior(student_t(3,0, 2.5), class = "sigma")),
    warmup = 10, iter = 20, chains = 1)

get_prior(formula = HCB ~ urban_degree_dic,data = pest_pest1_nm)

example(stan_model, package = "rstan", run.dontrun = TRUE)
