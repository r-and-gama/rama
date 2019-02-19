# install and load the package
#install.packages("devtools")
#devtools::install_github("r-and-gama/rama")
library(rama)

# if necessary to configure GAMA path
# defpath("/Applications/Gama.app")

# Load an experiment
exp <- load_experiment("run",
                       system.file("examples", "snubbies.gaml",
                                   package = "rama"))

## Preparing the experiment
exp$tmax <- 2 * 365 * 24 # two years ( step is one hour)
n_sim <- 2 # number of simulations
# preparing the parameters
exp$p_v_max <- 0.3472222
exp$p_s_max <- 0.5
exp$p_explorer_snubbies <- runif(n_sim, 0.1, 0.5)
exp$p_viscosity_factor_habitat_1 <- 1
exp$p_viscosity_factor_habitat_2 <- 0.9
exp$p_viscosity_factor_habitat_3 <- 0.5
exp$p_viscosity_factor_habitat_4 <- 0.1
exp$p_viscosit_factor_habitat_5 <- 0
exp$p_security_factor_habitat_1 <- 1
exp$p_security_factor_habitat_2 <- 0.9
exp$p_security_factor_habitat_3 <- 0.5
exp$p_security_factor_habitat_4 <- 0.1
exp$p_security_factor_habitat_5 <- 0
# preparing the monitoring rates variables
exp$r_number_of_dispersers <- 24
exp$r_map <- 30 * 24 ## save the corresponding bit map every 30 days
# replace NA by id number
exp[, "p_simulation_id"] <-  seq(n_sim) # method to avoid duplication of rows

## Run the experiments
out <- run_experiment(exp)
