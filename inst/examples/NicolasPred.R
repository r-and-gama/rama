# install and load the package
devtools::install_github("r-and-gama/rama")
library(rama)

# if necessary to configure GAMA path
# defpath("/Applications/Gama.app")

# to load the experiment
exp <- load_experiment("prey_predator",
                       system.file("examples", "NicolasPred.gaml",
                                   package = "rama"))




# to set the experiment
exp$p_danger_distance <-  c(100, 200, 300, 400, 500, 600, 700, 800, 900,
                            1000, 1500, 2000, 2500, 3000, 3500, 4000, 5500,
                            6000, 7000, 8000, 9000, 10000, 12000, 14000, 16000,
                            20000, 25000, 30000, 40000, 50000)
exp$p_environment_size <- c(500, 10000, 15000, 20000, 30000, 40000, 50000)

exp$seed<- seq(1, 30, 1)
exp$tmax <- 5000
# To test we set a shorter duration...
exp$tmax <- 3

exp$r_mean_pop <- 1
exp$r_micro_b <- 1
exp$r_nb_predators <- 1
exp$r_nb_preys <- 1

# to run the experiment (do not run)
#out <- run_experiment(exp)
