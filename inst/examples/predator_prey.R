# install and load the package
devtools::install_github("r-and-gama/rama")
library(rama)

# if necessary to configure GAMA path
# defpath("/Applications/Gama.app")

# load the experiment
exp <- load_experiment("prey_predator",
                       system.file("examples/predator_prey/models",
                                   "predator_prey.gaml", package = "rama"))

# to explore the parameters and the observed variables
parameters(exp)
obs_rates(exp)

# to set the experiment
exp$p_Initial_number_of_preys_ <- 990
exp$p_Initial_number_of_predators_ <- 100
exp$p_Predator_probability_reproduce_ <- 0.1
exp$tmax <- 100

# to run the experiment (do not run)
#out <- run_experiment(exp)

# to visualize the output
with(out[[1]],plot(Step,`Number of preys`,type="l",lwd=2,col="red"))

#makemovie(output[[1]])
