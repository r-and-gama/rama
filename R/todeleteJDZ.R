exp0 <- experiment(
  data.frame(S0 = c(900, 100), # this is a data frame of 2 lines
             I0 = c(100, 900),
             R0 = 0,
             beta = 2,
             gamma = .15),
  data.frame(S = 1, I = 2, R = 3),
  tmax = 1000,
  seed = 1,
  model = gaml_file,
  experiment = "sir"
)
exp1 <- load_experiment("sir", gaml_file, "sir")
exp0 <- repl(exp1,3)
exp0[1,"p_S0"] <- 500
exp0[2,"p_S0"] <- 800
exp0[3,"p_S0"] <- 200
exp0[1,"p_I0"] <- 500
exp0[2,"p_I0"] <- 200
exp0[3,"p_I0"] <- 800
exp0[2,"r_S"] <- 2
exp0[3,"r_S"] <- 3
exp0[3,"r_I"] <- 8



#otp <- run_experiment(exp0)
str(output)
newoutput <- realexp(output,exp0)
str(newoutput)


df <-   data.frame(S0 = c(900, 800, 500), # this is a data frame of 3 lines
                   I0 = c(100, 200, 500),
                   R0 = 0,
                   beta = 1.5,
                   gamma = .15,
                   S = c(1,2,3),
                   I = c(2,4,6),
                   R =c(10,20,30),
                   nbiter = 1000,
                   seed = "123456789")

exp0 <- experiment(
  df,
  parameters = c("S0","I0","R0","beta","gamma"),
  obsrates  = c("S", "I", "R"),
  tmax = "nbiter",
  seed = "seed",
  experiment = "sir",
  model = system.file("examples", "sir.gaml", package = "rama"),
  dir = "testsir"
)


exp0
otp <- run_experiment(exp0)
str(otp)
newoutput <- realexp(otp,exp0)
str(newoutput)
