library(eurostat)
library(readr)
library(countrycode)
library(bayesPop)
library(bayesMig)
library(bayesLife)
library(wpp2019)
library(tidyverse)
library(writexl)
library(pxweb)

my.mig.file <- "data/my_net_migration_rates.txt"
head(read.delim(my.mig.file, check.names = FALSE))

mig.dir <- "data/mig"
mc<- run.mig.mcmc(nr.chains = 4, 
                  iter = 10000,
                  output.dir = mig.dir,
                  my.mig.file = my.mig.file,
                  present.year = 2024,
                  start.year = 2000,
                  wpp.year = 2024,
                  annual = TRUE,
                  parallel= TRUE,
                  replace.output = TRUE)

mig.predictions <- mig.predict(sim.dir = mig.dir, 
                          nr.traj = 10000, 
                          burnin = 1000, 
                          end.year = 2050, 
                          save.as.ascii = 'all', 
                          replace.output = TRUE)

mig.trajectories.plot(mig.predictions, "Finland", pi = 80, ylim = c(-0.02, 0.02), nr.traj = 0)

########
#national forecast
########
sim.dir <- file.path(find.package("bayesPop"), "ex-data", "Pop")
nat.tfr.dir <- "data/wpp2024_projections/TFR1unc/sim20241101"
nat.e0.dir <- "data/wpp2024_projections/e01/sim20241101"
mig.file <- "data/mig_counts_Finland.txt"
mig.traj.file <- file.path(mig.dir, "predictions/ascii_trajectories.csv")
popM0.file<- "data/popM_koko.txt"
popF0.file<- "data/popF_koko.txt"

pred<- pop.predict(countries=246,default.country = 246,wpp.year = 2024, present.year = 2024,end.year = 2050,
                   output.dir = "results/finland",annual = TRUE,nr.traj=10000,
                   inputs = list( popM = popM0.file, popF = popF0.file,
                                  e0F.sim.dir = nat.e0.dir, 
                                  e0M.sim.dir = "joint_",
                                  tfr.sim.dir = nat.tfr.dir,                   
                                  mig = mig.file, migtraj = mig.traj.file),
                   mig.age.method = "rc",
                   mig.is.rate = c(FALSE, TRUE),
                   pasfr.ignore.phase2 = FALSE,
                   replace.output = TRUE, keep.vital.events = TRUE)  

observed2 <- get.pop.ex("G246", pred, as.dt=TRUE, observed = TRUE) |> rename(Muuttaneita=indicator) |> 
  left_join(get.pop.ex("P246", pred, as.dt=TRUE, observed = TRUE)) |> rename(pop=indicator) |> 
  left_join(get.pop.ex("P246[18:64]", pred, as.dt=TRUE, observed = TRUE)) |> rename(tyoikaiset=indicator) |> 
  left_join(get.pop.ex("P246[65:120]/P246[18:64]", pred, as.dt=TRUE, observed = TRUE)) |> rename(huolto=indicator) |> 
  left_join(get.pop.ex("F246", pred, as.dt=TRUE, observed = TRUE)) |> rename(Syntyvyys=indicator) |> 
  left_join(get.pop.ex("E246{0}", pred, as.dt=TRUE, observed = TRUE)) |> rename(e0=indicator) 

observed_long <- observed2 |>
  pivot_longer(cols = -c(year), names_to = "variable", values_to = "median") |>
  filter(year>1999)

predicted <- get.pop.ex("G246", pred, as.dt=TRUE) |> rename(Muuttaneita=indicator) |> 
  left_join(get.pop.ex("P246", pred, as.dt=TRUE)) |> rename(pop=indicator) |> 
  left_join(get.pop.ex("P246[18:64]", pred, as.dt=TRUE)) |> rename(tyoikaiset=indicator) |> 
  left_join(get.pop.ex("P246[65:120]/P246[18:64]", pred, as.dt=TRUE)) |> rename(huolto=indicator) |> 
  left_join(get.pop.ex("F246", pred, as.dt=TRUE)) |> rename(Syntyvyys=indicator) |> 
  left_join(get.pop.ex("E246{0}", pred, as.dt=TRUE)) |> rename(e0=indicator)

intervals <- predicted |>
  pivot_longer(cols = -c(year, trajectory), names_to = "variable", values_to = "value") |>
  group_by(year, variable) |>
  summarize(
    lower_80 = quantile(value, 0.10),
    upper_80 = quantile(value, 0.90),
    lower_50 = quantile(value, 0.25),
    upper_50 = quantile(value, 0.75),
    median = median(value),
    .groups = "drop"
  ) |>
  bind_rows(observed_long)
write_xlsx(intervals, "ennusteet.xlsx")
write_xlsx(predicted, "simulaatiot.xlsx")