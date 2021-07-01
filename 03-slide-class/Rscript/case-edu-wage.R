
# read data ----
data_wage <- dplyr::as_tibble(read.xlsx(here("data","extra",
                                 "Table-3-2-edu-wage.xlsx"), 
                                 sheet = 1))
n <- dim(data_wage)[1]
kk <- 2

mod_wage <- "Y~X"

lm.wage <- lm(formula = mod_wage, data_wage)

# FF and ff----

calc_tbl <- data_wage %>%
  mutate(FF_XY = X*Y, FF_X_sqr = X^2, FF_Y_sqr = Y^2,
         ff_x = X-mean(X), ff_y = Y-mean(Y),
         ff_xy=ff_x*ff_y, ff_x_sqr=ff_x^2, ff_y_sqr=ff_y^2, 
         Y_hat= fitted(lm.wage),
         e_i = residuals(lm.wage), 
         e_i_star =e_i/sd(e_i),
         e_i_star_st = MASS::studres(lm.wage),
         e_i_sqr = e_i^2)  %>%
  rbind(colSums(.)) %>%  
  mutate(obs=replace(obs, obs==91, "sum")) 

k <- dim(calc_tbl)[2]

FF_ff <- calc_tbl %>%
  filter(obs=="sum") %>%
  mutate_if(is.numeric, funs(round(., digits = 3)))

# rename
head.name <- c("obs","$X_i$","$Y_i$","$X_iY_i$", "$X_i^2$", "$Y_i^2$", "$x_i$", "$y_i$", "$x_iy_i$","$x_i^2$","$y_i^2$","$\\hat{Y}_i$","$e_i$","$e_{i, sd}^{\\ast}$","$e_{i, st}^{\\ast}$","$e_i^2$")  
show_tbl <- calc_tbl  %>% 
  rename_at(vars(names(.)), ~ head.name)

# estimate ----

b2 <- lm.wage$coefficients[2]
b1 <- lm.wage$coefficients[1]
mean_X <- (FF_ff$X)/n
mean_Y <- (FF_ff$Y)/n
dev <- (FF_ff$e_i_sqr)/(n-2)
dev_b2 <- dev/FF_ff$ff_x_sqr
dev_b1 <- dev_b2*(FF_ff$FF_X_sqr/n)
S_b1 <- sqrt(dev_b1)
S_b2 <- sqrt(dev_b2)
TSS <- sum((data_wage$Y -mean(data_wage$Y))^2)
RSS <- FF_ff$e_i_sqr
ESS <- TSS -RSS
r2  <- ESS/TSS
r <- cor(data_wage$X, data_wage$Y)
cov_XY <- cov(data_wage$X, data_wage$Y)
S_X <- sd(data_wage$X)
S_Y <- sd(data_wage$Y)
t_0.975 <- qt(0.975, n-2)
chisq_0.025 <- qchisq(0.025,n-2)
chisq_0.975 <- qchisq(0.975,n-2)
t_b1 <- b1/S_b1
t_b2 <- b2/S_b2
f_ESS <- 1
f_RSS <- n-2
f_TSS <- n-1
f_0.95 <- qf(0.95,f_ESS,f_RSS)
f_test <- (ESS/f_ESS)/(RSS/f_RSS)

# forecast -----
X_0 <- 20
Y_0_hat <- b1+b2*X_0
t_0.95 <- qt(0.95,n-2)
S2_Y0h <- dev*(1/n+(X_0-mean_X)^2/(FF_ff$ff_x_sqr))
Y_exp_lft <- Y_0_hat - t_0.95*(sqrt(S2_Y0h))
Y_exp_rht <- Y_0_hat + t_0.95*(sqrt(S2_Y0h))
S2_Y0h_mns <- dev*(1+1/n+(X_0-mean_X)^2/(FF_ff$ff_x_sqr))
Y_ind_lft <- Y_0_hat - t_0.95*(sqrt(S2_Y0h_mns))
Y_ind_rht <- Y_0_hat + t_0.95*(sqrt(S2_Y0h_mns))

