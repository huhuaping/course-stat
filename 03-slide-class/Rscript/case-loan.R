
# load data ----
df_loan <- openxlsx::read.xlsx(here("data","textbook","example",
                                    "case-11-6-loan.xlsx"),
                               startRow = 2)

# rename chn ----
names_chn <- c("分行编号",	"不良贷款",	"各项贷款余额
"	,"本年累计应收贷款"	,"贷款项目个数",	"本年固定资产投资额")
names_eng <- names(df_loan)
n <- nrow(df_loan)

# FF and ff table ----

avr_X <- mean(df_loan$loan.surplus)
avr_Y <- mean(df_loan$loan.bad)

df_rel1 <- df_loan %>%
  select(1:3) %>%
  mutate(XY = loan.bad *loan.surplus,
         X_sqr = loan.surplus^2,
         Y_sqr = loan.bad^2,
         x = loan.surplus - avr_X,
         y = loan.bad - avr_Y,
         x_sqr = x^2,
         y_sqr = y^2,
         xy = x*y)

## sum FF ----
tot_Y <- sum(df_rel1$loan.bad)
tot_X <- sum(df_rel1$loan.surplus)
tot_XX <- sum(df_rel1$X_sqr)
tot_YY <- sum(df_rel1$Y_sqr)
tot_XY <- sum(df_rel1$XY)

sum_x <- sum(df_rel1$x)
sum_y <- sum(df_rel1$y)
sum_xx <- sum(df_rel1$x_sqr)
sum_yy <- sum(df_rel1$y_sqr)
sum_xy <- sum(df_rel1$xy)


# cacl stats----

V1 <- df_loan$loan.bad
V2 <- df_loan$loan.surplus
V3 <- df_loan$loan.numbers


r_12 <- round(cor(V1,V2),4)
r_13 <- round(cor(V1,V3),4)
r_23 <- round(cor(V2,V3),4)

t_r <- round(abs(r_12)*sqrt((n-2)/(1-r_12^2)),4)

r12.3 <- par_corl(V1, V2, V3)
r13.2 <- par_corl(V1, V3, V2)
r23.1 <- par_corl(V2, V3, V1)
