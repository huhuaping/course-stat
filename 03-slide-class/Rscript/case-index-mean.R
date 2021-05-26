
# load data ----
df_index <- openxlsx::read.xlsx(here::here("data","complex-index-mean.xlsx"))

calc_index <- df_index %>%
  mutate(f0x0 = f0 * x_0,
         f0x1 = f0 * x_1,
         f1x1 = f1 * x_1,
         f1x0 = f1 * x_0) 
## for simple index-----
sum_00 <- sum(calc_index$f0x0)
sum_01 <- sum(calc_index$f0x1)
sum_11 <- sum(calc_index$f1x1)
sum_10 <- sum(calc_index$f1x0)

sum_f1 <- sum(calc_index$f1)
sum_f0 <- sum(calc_index$f0)

xn <- sum_10/sum_f1
x0 <- sum_00/sum_f0
x1 <- sum_11/sum_f1

I_f <- xn/x0
I_x <- x1/xn


change_tot <- x1 - x0
K <- x1/x0



