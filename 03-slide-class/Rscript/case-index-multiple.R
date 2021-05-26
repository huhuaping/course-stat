
# load data ----
df_index <- openxlsx::read.xlsx(here::here("data","complex-index-multiple.xlsx"))

calc_index <- df_index %>%
  mutate(q0m0p0 = q_0 *m_0* p_0,
         q1m0p0 = q_1 *m_0* p_0,
         q1m1p0 = q_1 *m_1* p_0,
         q1m1p1 = q_1 *m_1* p_1
         ) 
## for simple index-----
sum_000 <- sum(calc_index$q0m0p0)
sum_100 <- sum(calc_index$q1m0p0)
sum_110 <- sum(calc_index$q1m1p0)
sum_111 <- sum(calc_index$q1m1p1)

I_q <- sum_100/sum_000
I_m <- sum_110/sum_100
I_p <- sum_111/sum_110


change_tot <- sum_111 - sum_000
K <- sum_111/sum_000



