
# load data ----
df_index <- openxlsx::read.xlsx(here::here("data","complex-index.xlsx"))

calc_index <- df_index %>%
  mutate(q0p0 = q_0 * p_0,
         q0p1 = q_0 * p_1,
         q1p1 = q_1 * p_1,
         q1p0 = q_1 * p_0,
         k_q = q_1/q_0,
         k_p = p_1/p_0,
         aq.L = k_q*q0p0,
         ap.L = k_p*q0p0,
         aq.P = k_q*q0p1,
         ap.P = k_p*q1p0,
         hq.L = q1p0/k_q,
         hp.L = q0p1/k_p,
         hq.P = q1p1/k_q,
         hp.P = q1p1/k_p) 
## for simple index-----
sum_00 <- sum(calc_index$q0p0)
sum_01 <- sum(calc_index$q0p1)
sum_11 <- sum(calc_index$q1p1)
sum_10 <- sum(calc_index$q1p0)
## for weighted mean------
sum_aq.L <- sum(calc_index$aq.L)
sum_ap.L <- sum(calc_index$ap.L)
sum_aq.P <- sum(calc_index$aq.P)
sum_ap.P <- sum(calc_index$ap.P)
## for harmony mean-------
sum_hq.L <- sum(calc_index$hq.L)
sum_hp.L <- sum(calc_index$hp.L)
sum_hq.P <- sum(calc_index$hq.P)
sum_hp.P <- sum(calc_index$hp.P)

I_qL <- sum_10/sum_00
I_pL <- sum_01/sum_00
I_qP <- sum_11/sum_01
I_pP <- sum_11/sum_10

change_tot <- sum_11 - sum_00
K <- sum_11/sum_00



