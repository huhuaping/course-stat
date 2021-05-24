
# load data ----
df_index <- openxlsx::read.xlsx(here::here("data","complex-index.xlsx"))

calc_index <- df_index %>%
  mutate(q0p0 = q_0 * p_0,
         q0p1 = q_0 * p_1,
         q1p1 = q_1 * p_1,
         q1p0 = q_1 * p_0,
         k_q = round(q_1/q_0,4),
         k_p = round(p_1/p_0,4)) 
sum_00 <- sum(calc_index$q0p0)
sum_01 <- sum(calc_index$q0p1)
sum_11 <- sum(calc_index$q1p1)
sum_10 <- sum(calc_index$q1p0)
I_qL <- sum_10/sum_00
I_pL <- sum_01/sum_00
I_qP <- sum_11/sum_01
I_pP <- sum_11/sum_10

change_tot <- sum_11 - sum_00


# name chn -----
names_chn <- c("产品名称", "单位",
               "产量\\(q_0\\)","产量\\(q_1\\)",
               "价格\\(p_0\\)","价格\\(p_1\\)",
               "\\(q_0p_0\\)","\\(q_0p_1\\)",
               "\\(q_1p_1\\)","\\(q_1p_0\\)",
               "\\(k_q\\)", "\\(k_p\\)")
tbl_index <- 
  calc_index  %>%
  janitor::adorn_totals(.,where = "row",name = c("合计")) %>%
  mutate_at(vars(contains("_")), funs(ifelse(.==max(.), NA, .)))%>%
  rename_at(vars(names(.)),~ all_of(names_chn)) 

# individual index case

