
# load data ----
df <- openxlsx::read.xlsx(here("data","textbook","example",
                                    "case-13-1-4-vars.xlsx"),
                               startRow = 2)
n <- nrow(df)

names_chn <- c("年份","啤酒产量（万千升）", "人均GDP（元）",
               "煤炭占能源消费总量的比重(%)"	,"居民消费价格指数（上年=100）")

#=====cpi forecast====
cpi_ts <- ts(df$cpi, start=2000, end = 2013)
m <- 5
h <- 1
# Average method
f_simple <- meanf(cpi_ts, h)

# Naïve method
f_naive <- naive(cpi_ts, h)

move_cpi <- ma(cpi_ts, m, centre = F)


cpi_tbl <- as_tsibble(cpi_ts) %>%
  rename_all(~c("year", "cpi")) %>%
  mutate(cpi_m5 = ma(cpi, 6, centre = T))
