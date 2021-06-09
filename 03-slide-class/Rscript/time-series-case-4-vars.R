
# load data ----
df <- openxlsx::read.xlsx(here("data","textbook","example",
                                    "case-13-1-4-vars.xlsx"),
                               startRow = 2)
n <- nrow(df)

names_chn <- c("年份","啤酒产量（万千升）", "人均GDP（元）",
               "煤炭占能源消费总量的比重(%)"	,"居民消费价格指数（上年=100）")
