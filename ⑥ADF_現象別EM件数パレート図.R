# ファイル読込
Metis_MF3_2211 <- read_tsv("./tsv_data/Metis_MF3_2211.tsv") # A tibble: 516,414 × 19
Metis_sheet3_2211 <- read_tsv("./tsv_data/Metis_sheet3_2211.tsv")

# フォント設定
# windowsFonts(Japan1GothicBBB = windowsFont("Japan1GothicBBB"))
par(family="Noto Sans")

names(Metis_MF3_2211)
names(Metis_MIF_2211)
Metis_MF3_2211 %>% 
  distinct(Treatment_location)

# 製造月別台数/Metis_MF3_ADF_number_by_Manufacturing_dateの作成
Metis_MF3_ADF_number_by_Manufacturing_date <- 
  Create.graph_for.number_by.month(df = Metis_MIF_2211,
                                 field = "ADF",
                                 choice = c("COOK-C","SINAI-H"),
                                 sel_month = "製造年月",
                                 date.name = "Manufacturing_date",
                                 file.save = FALSE,
                                 write.file_path = "./tsv_data/Metis_MF3_ADF_number_by_Manufacturing_date.tsv",
                                 graph_title = "Metis-MF3 ADF 製造年月別台数",
                                 legend.position = c(0.75, 0.95),
                                 breaks = seq(as.Date("2018-10-01"), as.Date("2022-10-01"), by="6 month"),
                                 graph.save = FALSE,
                                 save.graph_path='./PDF/Metis-MF3_ADF_製造年月別台数.pdf',
                                 graph.width = 10, 
                                 graph.height = 6)

# COOK-Cの現象別EM件数パレート図
Pareto.chart_by.phenomenon(
  df = Metis_MF3_2211,            # 保守データ
  T_location = "ADF部",           # 処置部
  P_name = "COOK-C",              # 周辺機名
  Choices = "Manufacturing_date", # グルーピング条件 
  num = 1,                        # num未満はその他へ集約する。1の場合はすべて表示
  Number_by_Manufacturing_date = Metis_MF3_ADF_number_by_Manufacturing_date, # 製造月別台数
  # E_num = "`COOK-C`",      # 製造月別台数の選択列名
  y1.lim = c(0,4500),          # グラフ第1軸の範囲
  breaks = seq(0, 4500, 1000),　# グラフ第1軸の目盛り
  graph_save = FALSE,         # グラフの保存
  graph_title = "Metis-MF3 COOK-C EM現象別件数 (2019.2～2022.11)",
  graph_path = "./PDF/Metis-MF3_COOK_EM現象別件数パレート図(2019.2～2022.11)-2.pdf",
  graph_width = 22, # グラフの横幅
  graph_height = 6, # グラフの高さ
  hjust = 100        # 棒グラフ上部記載の数値高さ
)

# 製造月別EM件数
COOK_SCname_Manufacturing_date_1 <- 
  Create_Metis_MF3_SCname(
    T_location = "ADF部",           # 処置部
    P_name = "COOK-C",              # 周辺機名
    Choices = "Manufacturing_date", # グルーピング条件 
    num = 1, 
  ) %>% 
  left_join(Metis_MF3_ADF_number_by_Manufacturing_date$df, by="Manufacturing_date") %>% 
  mutate(
    SC_sum_rate = Peripheral_name_SC_sum/`COOK-C`
  )
# EM総数　14724
sum(COOK_SCname_Manufacturing_date_1$Peripheral_name_SC_sum)
# SC現象数　85
print(n=100,COOK_SCname_Manufacturing_date_1 %>% 
        distinct(SC_name_num))

# 製造月別グラフ
# 現象件数
g_COOK_SC_Manufacturing_date <- 
  geom_bar_SCname(df = COOK_SCname_Manufacturing_date_1,
                  x = "Manufacturing_date",
                  y = "Peripheral_name_SC_sum",
                  fill = "SC_name_num",
                  legend_name = "現象SC名称",
                  x_labels = "製造月",
                  y_labels = "件数",
                  title = "Metis-MF3 COOK-C EMにおける現象件数 (2019.2～2022.11)",
                  file_path = "./PDF/Metis-MF3-COOK製造月別EM現象件数(2019.2～2022.11)-2.pdf",
                  f_wrap = TRUE, ncol = 3,
                  g_save = FALSE, w = 16, h = 20,
                  # g_save = F, w = 30, h = 15,
                  legend_position = c(0.77, 0.97),
                  legend_justification = c(0, 1)
  )
#現象比率
g_COOK_SC_Manufacturing_date_rate <- 
  geom_bar_SCname(df = COOK_SCname_Manufacturing_date_1,
                  x = "Manufacturing_date",
                  y = "SC_sum_rate",
                  fill = "SC_name_num",
                  legend_name = "現象SC名称",
                  x_labels = "製造月",
                  y_labels = "台当たり比率（件数/製造台数）",
                  title = "Metis-MF3 COOK-C EMにおける現象比率 (2019.2～2022.11)",
                  file_path = "./PDF/Metis-MF3-COOK製造月別EM現象比率(2019.2～2022.11)-2.pdf",
                  f_wrap = TRUE, ncol = 3,
                  g_save = FALSE, w = 16, h = 20,
                  # g_save = F, w = 30, h = 15,
                  legend_position = c(0.77, 0.97),
                  legend_justification = c(0, 1)
  )

# SINAI-Hの現象別EM件数パレート図
Pareto.chart_by.phenomenon(
  df = Metis_MF3_2211,            # 保守データ
  T_location = "ADF部",           # 処置部
  P_name = "SINAI-H",              # 周辺機名
  Choices = "Manufacturing_date", # グルーピング条件 
  num = 1,                        # num未満はその他へ集約する。1の場合はすべて表示
  Number_by_Manufacturing_date = Metis_MF3_ADF_number_by_Manufacturing_date, # 製造月別台数
  # E_num = "`SINAI-H`",      # 製造月別台数の選択列名
  y1.lim = c(0,2000),          # グラフ第1軸の範囲
  breaks = seq(0, 2000, 500),　# グラフ第1軸の目盛り
  graph_save = FALSE,         # グラフの保存
  graph_title = "Metis-MF3 SINAI-H EM現象別件数 (2019.2～2022.11)",
  graph_path = "./PDF/Metis-MF3_SINAI_EM現象別件数パレート図(2019.2～2022.11)-2.pdf",
  graph_width = 18, # グラフの横幅
  graph_height = 6, # グラフの高さ
  hjust = 50        # 棒グラフ上部記載の数値高さ
)

SINAI_SCname_Manufacturing_date_1 <- 
  Create_Metis_MF3_SCname(
    T_location = "ADF部",           # 処置部
    P_name = "SINAI-H",              # 周辺機名
    Choices = "Manufacturing_date", # グルーピング条件 
    num = 1, 
  ) %>% 
  left_join(Metis_MF3_ADF_number_by_Manufacturing_date$df, by="Manufacturing_date") %>% 
  mutate(
    SC_sum_rate = Peripheral_name_SC_sum/`SINAI-H`
  )
# EM総数　3383
sum(SINAI_SCname_Manufacturing_date_1$Peripheral_name_SC_sum)
# SC現象数　68
print(n=100,SINAI_SCname_Manufacturing_date_1 %>% 
        distinct(SC_name_num))

# 製造月別グラフ
# 現象件数
g_SINAI_SC_Manufacturing_date <- 
  geom_bar_SCname(df = SINAI_SCname_Manufacturing_date_1,
                  x = "Manufacturing_date",
                  y = "Peripheral_name_SC_sum",
                  fill = "SC_name_num",
                  legend_name = "現象SC名称",
                  x_labels = "製造月",
                  y_labels = "件数",
                  title = "Metis-MF3 SINAI-H EMにおける現象件数 (2019.2～2022.11)",
                  file_path = "./PDF/Metis-MF3-SINAI製造月別EM現象件数(2019.2～2022.11).pdf",
                  f_wrap = TRUE, ncol = 3,
                  g_save = FALSE, w = 16, h = 20,
                  # g_save = F, w = 30, h = 15,
                  legend_position = c(0.77, 0.97),
                  legend_justification = c(0, 1)
  )
#現象比率
g_SINAI_SC_Manufacturing_date_rate <- 
  geom_bar_SCname(df = SINAI_SCname_Manufacturing_date_1,
                  x = "Manufacturing_date",
                  y = "SC_sum_rate",
                  fill = "SC_name_num",
                  legend_name = "現象SC名称",
                  x_labels = "製造月",
                  y_labels = "台当たり比率（件数/製造台数）",
                  title = "Metis-MF3 SINAI-H EMにおける現象比率 (2019.2～2022.11)",
                  file_path = "./PDF/Metis-MF3-SINAI製造月別EM現象比率(2019.2～2022.11).pdf",
                  f_wrap = TRUE, ncol = 3,
                  g_save = FALSE, w = 16, h = 20,
                  # g_save = F, w = 30, h = 15,
                  legend_position = c(0.77, 0.97),
                  legend_justification = c(0, 1)
  )
