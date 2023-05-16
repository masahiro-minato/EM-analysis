# ファイル読込
Metis_MF3_2211 <- read_tsv("./tsv_data/Metis_MF3_2211.tsv") # A tibble: 516,414 × 19
Metis_MIF_2211 <- read_tsv("./tsv_data/Metis_MIF_2211.tsv")

# フォント設定
# windowsFonts(Japan1GothicBBB = windowsFont("Japan1GothicBBB"))
par(family="Noto Sans")

# 製造月別台数データ
Metis_MF3_FIN_number_by_Manufacturing_date <- 
  Create.graph_for.number_by.month(df = Metis_MIF_2211,
                                 field = "フィニッシャー",
                                 choice = c("AMUR-C中綴じ","AMUR-C(HY)","VOLGA-E"),
                                 sel_month = "製造年月",
                                 date.name = "Manufacturing_date",
                                 file.save = FALSE,
                                 write.file_path = "./tsv_data/Metis_MF3_FIN_number_by_Manufacturing_date.tsv",
                                 graph_title = "Metis-MF3 FIN 製造年月別台数",
                                 legend.position = c(0.75, 0.95),
                                 breaks = seq(as.Date("2018-10-01"), as.Date("2022-10-01"), by="6 month"),
                                 graph.save = FALSE,
                                 save.graph_path='./PDF/Metis-MF3_FIN_製造年月別台数.pdf',
                                 graph.width = 10, 
                                 graph.height = 6)

# VOLGA-Eの現象別EM件数パレート図
Pareto.chart_by.phenomenon(
    df = Metis_MF3_2211,            # 保守データ
    T_location = "ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部",   # 処置部
    P_name = "VOLGA-E",             # 周辺機名
    Choices = "Manufacturing_date", # グルーピング条件 
    num = 1,                        # num未満はその他へ集約する。1の場合はすべて表示
    Number_by_Manufacturing_date = Metis_MF3_FIN_number_by_Manufacturing_date, # 製造月別台数
    # E_num = "`VOLGA-E`",          # 製造月別台数の選択列名
    y1.lim = c(0,600),              # グラフ第1軸の範囲
    breaks = seq(0, 600, 100),　    # グラフ第1軸の目盛り
    graph_save = FALSE,             # グラフの保存
    graph_title = "Metis-MF3 VOLGA-E EM現象別件数 (2019.2～2022.11)",
    graph_path = "./PDF/Metis-MF3_VOLGA_EM現象別件数パレート図 (2019.2～2022.11)-2.pdf",
    graph_width = 14,               # グラフの横幅
    graph_height = 6,               # グラフの高さ
    hjust = 10                      # 棒グラフ上部記載の数値高さ
)

# AMUR-C(HY)の現象別EM件数パレート図
Pareto.chart_by.phenomenon(
  df = Metis_MF3_2211,            # 保守データ
  T_location = "ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部",   # 処置部
  P_name = "AMUR-C(HY)",          # 周辺機名
  Choices = "Manufacturing_date", # グルーピング条件 
  num = 1,                        # num未満はその他へ集約する。1の場合はすべて表示
  Number_by_Manufacturing_date = Metis_MF3_FIN_number_by_Manufacturing_date, # 製造月別台数
  # E_num = "`AMUR-C(HY)`",       # 製造月別台数の選択列名
  y1.lim = c(0,100),              # グラフ第1軸の範囲
  breaks = seq(0, 100, 20),　     # グラフ第1軸の目盛り
  graph_save = FALSE,             # グラフの保存
  graph_title = "Metis-MF3 AMUR-C(HY) EM現象別件数 (2019.2～2022.11)",
  graph_path = "./PDF/Metis-MF3_AMUR-C(HY)_EM現象別件数パレート図 (2019.2～2022.11).pdf",
  graph_width = 7,                # グラフの横幅
  graph_height = 6,               # グラフの高さ
  hjust = 3                       # 棒グラフ上部記載の数値高さ
)

# AMUR-C中綴じの現象別EM件数パレート図
Pareto.chart_by.phenomenon(
  df = Metis_MF3_2211,            # 保守データ
  T_location = "ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部",   # 処置部
  P_name = "AMUR-C中綴じ",        # 周辺機名
  Choices = "Manufacturing_date", # グルーピング条件 
  num = 1,                        # num未満はその他へ集約する。1の場合はすべて表示
  Number_by_Manufacturing_date = Metis_MF3_FIN_number_by_Manufacturing_date, # 製造月別台数
  # E_num = "`AMUR-C中綴じ`",     # 製造月別台数の選択列名
  y1.lim = c(0,200),              # グラフ第1軸の範囲
  breaks = seq(0, 200, 50),　     # グラフ第1軸の目盛り
  graph_save = FALSE,             # グラフの保存
  graph_title = "Metis-MF3 AMUR-C中綴じ EM現象別件数 (2019.2～2022.11)",
  graph_path = "./PDF/Metis-MF3_AMUR-C中綴じ_EM現象別件数パレート図 (2019.2～2022.11).pdf",
  graph_width = 10,               # グラフの横幅
  graph_height = 6,               # グラフの高さ
  hjust = 5                       # 棒グラフ上部記載の数値高さ
)

#### 以下は関数を使用しない場合 ----------------------------

# VOLGA-E 製造月別EM件数 -----------------------------------
VOLGA_SCname_Manufacturing_date_1 <- 
  Create_Metis_MF3_SCname(
    df = Metis_MF3_2211,
    T_location = "ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部", 
    P_name = "VOLGA-E",
    Choices = "Manufacturing_date",
    # Choices = "Working_month",
    num = 1
  )

VOLGA_SCname_Manufacturing_date_10 <- 
  Create_Metis_MF3_SCname(
    df = Metis_MF3_2211,
    T_location = "ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部", 
    P_name = "VOLGA-E",
    Choices = "Manufacturing_date",
    # Choices = "Working_month",
    num = 10
  )

# EM総数　1734
sum(VOLGA_SCname_Manufacturing_date_1$Peripheral_name_SC_sum)
sum(VOLGA_SCname_Manufacturing_date_10$Peripheral_name_SC_sum)

print(n=51,VOLGA_SCname_Manufacturing_date_10 %>% 
        distinct(SC_name_num))
VOLGA_SCname_Manufacturing_date_1 %>% 
  distinct(SC_name_num)
VOLGA_SCname_Manufacturing_date_1$SC_name_num

# レベル変更
VOLGA_SCname_Manufacturing_date_10 <- 
  VOLGA_SCname_Manufacturing_date_10 %>%
  mutate(
    SC_name_num = factor(SC_name_num, levels = c("SC720",             
                                                 "ｴﾗｰｺｰﾄﾞ･ﾒｯｾｰｼﾞ表示",
                                                 "用紙詰まり",      
                                                 "ｻﾌﾟﾗｲ･ﾊﾟｰﾂ交換表示",
                                                 "機械的異音",        
                                                 "部品外れ", 
                                                 "その他" ))
  )

# 結合
VOLGA_SCname_Manufacturing_date_1 <- 
  VOLGA_SCname_Manufacturing_date_1 %>% 
  left_join(Metis_MF3_FIN_number_by_Manufacturing_date$df, by="Manufacturing_date")

VOLGA_SCname_Manufacturing_date_10 <- 
  VOLGA_SCname_Manufacturing_date_10 %>% 
  left_join(Metis_MF3_FIN_number_by_Manufacturing_date$df, by="Manufacturing_date")

# 現象件数を台数比率へ換算した列追加
VOLGA_SCname_Manufacturing_date_1 <- 
  VOLGA_SCname_Manufacturing_date_1 %>% 
  mutate(
    SC_sum_rate = Peripheral_name_SC_sum/`VOLGA-E`
  )

VOLGA_SCname_Manufacturing_date_10 <- 
  VOLGA_SCname_Manufacturing_date_10 %>% 
  mutate(
    SC_sum_rate = Peripheral_name_SC_sum/`VOLGA-E`
  )

# SC現象別件数
SC_count_VOLGA <- 
  VOLGA_SCname_Manufacturing_date_1 %>% 
  group_by(SC_name_num) %>% 
  summarise(
    SC_count = sum(Peripheral_name_SC_sum)
  ) %>%
  arrange(-SC_count) %>% 
  ungroup()

SC_count_VOLGA <- 
  SC_count_VOLGA %>% 
  mutate(
    rate = SC_count/sum(SC_count),
    cum = cumsum(SC_count), 
    cumrate = cum/sum(SC_count),
    SC_name = forcats::as_factor(SC_name_num)
  )

EM_number.VOLGA <- 
  sum(SC_count_VOLGA$SC_count) # 1283 1734

# 変数のスケーラ。
# pにy2の値ベクトルを与えると、y1の尺に合わせた数字に変換。
# y2をまずゼロ基準に戻し、y2とy1のlimの幅の比でスケーリング
# した後で、y1のゼロ基準からの乖離分を足す。
variable_scaler <- function(p, lim1, lim2){
  to_zero <- p-lim2[1]
  y1_range <- lim1[2]-lim1[1]
  y2_range <- lim2[2]-lim2[1]
  scaled <- to_zero*y1_range/y2_range
  from_zero <- scaled + lim1[1]
  return(from_zero)
}
# 第2軸の目盛りのスケーラ。
# pは、sec_axis()の'.'になる。y1の目盛りをy2の目盛りに読み替えるもの。
# y1の目盛りをまずゼロ基準に戻し、y1とy2のlimの幅の比スケーリング
# した後で、y2の目盛りのゼロ基準からの乖離分を足す。

axis_scaler <- function(p, lim1, lim2){
  to_zero <- p-lim1[1]
  y1_range <- lim1[2]-lim1[1]
  y2_range <- lim2[2]-lim2[1]
  scaled <- to_zero*y2_range/y1_range
  from_zero <- scaled + lim2[1]
  return(from_zero)
}

y1.lim <- c(0,600)
y2.lim <- c(0,1)
g_plate_VOLGA <- 
  ggplot(SC_count_VOLGA, aes(reorder(x = SC_name, X = -SC_count))) + 
  scale_y_continuous(limit=y1.lim,  # 第1軸の範囲
                     breaks=seq(0, 600, 100),  # 第1軸の目盛り
                     sec.axis=sec_axis(
                       ~(axis_scaler(., y1.lim, y2.lim)), # 軸スケーリング
                       breaks=seq(0, 1, 0.2), # 第2軸の目盛り
                       name="累積比（折れ線）")  # y2のラベルはここで設定する
  ) +
  geom_bar(aes(y = SC_count, fill=SC_name), stat = "identity") +
  # labs(x = "現象名称", y = "EM件数", title = "Metis-MF3 VOLGA-E EM現象別件数") +
  labs(x = "現象名称", y = "EM件数", title = str_c("Metis-MF3 VOLGA-E EM現象別件数","　EM総数:",EM_number.VOLGA,"件")) +
  theme_bw() +
  theme(title = element_text(size = 12), text = element_text(size = 12)) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  geom_path(aes(y = variable_scaler(cumrate, y1.lim, y2.lim), group = 0), colour="blue") + 
  geom_point(aes(y = variable_scaler(cumrate, y1.lim, y2.lim)), colour="blue")+
  geom_text(aes(SC_name, SC_count+10, label = round(SC_count,2)),color="black", size = 3)

g_plate_VOLGA
# グラフ保存
ggsave("./PDF/Metis-MF3_VOLGA_EM現象別件数パレート図.pdf", 
       plot = g_plate_VOLGA, device = cairo_pdf, dpi=300, width=14, height=6)

# 製造月別グラフ
# 現象件数
g_VOLGA_SC_Manufacturing_date <- 
  geom_bar_SCname(df = VOLGA_SCname_Manufacturing_date_10,
                  x = "Manufacturing_date",
                  y = "Peripheral_name_SC_sum",
                  fill = "SC_name_num",
                  legend_name = "現象SC名称",
                  x_labels = "製造月",
                  y_labels = "件数",
                  title = "Metis-MF3 VOLGA-E EMにおける現象件数 (2019.2～2022.11)",
                  file_path = "./PDF/Metis-MF3-VOLGA製造月別EMの現象件数(2019.2～2022.11)集約-2.pdf",
                  f_wrap = TRUE, ncol = 2,
                  g_save = TRUE, w = 16, h = 12,
                  # f_wrap = TRUE, ncol = 5,
                  # g_save = TRUE, w = 16, h = 20,
                  # g_save = F, w = 30, h = 15,
                  legend_position = c(0.77, 0.97),
                  legend_justification = c(0, 1)
  )
#現象比率
g_VOLGA_SC_Manufacturing_date_rate <- 
  geom_bar_SCname(df = VOLGA_SCname_Manufacturing_date_10,
                  x = "Manufacturing_date",
                  y = "SC_sum_rate",
                  fill = "SC_name_num",
                  legend_name = "現象SC名称",
                  x_labels = "製造月",
                  y_labels = "台当たり比率（件数/製造台数）",
                  title = "Metis-MF3 VOLGA-E EMにおける現象比率 (2019.2～2022.11)",
                  file_path = "./PDF/Metis-MF3-VOLGA製造月別EMの現象比率(2019.2～2022.11)集約-2.pdf",
                  f_wrap = TRUE, ncol = 2,
                  g_save = TRUE, w = 16, h = 12,
                  # f_wrap = TRUE, ncol = 5,
                  # g_save = TRUE, w = 16, h = 20,
                  # g_save = F, w = 30, h = 15,
                  legend_position = c(0.77, 0.97),
                  legend_justification = c(0, 1)
  )

# AMUR-C 製造月別EM件数 -----------------------------------
AMUR_C_SCname_Manufacturing_date_1 <- 
  Create_Metis_MF3_SCname(
    T_location = "ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部",
    P_name = "AMUR-C中綴じ",
    Choices = "Manufacturing_date",
    # Choices = "Working_month",
    num = 1
  )

AMUR_C_SCname_Manufacturing_date_5 <- 
  Create_Metis_MF3_SCname(
    T_location = "ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部",
    P_name = "AMUR-C中綴じ",
    Choices = "Manufacturing_date",
    # Choices = "Working_month",
    num = 5
  )

# EM総数　536 518 799
sum(AMUR_C_SCname_Manufacturing_date_1$Peripheral_name_SC_sum)
sum(AMUR_C_SCname_Manufacturing_date_5$Peripheral_name_SC_sum)

AMUR_C_SCname_Manufacturing_date_1 %>% 
  distinct(SC_name_num)
AMUR_C_SCname_Manufacturing_date_1$SC_name_num

print(n=51,AMUR_C_SCname_Manufacturing_date_5 %>% 
        distinct(SC_name_num))

# レベル変更
AMUR_C_SCname_Manufacturing_date_5 <- 
  AMUR_C_SCname_Manufacturing_date_5 %>%
  mutate(
    SC_name_num = factor(SC_name_num, levels = c("ｽﾃｰﾌﾟﾙ位置ずれ",    
                                                 "機械的異音",        
                                                 "部品外れ",          
                                                 "用紙詰まり",        
                                                 "SC721",             
                                                 "ｴﾗｰｺｰﾄﾞ･ﾒｯｾｰｼﾞ表示",
                                                 "ｻﾌﾟﾗｲ･ﾊﾟｰﾂ交換表示",
                                                 "ｻﾌﾟﾗｲｾｯﾄできず",    
                                                 "部品破損", 
                                                 "その他" ))
  )

# 結合
AMUR_C_SCname_Manufacturing_date_1 <- 
  AMUR_C_SCname_Manufacturing_date_1 %>% 
  left_join(Metis_MF3_FIN_number_by_Manufacturing_date$df, by="Manufacturing_date")

AMUR_C_SCname_Manufacturing_date_5 <- 
  AMUR_C_SCname_Manufacturing_date_5 %>% 
  left_join(Metis_MF3_FIN_number_by_Manufacturing_date$df, by="Manufacturing_date")

# 現象件数を台数比率へ換算した列追加
AMUR_C_SCname_Manufacturing_date_1 <- 
  AMUR_C_SCname_Manufacturing_date_1 %>% 
  mutate(
    SC_sum_rate = Peripheral_name_SC_sum/`AMUR-C中綴じ`
  )

AMUR_C_SCname_Manufacturing_date_5 <- 
  AMUR_C_SCname_Manufacturing_date_5 %>% 
  mutate(
    SC_sum_rate = Peripheral_name_SC_sum/`AMUR-C中綴じ`
  )

# SC現象別件数
SC_count_AMUR_C <- 
  AMUR_C_SCname_Manufacturing_date_1 %>% 
  group_by(SC_name_num) %>% 
  summarise(
    SC_count = sum(Peripheral_name_SC_sum)
  ) %>%
  arrange(-SC_count)
SC_count_AMUR_C <- 
  SC_count_AMUR_C %>% 
  mutate(
    rate = SC_count/sum(SC_count),
    cum = cumsum(SC_count), 
    cumrate = cum/sum(SC_count),
    SC_name = forcats::as_factor(SC_name_num)
  )

EM_number.AMUR_C <- 
  sum(SC_count_AMUR_C$SC_count)

y1.lim <- c(0,200)
y2.lim <- c(0,1)
g_plate_AMUR_C <- 
  ggplot(SC_count_AMUR_C, aes(reorder(x = SC_name, X = -SC_count))) + 
  scale_y_continuous(limit=y1.lim,  # 第1軸の範囲
                     breaks=seq(0, 200, 20),  # 第1軸の目盛り
                     sec.axis=sec_axis(
                       ~(axis_scaler(., y1.lim, y2.lim)), # 軸スケーリング
                       breaks=seq(0, 1, 0.2), # 第2軸の目盛り
                       name="累積比（折れ線）")  # y2のラベルはここで設定する
  ) +
  geom_bar(aes(y = SC_count, fill=SC_name), stat = "identity") +
  # labs(x = "現象名称", y = "EM件数", title = "Metis-MF3 AMUR-C中綴じ EM現象別件数") +
  labs(x = "現象名称", y = "EM件数", title = str_c("Metis-MF3 AMUR-C中綴じ EM現象別件数","　EM総数:",EM_number.AMUR_C,"件")) +
  theme_bw() +
  theme(title = element_text(size = 12), text = element_text(size = 12)) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  geom_path(aes(y = variable_scaler(cumrate, y1.lim, y2.lim), group = 0), colour="blue") + 
  geom_point(aes(y = variable_scaler(cumrate, y1.lim, y2.lim)), colour="blue") +
  geom_text(aes(SC_name, SC_count+2, label = round(SC_count,2)),color="black", size = 3)
g_plate_AMUR_C

# グラフ保存
ggsave("./PDF/Metis-MF3_AMUR_C中綴じ_EM現象別件数パレート図.pdf", 
       plot = g_plate_AMUR_C, device = cairo_pdf, dpi=300, width=10, height=6)

# 製造月別グラフ
# 現象件数 
g_AMUR_C_SC_Manufacturing_date <- 
  geom_bar_SCname(df = "AMUR_C_SCname_Manufacturing_date_1",
                  x = "Manufacturing_date",
                  y = "Peripheral_name_SC_sum", 
                  fill = "SC_name_num",
                  legend_name = "現象SC名称",
                  x_labels = "製造月",
                  y_labels = "件数",
                  title = "Metis-MF3 AMUR_C中綴じ EMにおける現象件数 (2019.2～2022.11)",
                  file_path = "./PDF/Metis-MF3-AMUR_C中綴じ製造月別EMの現象件数(2019.2～2022.11).pdf",
                  f_wrap = TRUE, ncol = 5,
                  g_save = TRUE, w = 16, h = 14,
                  # g_save = F, w = 30, h = 15,
                  legend_position = c(0.77, 0.97),
                  legend_justification = c(0, 1))
# 現象比率
g_AMUR_C_SC_Manufacturing_date_rate <- 
  geom_bar_SCname(df = "AMUR_C_SCname_Manufacturing_date_1",
                  x = "Manufacturing_date",
                  y = "SC_sum_rate", 
                  fill = "SC_name_num",
                  legend_name = "現象SC名称",
                  x_labels = "製造月",
                  y_labels = "台当たり比率（件数/製造台数）",
                  title = "Metis-MF3 AMUR_C中綴じ EMにおける現象比率 (2019.2～2022.11)",
                  file_path = "./PDF/Metis-MF3-AMUR_C中綴じ製造月別EMの現象比率(2019.2～2022.11).pdf",
                  f_wrap = TRUE, ncol = 5,
                  g_save = TRUE, w = 16, h = 14,
                  # g_save = F, w = 30, h = 15,
                  legend_position = c(0.77, 0.97),
                  legend_justification = c(0, 1))

# AMUR-C(HY) 製造月別EM件数 -----------------------------------
AMUR_CHY_SCname_Manufacturing_date_1 <- 
  Create_Metis_MF3_SCname(
    T_location = "ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部",
    P_name = "AMUR-C(HY)",
    Choices = "Manufacturing_date",
    # Choices = "Working_month",
    num = 1
  )

AMUR_CHY_SCname_Manufacturing_date_5 <- 
  Create_Metis_MF3_SCname(
    T_location = "ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部",
    P_name = "AMUR-C(HY)",
    Choices = "Manufacturing_date",
    # Choices = "Working_month",
    num = 5
  )

# EM総数　176 299
sum(AMUR_CHY_SCname_Manufacturing_date_1$Peripheral_name_SC_sum)
sum(AMUR_CHY_SCname_Manufacturing_date_5$Peripheral_name_SC_sum)

AMUR_CHY_SCname_Manufacturing_date_1 %>% 
  distinct(SC_name_num)
AMUR_CHY_SCname_Manufacturing_date_1$SC_name_num

print(n=51,AMUR_CHY_SCname_Manufacturing_date_5 %>% 
        distinct(SC_name_num))

# レベル変更
AMUR_CHY_SCname_Manufacturing_date_5 <- 
  AMUR_CHY_SCname_Manufacturing_date_5 %>%
  mutate(
    SC_name_num = factor(SC_name_num, levels = c("SC722",      
                                                 "用紙詰まり", 
                                                 "部品外れ" , 
                                                 "その他" ))
  )

# 結合
AMUR_CHY_SCname_Manufacturing_date_1 <- 
  AMUR_CHY_SCname_Manufacturing_date_1 %>% 
  left_join(Metis_MF3_FIN_number_by_Manufacturing_date, by="Manufacturing_date")

AMUR_CHY_SCname_Manufacturing_date_5 <- 
  AMUR_CHY_SCname_Manufacturing_date_5 %>% 
  left_join(Metis_MF3_FIN_number_by_Manufacturing_date, by="Manufacturing_date")

# 現象件数を台数比率へ換算した列追加
AMUR_CHY_SCname_Manufacturing_date_1 <- 
  AMUR_CHY_SCname_Manufacturing_date_1 %>% 
  mutate(
    SC_sum_rate = Peripheral_name_SC_sum/`AMUR-C(HY)`
  )

AMUR_CHY_SCname_Manufacturing_date_5 <- 
  AMUR_CHY_SCname_Manufacturing_date_5 %>% 
  mutate(
    SC_sum_rate = Peripheral_name_SC_sum/`AMUR-C(HY)`
  )

# SC現象別件数
SC_count_AMUR_CHY <- 
  AMUR_CHY_SCname_Manufacturing_date_1 %>% 
  group_by(SC_name_num) %>% 
  summarise(
    SC_count = sum(Peripheral_name_SC_sum)
  ) %>%
  arrange(-SC_count)
SC_count_AMUR_CHY <- 
  SC_count_AMUR_CHY %>% 
  mutate(
    rate = SC_count/sum(SC_count),
    cum = cumsum(SC_count), 
    cumrate = cum/sum(SC_count),
    SC_name = forcats::as_factor(SC_name_num)
  )
EM_number.AMUR_CHY <- 
  sum(SC_count_AMUR_CHY$SC_count) #176

y1.lim <- c(0,100)
y2.lim <- c(0,1)
g_plate_AMUR_CHY <- 
  ggplot(SC_count_AMUR_CHY, aes(reorder(x = SC_name, X = -SC_count))) + 
  scale_y_continuous(limit=y1.lim,  # 第1軸の範囲
                     breaks=seq(0, 100, 10),  # 第1軸の目盛り
                     sec.axis=sec_axis(
                       ~(axis_scaler(., y1.lim, y2.lim)), # 軸スケーリング
                       breaks=seq(0, 1, 0.2), # 第2軸の目盛り
                       name="累積比（折れ線）")  # y2のラベルはここで設定する
  ) +
  geom_bar(aes(y = SC_count, fill=SC_name), stat = "identity") +
  # labs(x = "現象名称", y = "EM件数", title = "Metis-MF3 AMUR-C(HY) EM現象別件数") +
  labs(x = "現象名称", y = "EM件数", title = str_c("Metis-MF3 AMUR-C(HY) EM現象別件数","　EM総数:",EM_number.AMUR_CHY,"件")) +
  theme_bw() +
  theme(title = element_text(size = 12), text = element_text(size = 12)) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  geom_path(aes(y = variable_scaler(cumrate, y1.lim, y2.lim), group = 0), colour="blue") + 
  geom_point(aes(y = variable_scaler(cumrate, y1.lim, y2.lim)), colour="blue") +
  geom_text(aes(SC_name, SC_count+1, label = round(SC_count,2)),color="black", size = 3)
g_plate_AMUR_CHY

# グラフ保存
ggsave("./PDF/Metis-MF3_AMUR_C(HY)_EM現象別件数パレート図.pdf", 
       plot = g_plate_AMUR_CHY, device = cairo_pdf, dpi=300, width=7, height=6)

# 製造月別グラフ
# 現象件数 
g_AMUR_CHYHY_SC_Manufacturing_date <- 
  geom_bar_SCname(df = "AMUR_CHY_SCname_Manufacturing_date_1",
                  x = "Manufacturing_date",
                  y = "Peripheral_name_SC_sum", 
                  fill = "SC_name_num",
                  legend_name = "現象SC名称",
                  x_labels = "製造月",
                  y_labels = "件数",
                  title = "Metis-MF3 AMUR_C(HY) EMにおける現象件数 (2019.2～2022.11)",
                  file_path = "./PDF/Metis-MF3-AMUR_C(HY)製造月別EMの現象件数(2019.2～2022.11).pdf",
                  f_wrap = TRUE, ncol = 5,
                  g_save = TRUE, w = 16, h = 10,
                  # g_save = F, w = 30, h = 15,
                  legend_position = c(0.77, 0.97),
                  legend_justification = c(0, 1))
# 現象比率
g_AMUR_CHYHY_SC_Manufacturing_date_rate <- 
  geom_bar_SCname(df = "AMUR_CHY_SCname_Manufacturing_date_1",
                  x = "Manufacturing_date",
                  y = "SC_sum_rate", 
                  fill = "SC_name_num",
                  legend_name = "現象SC名称",
                  x_labels = "製造月",
                  y_labels = "台当たり比率（件数/製造台数）",
                  title = "Metis-MF3 AMUR_C(HY) EMにおける現象比率 (2019.2～2022.11)",
                  file_path = "./PDF/Metis-MF3-AMUR_C(HY)製造月別EMの現象比率(2019.2～2022.11).pdf",
                  f_wrap = TRUE, ncol = 5,
                  g_save = TRUE, w = 16, h = 10,
                  # g_save = F, w = 30, h = 15,
                  legend_position = c(0.77, 0.97),
                  legend_justification = c(0, 1))
