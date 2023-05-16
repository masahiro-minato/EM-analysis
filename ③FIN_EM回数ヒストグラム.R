####ファイル読込####
options(readr.show_col_types = FALSE)
Metis_MF3_distinct <- read_tsv("./tsv_data/Metis_MF3_distinct.tsv")
Metis_sheet3 <- read_tsv("./tsv_data/Metis_sheet3.tsv")

# 22年11月までのデータ
Metis_MF3_2211 <- read_tsv("./tsv_data/Metis_MF3_2211.tsv")
Metis_sheet3_2211 <- read_tsv("./tsv_data/Metis_sheet3_2211.tsv")

df_tmp_Peripheral <- 
  Metis_MF3_distinct %>% 
  distinct(Peripheral_name)
df_tmp_Treatment <- 
  Metis_MF3_distinct %>% 
  distinct(Treatment_location)

# ヒストグラム
Finisher_EM_histogram(
    df_EM = Metis_MF3_2211,
    df_machine = Metis_sheet3_2211,
    graph_save = TRUE,
    y.breaks = seq(0, 18000, 2000),
    file_path = './PDF/Metis-MF3_Finisher_EMcount_2207-2.pdf',
    grapf_title = "Metis-MF3 Finisher EM回数(2019.02～2022.11)"
)

Metis_MF3_EM_Finisher_by_Machine_numbers %>% 
  distinct(Machine_numbers)

sum(Metis_MF3_EM_Finisher_by_Machine_numbers$VOLGA_E_EM, na.rm=TRUE) #1817 ⇒機器データ基準だと1749
length(Metis_MF3_EM_Finisher_by_Machine_numbers$VOLGA_E_EM) #2369　NA含む
# NAを除くと1430台
y <- Metis_MF3_EM_Finisher_by_Machine_numbers$VOLGA_E_EM[!is.na(Metis_MF3_EM_Finisher_by_Machine_numbers$VOLGA_E_EM)]
length(y) #1430

#### 以下は関数を使わない場合 -------------------
# 機番毎のEM数の抽出
Metis_MF3_EM_VOLGA_E_by_Machine_numbers <- 
  Metis_MF3_2211 %>% 
  filter(Treatment_location == "ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部") %>%
  filter(Peripheral_name == "VOLGA-E") %>%
  group_by(Machine_numbers) %>% 
  summarise(
    VOLGA_E_EM = n()
  )

sum(Metis_MF3_EM_VOLGA_E_by_Machine_numbers$VOLGA_E_EM) # 1205　1734

Metis_MF3_EM_VOLGA_E_P_by_Machine_numbers <- 
  Metis_MF3_2211 %>% 
  filter(Treatment_location == "ﾊﾟﾝﾁ部") %>%
  filter(Peripheral_name == "VOLGA-E") %>%
  group_by(Machine_numbers) %>% 
  summarise(
    VOLGA_E_EM_P = n()
  )

sum(Metis_MF3_EM_VOLGA_E_P_by_Machine_numbers$VOLGA_E_EM_P) # 13　19

Metis_MF3_EM_VOLGA_E_S_by_Machine_numbers <- 
  Metis_MF3_2211 %>% 
  filter(Treatment_location == "ｽﾃｰﾌﾟﾙ部") %>%
  filter(Peripheral_name == "VOLGA-E") %>%
  group_by(Machine_numbers) %>% 
  summarise(
    VOLGA_E_EM_S = n()
  )

sum(Metis_MF3_EM_VOLGA_E_S_by_Machine_numbers$VOLGA_E_EM_S) # 40　64

# 結合
Metis_MF3_EM_VOLGA_E_all_by_Machine_numbers <- 
  Metis_MF3_EM_VOLGA_E_by_Machine_numbers %>% 
  full_join(Metis_MF3_EM_VOLGA_E_P_by_Machine_numbers, by="Machine_numbers") %>% 
  full_join(Metis_MF3_EM_VOLGA_E_S_by_Machine_numbers, by="Machine_numbers") %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(
    VOLGA_E_EM = VOLGA_E_EM + VOLGA_E_EM_P + VOLGA_E_EM_S,
    Finisher = "VOLGA_E"
  )

# AMUR-C(HY)
Metis_MF3_EM_AMUR_CHY_by_Machine_numbers <- 
  Metis_MF3_2211 %>% 
  filter(Treatment_location == "ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部") %>%
  filter(Peripheral_name == "AMUR-C(HY)") %>%
  group_by(Machine_numbers) %>% 
  summarise(
    AMUR_CHY_EM = n()
  )

sum(Metis_MF3_EM_AMUR_CHY_by_Machine_numbers$AMUR_CHY_EM) # 176　299

Metis_MF3_EM_AMUR_CHY_P_by_Machine_numbers <- 
  Metis_MF3_2211 %>% 
  filter(Treatment_location == "ﾊﾟﾝﾁ部") %>%
  filter(Peripheral_name == "AMUR-C(HY)") %>%
  group_by(Machine_numbers) %>% 
  summarise(
    AMUR_CHY_EM_P = n()
  )
sum(Metis_MF3_EM_AMUR_CHY_P_by_Machine_numbers$AMUR_CHY_EM_P) # 7　7

Metis_MF3_EM_AMUR_CHY_S_by_Machine_numbers <- 
  Metis_MF3_2211 %>% 
  filter(Treatment_location == "ｽﾃｰﾌﾟﾙ部") %>%
  filter(Peripheral_name == "AMUR-C(HY)") %>%
  group_by(Machine_numbers) %>% 
  summarise(
    AMUR_CHY_EM_S = n()
  )

sum(Metis_MF3_EM_AMUR_CHY_S_by_Machine_numbers$AMUR_CHY_EM_S) # 9　11

# 結合
Metis_MF3_EM_AMUR_CHY_all_by_Machine_numbers <- 
  Metis_MF3_EM_AMUR_CHY_by_Machine_numbers %>% 
  full_join(Metis_MF3_EM_AMUR_CHY_P_by_Machine_numbers, by="Machine_numbers") %>% 
  full_join(Metis_MF3_EM_AMUR_CHY_S_by_Machine_numbers, by="Machine_numbers") %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(
    AMUR_CHY_EM = AMUR_CHY_EM + AMUR_CHY_EM_P + AMUR_CHY_EM_S,
    Finisher = "AMUR_CHY"
  )

# AMUR-C中綴じ
Metis_MF3_EM_AMUR_C_by_Machine_numbers <- 
  Metis_MF3_2211 %>% 
  filter(Treatment_location == "ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部") %>%
  filter(Peripheral_name == "AMUR-C中綴じ") %>%
  group_by(Machine_numbers) %>% 
  summarise(
    AMUR_C_EM = n()
  )

sum(Metis_MF3_EM_AMUR_C_by_Machine_numbers$AMUR_C_EM) # 518　799

Metis_MF3_EM_AMUR_C_P_by_Machine_numbers <- 
  Metis_MF3_2211 %>% 
  filter(Treatment_location == "ﾊﾟﾝﾁ部") %>%
  filter(Peripheral_name == "AMUR-C中綴じ") %>%
  group_by(Machine_numbers) %>% 
  summarise(
    AMUR_C_EM_P = n()
  )
sum(Metis_MF3_EM_AMUR_C_P_by_Machine_numbers$AMUR_C_EM_P) # 17　27

Metis_MF3_EM_AMUR_C_S_by_Machine_numbers <- 
  Metis_MF3_2211 %>% 
  filter(Treatment_location == "ｽﾃｰﾌﾟﾙ部") %>%
  filter(Peripheral_name == "AMUR-C中綴じ") %>%
  group_by(Machine_numbers) %>% 
  summarise(
    AMUR_C_EM_S = n()
  )

sum(Metis_MF3_EM_AMUR_C_S_by_Machine_numbers$AMUR_C_EM_S) # 70　116

# 結合
Metis_MF3_EM_AMUR_C_all_by_Machine_numbers <- 
  Metis_MF3_EM_AMUR_C_by_Machine_numbers %>% 
  full_join(Metis_MF3_EM_AMUR_C_P_by_Machine_numbers, by="Machine_numbers") %>% 
  full_join(Metis_MF3_EM_AMUR_C_S_by_Machine_numbers, by="Machine_numbers") %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(
    AMUR_C_EM = AMUR_C_EM + AMUR_C_EM_P + AMUR_C_EM_S,
    Finisher = "AMUR_C"
  )

tmp <- 
  Metis_MF3_2211 %>% 
  filter(Machine_numbers == "302A615997")

# 全体結合
Metis_MF3_EM_AMUR_by_Machine_numbers <-
  Metis_MF3_EM_AMUR_CHY_all_by_Machine_numbers %>% 
  full_join(Metis_MF3_EM_AMUR_C_all_by_Machine_numbers, by=c("Machine_numbers","Finisher"))

Metis_MF3_EM_AMUR_by_Machine_numbers %>% 
  distinct_all()
names(Metis_MF3_EM_AMUR_by_Machine_numbers)

Metis_MF3_EM_Finisher_by_Machine_numbers <-
  Metis_MF3_EM_VOLGA_E_all_by_Machine_numbers %>% 
  full_join(Metis_MF3_EM_AMUR_by_Machine_numbers, by=c("Machine_numbers","Finisher")) %>% 
  select(Machine_numbers, Finisher, AMUR_C_EM, AMUR_CHY_EM, VOLGA_E_EM)

# # FinisherのEM数カウント
# Metis_MF3_EM_Finisher_by_Machine_numbers <- 
#   Finisher_EMcount(Metis_MF3_distinct)
# 
# # One-hot化
# Metis_MF3_by_model <-
#   One_hot_Peripheral_machine(df=Metis_sheet3)

# 納品全台数情報であるMetis_sheet3から、必要列を抽出
Metis_MF3_Count <-
  Metis_sheet3_2211 %>%
  select(機種機番,機器番号,納品年月日,機種略,製造年月,納入月,ACV,ADF,フィニッシャー,LCT,バンク,
         インサーター,スライドソートトレイ,スタッカー,
         トリマー,デカーラ,紙折り,インナー1ビン)
# data型への列変換
Metis_MF3_Count <-
  Date_conversion(df = "Metis_MF3_Count", col_list = c("納品年月日","製造年月","納入月"))

Metis_MF3_by_model <-
  Metis_MF3_Count %>%
  mutate(
    COOK = case_when(
      ADF == "COOK-C" ~ 1,
      ADF == NA ~ 0,
      TRUE ~ 0),
    SINAI = case_when(
      ADF == "SINAI-H" ~ 1,
      ADF == NA ~ 0,
      TRUE ~ 0),
    AMUR_C_HY = case_when(
      フィニッシャー == "AMUR-C(HY)" ~ 1,
      フィニッシャー == NA ~ 0,
      TRUE ~ 0),
    AMUR_C_saddle = case_when(
      フィニッシャー == "AMUR-C中綴じ" ~ 1,
      フィニッシャー == NA ~ 0,
      TRUE ~ 0),
    VOLGA_E = case_when(
      フィニッシャー == "VOLGA-E" ~ 1,
      フィニッシャー == NA ~ 0,
      TRUE ~ 0),
    CANARIA_D = case_when(
      バンク == "CANARIA-D" ~ 1,
      バンク == NA ~ 0,
      TRUE ~ 0),
    GOREE_D = case_when(
      バンク == "GOREE-D" ~ 1,
      バンク == NA ~ 0,
      TRUE ~ 0),
    CUBA_C = case_when(
      バンク == "CUBA-C" ~ 1,
      バンク == NA ~ 0,
      TRUE ~ 0),
    CANARIA_E = case_when(
      バンク == "CANARIA-E" ~ 1,
      バンク == NA ~ 0,
      TRUE ~ 0),
    THAMES_C = case_when(
      紙折り == "THAMES-C" ~ 1,
      紙折り == NA ~ 0,
      TRUE ~ 0)
  )

Metis_MF3_by_model <-
  Metis_MF3_by_model %>%
  rename(Machine_numbers = 機種機番)

# 結合
Metis_MF3_by_model <- 
  Metis_MF3_by_model %>% 
  left_join(Metis_MF3_EM_Finisher_by_Machine_numbers, by="Machine_numbers")

# EM発生無し機の情報を含めてのEM回数
Metis_MF3_Finisher_ENCount_by_model <- 
  Metis_MF3_by_model %>% 
  mutate(
    VOLGA_E_EM_Count = case_when(is.na(VOLGA_E_EM) & VOLGA_E == 1 ~ 0, 
                              !is.na(VOLGA_E_EM)& VOLGA_E == 1 ~ as.double(VOLGA_E_EM),
                              TRUE ~ NaN),
    AMUR_C_EM_Count = case_when(is.na(AMUR_C_EM) & AMUR_C_saddle == 1 ~ 0, 
                               !is.na(AMUR_C_EM)& AMUR_C_saddle == 1 ~ as.double(AMUR_C_EM),
                               TRUE ~ NaN),
    AMUR_CHY_EM_Count = case_when(is.na(AMUR_CHY_EM) & AMUR_C_HY == 1 ~ 0, 
                                !is.na(AMUR_CHY_EM)& AMUR_C_HY == 1 ~ as.double(AMUR_CHY_EM),
                                TRUE ~ NaN)
  ) %>% 
  select(Machine_numbers, VOLGA_E_EM_Count, AMUR_C_EM_Count, AMUR_CHY_EM_Count, Finisher,
         VOLGA_E, AMUR_C_HY, AMUR_C_saddle)

sum(Metis_MF3_Finisher_ENCount_by_model$VOLGA_E)  # 15251　18406
sum(Metis_MF3_Finisher_ENCount_by_model$AMUR_C_saddle) #  7317　8876
sum(Metis_MF3_Finisher_ENCount_by_model$AMUR_C_HY) #  2017　2472
nrow(Metis_MF3_Finisher_ENCount_by_model)      # 130420　160048

# ピボット変換
Metis_MF3_Finisher_ENCount_by_model_pivot <- 
  pivot_longer(data = Metis_MF3_Finisher_ENCount_by_model, cols = ends_with("Count")) %>% 
  subset(!(is.nan(value)))

# フォント設定
# windowsFonts(Japan1GothicBBB = windowsFont("Japan1GothicBBB"))
par(family="Noto Sans")

# ヒストグラム
Att.labs <- c("VOLGA-E", "AMUR-C中綴じ", "AMUR-C(HY)")
names(Att.labs) <- c("VOLGA_E_EM_Count", "AMUR_C_EM_Count", "AMUR_CHY_EM_Count")
g_EMcount_hist <- 
  ggplot(data = Metis_MF3_Finisher_ENCount_by_model_pivot,
         mapping = aes(x = value, fill = name)) +
  geom_histogram(bins = 10, colour = "black",
                 position = position_dodge(), alpha = 0.8) +
  geom_text(aes(y = after_stat(count) + 1000, label = after_stat(count)), stat = "bin", binwidth = 1, size = 5) +
  facet_wrap(~name, ncol = 1, labeller = labeller(name = Att.labs)) +
  labs(x = "EM回数", y = "count", fill = "機種", 
       title = "Metis-MF3 Finisher EM回数(2019.02～2022.11)") +
  # scale_fill_hue(labels = c(VOLGA_E="VOLGA-E",AMUR_CHY="AMUR-C(HY)",AMUR_C="AMUR-C")) +
  theme_bw() + 
  theme(text = element_text(size = 14)) +
  scale_y_continuous(breaks = seq(0, 18000, 2000),labels = label_comma()) +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  theme(legend.position = c(0.9,0.9), legend.justification = c(1,1)) +
  guides(fill = "none") +
  theme(strip.background = element_blank(), strip.text = element_text(size = 15)) 

# グラフ保存
file_path='./PDF/Metis-MF3_FIN_EMcount-2.pdf'
ggsave(file=file_path, device=cairo_pdf, plot=g_EMcount_hist, dpi=300, w=8, h=8)
