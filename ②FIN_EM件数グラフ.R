####EM件数グラフ####

# データ読込
Metis_MF3_2211 <- read_tsv("./tsv_data/Metis_MF3_2211.tsv")

names(Metis_MF3_2211)

# 処置日毎のEM数の抽出
Metis_MF3_EM_VOLGA_E_by_Maintenance_date <- 
  Metis_MF3_2211 %>% 
  filter(Treatment_location == "ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部") %>%
  filter(Peripheral_name == "VOLGA-E") %>%
  group_by(Maintenance_date) %>% 
  summarise(
    VOLGA_E_EM_F = n()
  )

sum(Metis_MF3_EM_VOLGA_E_by_Maintenance_date$VOLGA_E_EM_F) # 1205　1734

Metis_MF3_EM_VOLGA_E_P_by_Maintenance_date <- 
  Metis_MF3_2211 %>% 
  filter(Treatment_location == "ﾊﾟﾝﾁ部") %>%
  filter(Peripheral_name == "VOLGA-E") %>%
  group_by(Maintenance_date) %>% 
  summarise(
    VOLGA_E_EM_P = n()
  )

sum(Metis_MF3_EM_VOLGA_E_P_by_Maintenance_date$VOLGA_E_EM_P) # 13　19

Metis_MF3_EM_VOLGA_E_S_by_Maintenance_date <- 
  Metis_MF3_2211 %>% 
  filter(Treatment_location == "ｽﾃｰﾌﾟﾙ部") %>%
  filter(Peripheral_name == "VOLGA-E") %>%
  group_by(Maintenance_date) %>% 
  summarise(
    VOLGA_E_EM_S = n()
  )

sum(Metis_MF3_EM_VOLGA_E_S_by_Maintenance_date$VOLGA_E_EM_S) # 40　64

# 結合
Metis_MF3_EM_VOLGA_E_all_by_Maintenance_date <- 
  Metis_MF3_EM_VOLGA_E_by_Maintenance_date %>% 
  full_join(Metis_MF3_EM_VOLGA_E_P_by_Maintenance_date, by="Maintenance_date") %>% 
  full_join(Metis_MF3_EM_VOLGA_E_S_by_Maintenance_date, by="Maintenance_date") %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(
    VOLGA_E_EM = VOLGA_E_EM_F + VOLGA_E_EM_P + VOLGA_E_EM_S,
    Finisher = "VOLGA_E"
  ) %>% 
  select(Maintenance_date, VOLGA_E_EM)
# 結合
X_date <- seq(as.Date("2019-02-01"), as.Date("2022-11-30"), by = "day")
VOLGA_EM_date <- tibble(Maintenance_date = X_date)
VOLGA_EM_date <- 
  VOLGA_EM_date %>% 
  full_join(Metis_MF3_EM_VOLGA_E_all_by_Maintenance_date, by="Maintenance_date") %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

# AMUR-C(HY)
Metis_MF3_EM_AMUR_CHY_by_Maintenance_date <- 
  Metis_MF3_2211 %>% 
  filter(Treatment_location == "ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部") %>%
  filter(Peripheral_name == "AMUR-C(HY)") %>%
  group_by(Maintenance_date) %>% 
  summarise(
    AMUR_CHY_EM_F = n()
  )

sum(Metis_MF3_EM_AMUR_CHY_by_Maintenance_date$AMUR_CHY_EM_F) # 176　299

Metis_MF3_EM_AMUR_CHY_P_by_Maintenance_date <- 
  Metis_MF3_2211 %>% 
  filter(Treatment_location == "ﾊﾟﾝﾁ部") %>%
  filter(Peripheral_name == "AMUR-C(HY)") %>%
  group_by(Maintenance_date) %>% 
  summarise(
    AMUR_CHY_EM_P = n()
  )
sum(Metis_MF3_EM_AMUR_CHY_P_by_Maintenance_date$AMUR_CHY_EM_P) # 7　7

Metis_MF3_EM_AMUR_CHY_S_by_Maintenance_date <- 
  Metis_MF3_2211 %>% 
  filter(Treatment_location == "ｽﾃｰﾌﾟﾙ部") %>%
  filter(Peripheral_name == "AMUR-C(HY)") %>%
  group_by(Maintenance_date) %>% 
  summarise(
    AMUR_CHY_EM_S = n()
  )

sum(Metis_MF3_EM_AMUR_CHY_S_by_Maintenance_date$AMUR_CHY_EM_S) # 9　11

# 結合
Metis_MF3_EM_AMUR_CHY_all_by_Maintenance_date <- 
  Metis_MF3_EM_AMUR_CHY_by_Maintenance_date %>% 
  full_join(Metis_MF3_EM_AMUR_CHY_P_by_Maintenance_date, by="Maintenance_date") %>% 
  full_join(Metis_MF3_EM_AMUR_CHY_S_by_Maintenance_date, by="Maintenance_date") %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(
    AMUR_CHY_EM = AMUR_CHY_EM_F + AMUR_CHY_EM_P + AMUR_CHY_EM_S,
    Finisher = "AMUR_CHY"
  ) %>% 
  select(Maintenance_date, AMUR_CHY_EM)
# 結合
X_date <- seq(as.Date("2019-02-01"), as.Date("2022-11-30"), by = "day")
AMUR_CHY_EM_date <- tibble(Maintenance_date = X_date)
AMUR_CHY_EM_date <- 
  AMUR_CHY_EM_date %>% 
  full_join(Metis_MF3_EM_AMUR_CHY_all_by_Maintenance_date, by="Maintenance_date") %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))


# AMUR-C中綴じ
Metis_MF3_EM_AMUR_C_by_Maintenance_date <- 
  Metis_MF3_2211 %>% 
  filter(Treatment_location == "ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部") %>%
  filter(Peripheral_name == "AMUR-C中綴じ") %>%
  group_by(Maintenance_date) %>% 
  summarise(
    AMUR_C_EM_F = n()
  )

sum(Metis_MF3_EM_AMUR_C_by_Maintenance_date$AMUR_C_EM_F) # 518　799

Metis_MF3_EM_AMUR_C_P_by_Maintenance_date <- 
  Metis_MF3_2211 %>% 
  filter(Treatment_location == "ﾊﾟﾝﾁ部") %>%
  filter(Peripheral_name == "AMUR-C中綴じ") %>%
  group_by(Maintenance_date) %>% 
  summarise(
    AMUR_C_EM_P = n()
  )
sum(Metis_MF3_EM_AMUR_C_P_by_Maintenance_date$AMUR_C_EM_P) # 17　27

Metis_MF3_EM_AMUR_C_S_by_Maintenance_date <- 
  Metis_MF3_2211 %>% 
  filter(Treatment_location == "ｽﾃｰﾌﾟﾙ部") %>%
  filter(Peripheral_name == "AMUR-C中綴じ") %>%
  group_by(Maintenance_date) %>% 
  summarise(
    AMUR_C_EM_S = n()
  )

sum(Metis_MF3_EM_AMUR_C_S_by_Maintenance_date$AMUR_C_EM_S) # 70　116

# 結合
Metis_MF3_EM_AMUR_C_all_by_Maintenance_date <- 
  Metis_MF3_EM_AMUR_C_by_Maintenance_date %>% 
  full_join(Metis_MF3_EM_AMUR_C_P_by_Maintenance_date, by="Maintenance_date") %>% 
  full_join(Metis_MF3_EM_AMUR_C_S_by_Maintenance_date, by="Maintenance_date") %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(
    AMUR_C_EM = AMUR_C_EM_F + AMUR_C_EM_P + AMUR_C_EM_S,
    Finisher = "AMUR_C"
  ) %>% 
  select(Maintenance_date, AMUR_C_EM)
# 結合
X_date <- seq(as.Date("2019-02-01"), as.Date("2022-11-30"), by = "day")
AMUR_C_EM_date <- tibble(Maintenance_date = X_date)
AMUR_C_EM_date <- 
  AMUR_C_EM_date %>% 
  full_join(Metis_MF3_EM_AMUR_C_all_by_Maintenance_date, by="Maintenance_date") %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))


# 全体結合
AMUR_C_EM_date <-
  AMUR_C_EM_date %>% 
  full_join(AMUR_CHY_EM_date, by=c("Maintenance_date"))

Metis_MF3_EM_FIN_by_Maintenance_date <-
  VOLGA_EM_date %>% 
  full_join(AMUR_C_EM_date, by=c("Maintenance_date"))

# ファイル保存
# write_tsv(Metis_MF3_EM_FIN_by_Maintenance_date, "./tsv_data/Metis_MF3_EM_FIN_by_Maintenance_date.tsv")

# ピボット変換
Metis_MF3_EM_FIN_by_Maintenance_date_pivot <- 
  pivot_longer(data = Metis_MF3_EM_FIN_by_Maintenance_date, cols = ends_with("EM")) 

# 列要素
Metis_MF3_EM_FIN_by_Maintenance_date_pivot %>% 
  distinct(name)

# # 台数昇順にレベル変更
# VOLGA_EM_date_pivot$name <- 
#   factor(VOLGA_EM_date_pivot$name, 
#          levels=c("VOLGA_E_EM"))

# フォント設定
# windowsFonts(Japan1GothicBBB = windowsFont("Japan1GothicBBB"))
par(family="Noto Sans")

# 折れ線グラフ
Att.labs <- c("VOLGA-E", "AMUR-C中綴じ", "AMUR-C(HY)")
names(Att.labs) <- c("VOLGA_E_EM", "AMUR_C_EM", "AMUR_CHY_EM")
g_EM_FIN <- 
  ggplot(data = Metis_MF3_EM_FIN_by_Maintenance_date_pivot, 
         mapping = aes(x=Maintenance_date,y=value,colour=name)) +
  geom_line(linewidth = 0.8) +
  theme_bw() + 
  theme(text = element_text(size = 16), title = element_text(size = 14)) +
  # theme(legend.position = c(0.05, 0.98), legend.justification = c(0, 1)) +
  # scale_color_hue(name = "機種", 
  #                 labels = c(VOLGA_E_EM="VOLGA-E",AMUR_C_EM="AMUR-C中綴じ",AMUR_CHY_EM="AMUR-C(HY)")) +
  labs(x = "年月日",y = "件数", title = "Metis-MF3_Finisher EM件数") +
  scale_x_date(breaks = seq(as.Date("2019-02-01"), as.Date("2022-11-01"), by="6 month"),labels=date_format("%Y/%m")) +
  scale_y_continuous(breaks=seq(0,10,2)) +
  coord_cartesian(ylim = c(0, 10)) +
  theme(legend.position = "none") +
  theme(strip.background = element_blank(), strip.text = element_text(size = 16)) +
  facet_wrap(~name, ncol = 1, labeller = labeller(name = Att.labs))

# グラフ保存
file_path='./PDF/Metis-MF3_FIN_EM-dayly件数.pdf'
ggsave(file=file_path, device=cairo_pdf, plot=g_EM_FIN, dpi=300, w=10, h=6)

# ヒストグラム
Att.labs <- c("VOLGA-E", "AMUR-C中綴じ", "AMUR-C(HY)")
names(Att.labs) <- c("VOLGA_E_EM", "AMUR_C_EM", "AMUR_CHY_EM")
g_EM_hist_FIN <- 
  ggplot(data = Metis_MF3_EM_FIN_by_Maintenance_date_pivot,
         mapping = aes(x = value, fill = name)) +
  geom_histogram(bins = 10, colour = "black",
                 position = position_dodge(), alpha = 0.8) +
  geom_text(aes(y = after_stat(count) + 80, label = after_stat(count)), stat = "bin", binwidth = 1, size = 5) +
  facet_wrap(~name, ncol = 1, labeller = labeller(name = Att.labs)) +
  labs(x = "EM回数", y = "count", fill = "機種", 
       title = "Metis-MF3 Finisher 日毎のEM回数(2019.02～2022.11)") +
  # scale_fill_hue(labels = c(VOLGA_E="VOLGA-E",AMUR_CHY="AMUR-C(HY)",AMUR_C="AMUR-C")) +
  # theme(legend.position = c(0.9,0.9), legend.justification = c(1,1)) +
  theme_bw() + 
  theme(text = element_text(size = 16), title = element_text(size = 14)) +
  scale_y_continuous(breaks = seq(0, 1400, 200)) +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  guides(fill = "none") +
  theme(strip.background = element_blank(), strip.text = element_text(size = 16)) 

# グラフ保存
file_path='./PDF/Metis-MF3_FIN_Eachday_EMcount.pdf'
ggsave(file=file_path, device=cairo_pdf, plot=g_EM_hist_FIN, dpi=300, w=8, h=8)
