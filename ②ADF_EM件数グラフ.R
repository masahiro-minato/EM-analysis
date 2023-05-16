# ファイル読込
Metis_MF3_2211 <- read_tsv("./tsv_data/Metis_MF3_2211.tsv") # A tibble: 516,414 × 19
Metis_sheet3_2211 <- read_tsv("./tsv_data/Metis_sheet3_2211.tsv")

# フォント設定
# windowsFonts(Japan1GothicBBB = windowsFont("Japan1GothicBBB"))
par(family="Noto Sans")

names(Metis_MF3_2211)
print(n =100,Metis_MF3_2211 %>% 
  distinct(Treatment_location))

# 処置日毎のEM数の抽出
Metis_MF3_EM_COOK_C_by_Maintenance_date <- 
  Metis_MF3_2211 %>% 
  filter(Treatment_location == "ADF部") %>%
  filter(Peripheral_name == "COOK-C") %>%
  group_by(Maintenance_date) %>% 
  summarise(
    COOK_C_EM = n()
  )

sum(Metis_MF3_EM_COOK_C_by_Maintenance_date$COOK_C_EM) # 14724

Metis_MF3_EM_SINAI_H_by_Maintenance_date <- 
  Metis_MF3_2211 %>% 
  filter(Treatment_location == "ADF部") %>%
  filter(Peripheral_name == "SINAI-H") %>%
  group_by(Maintenance_date) %>% 
  summarise(
    SINAI_H_EM = n()
  )

sum(Metis_MF3_EM_SINAI_H_by_Maintenance_date$SINAI_H_EM) # 3383

# 結合
X_date <- seq(as.Date("2019-02-01"), as.Date("2022-11-30"), by = "day")
ADF_EM_date <- tibble(Maintenance_date = X_date)
Metis_MF3_EM_ADF_by_Maintenance_date <- 
  ADF_EM_date %>% 
  full_join(Metis_MF3_EM_COOK_C_by_Maintenance_date, by="Maintenance_date") %>% 
  full_join(Metis_MF3_EM_SINAI_H_by_Maintenance_date, by="Maintenance_date") %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

# ファイル保存
# write_tsv(Metis_MF3_EM_ADF_by_Maintenance_date, "./tsv_data/Metis_MF3_EM_ADF_by_Maintenance_date.tsv")

# ピボット変換
Metis_MF3_EM_ADF_by_Maintenance_date_pivot <- 
  pivot_longer(data = Metis_MF3_EM_ADF_by_Maintenance_date, cols = ends_with("EM")) 

# 列要素
Metis_MF3_EM_ADF_by_Maintenance_date_pivot %>% 
  distinct(name)

# 台数昇順にレベル変更
Metis_MF3_EM_ADF_by_Maintenance_date_pivot$name <- 
  factor(Metis_MF3_EM_ADF_by_Maintenance_date_pivot$name, 
         levels=c("COOK_C_EM","SINAI_H_EM"))

# フォント設定
# windowsFonts(Japan1GothicBBB = windowsFont("Japan1GothicBBB"))
par(family="Noto Sans")

# 折れ線グラフ
Att.labs <- c("COOK-C", "SINAI-H")
names(Att.labs) <- c("COOK_C_EM", "SINAI_H_EM")
g_EM_ADF <- 
  ggplot(data = Metis_MF3_EM_ADF_by_Maintenance_date_pivot, 
         mapping = aes(x=Maintenance_date,y=value,colour=name)) +
  geom_line(linewidth = 0.8) +
  theme_bw() + 
  theme(text = element_text(size = 16), title = element_text(size = 14)) +
  # theme(legend.position = c(0.05, 0.98), legend.justification = c(0, 1)) +
  # scale_color_hue(name = "機種", 
  #                 labels = c(VOLGA_E_EM="VOLGA-E",AMUR_C_EM="AMUR-C中綴じ",AMUR_CHY_EM="AMUR-C(HY)")) +
  labs(x = "年月日",y = "件数", title = "Metis-MF3_ADF EM件数") +
  scale_x_date(breaks = seq(as.Date("2019-02-01"), as.Date("2022-11-01"), by="6 month"),labels=date_format("%Y/%m")) +
  # scale_y_continuous(breaks=seq(0,10,2)) +
  # coord_cartesian(ylim = c(0, 10)) +
  theme(legend.position = "none") +
  theme(strip.background = element_blank(), strip.text = element_text(size = 16)) +
  facet_wrap(~name, ncol = 1, labeller = labeller(name = Att.labs))

# グラフ保存
file_path='./VOLGA/PDF/Metis-MF3_ADF_EM-dayly件数.pdf'
ggsave(file=file_path, device=cairo_pdf, plot=g_EM_ADF, dpi=300, w=10, h=6)

# ヒストグラム
Att.labs <- c("COOK-C", "SINAI-H")
names(Att.labs) <- c("COOK_C_EM", "SINAI_H_EM")
g_EM_hist_ADF <- 
  ggplot(data = Metis_MF3_EM_ADF_by_Maintenance_date_pivot,
         mapping = aes(x = value, fill = name)) +
  geom_histogram(bins = 46, colour = "black",
                 position = position_dodge(), alpha = 0.8) +
  geom_text(aes(y = after_stat(count) + 20, label = after_stat(count)), stat = "bin", binwidth = 1, size = 4) +
  facet_wrap(~name, ncol = 1, labeller = labeller(name = Att.labs)) +
  labs(x = "EM回数", y = "count", fill = "機種", 
       title = "Metis-MF3 ADF 日毎のEM回数(2019.02～2022.11)") +
  # scale_fill_hue(labels = c(VOLGA_E="VOLGA-E",AMUR_CHY="AMUR-C(HY)",AMUR_C="AMUR-C")) +
  # theme(legend.position = c(0.9,0.9), legend.justification = c(1,1)) +
  theme_bw(base_family = "Japan1GothicBBB") + 
  theme(text = element_text(size = 16), title = element_text(size = 14)) +
  scale_y_continuous(breaks = seq(0, 520, 100)) +
  scale_x_continuous(breaks = seq(0, 45, 5)) +
  guides(fill = "none") +
  theme(strip.background = element_blank(), strip.text = element_text(size = 16)) 

# グラフ保存
file_path='./PDF/Metis-MF3_ADF_Eachday_EMcount.pdf'
ggsave(file=file_path, device=cairo_pdf, plot=g_EM_hist_ADF, dpi=300, w=12, h=8)
