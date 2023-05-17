#### CE作業時間のヒストグラムを作成

# ファイル読込
Metis_MF3_2211 <- read_tsv("./tsv_data/Metis_MF3_2211.tsv")
# フォント設定
# windowsFonts(Japan1GothicBBB = windowsFont("Japan1GothicBBB"))
par(family="Noto Sans")

Metis_MF3_2211 %>%
  distinct(Peripheral_name)
print(n=93,Metis_MF3_2211 %>%
  distinct(Treatment_location))

# Finisher CE作業時間の抽出
Metis_MF3_FIN_CE_Working_hours <- 
  Metis_MF3_2211 %>% 
  filter(Treatment_location == "ﾊﾟﾝﾁ部" |
         Treatment_location == "ｽﾃｰﾌﾟﾙ部" |
         Treatment_location == "ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部") %>%
  filter(Peripheral_name == "VOLGA-E"|
         Peripheral_name == "AMUR-C中綴じ"|
         Peripheral_name == "AMUR-C(HY)") %>% 
  select(Machine_numbers, CE_Working_hours, Peripheral_name, Treatment_location)

colnames(Metis_MF3_FIN_CE_Working_hours)

# CE作業時間のヒストグラム
g_ce_work_FIN <- 
  ggplot(data = Metis_MF3_FIN_CE_Working_hours,
         mapping = aes(x = CE_Working_hours, fill=Treatment_location)) +
  geom_histogram(bins = 74, colour = "black") +
  scale_x_continuous(breaks = seq(0, 400, 50)) +
  scale_y_continuous(breaks = seq(0, 300, 50)) +
  labs(x = "CE 作業時間（分）",y = "count", fill = "処置部", title = "Metis-MF3_Finisher CE作業時間(2019.02～2022.11)") +
  theme_bw() + 
  theme(text = element_text(size = 20), title = element_text(size = 14)) +
  facet_wrap(~Peripheral_name, ncol = 1)　+
  theme(legend.position = c(0.9,0.95), legend.justification = c(1,1)) +
  theme(strip.background = element_blank(), strip.text = element_text(size = 15))
g_ce_work_FIN
# グラフ保存
file_path='./PDF/Metis-MF3_FIN_CE_Working_hours.pdf'
ggsave(file=file_path, device=cairo_pdf, plot=g_ce_work_FIN, dpi=300, w=10, h=8)
