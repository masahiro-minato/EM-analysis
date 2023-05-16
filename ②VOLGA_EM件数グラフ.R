####EM件数グラフ####

# データ読込
Metis_MF3_EM_VOLGA <- read_tsv("./tsv_data/Metis_MF3_EM_VOLGA.tsv")

X_date <- seq(as.Date("2019-02-01"), as.Date("2022-11-30"), by = "day")
# X_date <- seq(as.Date("2021-03-01"), as.Date("2022-02-28"), by = "day")

# # 時間ラベルの作成
# years <- seq(from = as.POSIXct("1829-01-01"), 
#              by = "1 year", 
#              len = length(boat))
# head(years, n = 3)

VOLGA_EM_date <- tibble(Maintenance_date = X_date)
# 結合
VOLGA_EM_date <- 
  VOLGA_EM_date %>% 
  full_join(Metis_MF3_EM_VOLGA, by="Maintenance_date")
# NAを0へ置換
VOLGA_EM_date[is.na(VOLGA_EM_date)] <- 0

# EM数
sum(VOLGA_EM_date$VOLGA_E_EM) # 1790

####ファイル保存####
# write_tsv(VOLGA_EM_date, "./VOLGA/tsv_data/VOLGA_EM_date.tsv")

# ピボット変換
VOLGA_EM_date_pivot <- 
  pivot_longer(data = VOLGA_EM_date, cols = ends_with("EM")) 

# 列要素
VOLGA_EM_date_pivot %>% 
  distinct(name)

# 台数昇順にレベル変更
VOLGA_EM_date_pivot$name <- 
  factor(VOLGA_EM_date_pivot$name, 
         levels=c("VOLGA_E_EM"))

# フォント設定
# windowsFonts(Japan1GothicBBB = windowsFont("Japan1GothicBBB"))
par(family="Noto Sans")

# 折れ線グラフ
g_EM <- 
  ggplot(data = VOLGA_EM_date_pivot, mapping = aes(x=Maintenance_date,y=value,colour=name)) +
  geom_line(linewidth = 0.8) +
  theme_bw() + theme(text = element_text(size = 18)) +
  theme(legend.position = c(0.05, 0.98), legend.justification = c(0, 1)) +
  labs(x = "年月日",y = "件数", title = "Metis-MF3_VOLGA-E EM件数") +
  scale_x_date(breaks = seq(as.Date("2019-02-01"), as.Date("2022-11-01"), by="6 month"),labels=date_format("%Y/%m")) +
  scale_color_hue(name = "機種", labels = c(VOLGA_E_EM="VOLGA-E")) +
  scale_y_continuous(breaks=seq(0,10,1)) +
  coord_cartesian(ylim = c(0, 10))

# グラフ保存
file_path='./PDF/Metis-MF3_VOLGA-E_EM-dayly件数.pdf'
ggsave(file=file_path, device=cairo_pdf, plot=g_EM, dpi=300, w=12, h=4)

