####ファイル読込####
Metis_MIF_2211 <- read_tsv("./tsv_data/Metis_MIF_2211.tsv")
Metis_MF3_EM_ADF_by_Maintenance_date <- read_tsv("./tsv_data/Metis_MF3_EM_ADF_by_Maintenance_date.tsv")


# 納品日別台数/Metis_MF3_ADF_number_by_Due_dateの作成
Metis_MF3_ADF_number_by_Due_date <- 
  Create.graph_for.number_by.month(df = Metis_MIF_2211,
                                   field = "ADF",
                                   choice = c("COOK-C","SINAI-H"),
                                   sel_month = "納品年月日",
                                   date.name = "Due_date",
                                   file.save = FALSE,
                                   write.file_path = "./tsv_data/Metis_MF3_ADF_number_by_Due_date.tsv",
                                   graph_title = "Metis-MF3 ADF 製造年月別台数",
                                   legend.position = c(0.75, 0.95),
                                   breaks = seq(as.Date("2018-10-01"), as.Date("2022-10-01"), by="6 month"),
                                   graph.save = FALSE,
                                   save.graph_path='./PDF/Metis-MF3_ADF_製造年月別台数.pdf',
                                   graph.width = 10, 
                                   graph.height = 6)

Metis_MF3_ADF_number.cumsum_by_Due_date <- 
  Metis_MF3_ADF_number_by_Due_date$df %>% 
  mutate(
    `COOK-C` = cumsum(`COOK-C`),
    `SINAI-H` = cumsum(`SINAI-H`)
  )
X_date <- seq(as.Date("2019-01-01"), as.Date("2022-11-30"), by = "day")
ADF.X_date <- tibble(Maintenance_date = X_date)

Metis_MF3_EM.rate_ADF_by_daily <- 
  ADF.X_date %>%
  left_join(Metis_MF3_EM_ADF_by_Maintenance_date) %>% 
  left_join(Metis_MF3_ADF_number_by_Due_date$df, by=c("Maintenance_date"="Due_date")) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  mutate(
    `COOK-C.cumsum` = cumsum(`COOK-C`),
    `SINAI-H.cumsum` = cumsum(`SINAI-H`),
    COOK_C_EM.rate = COOK_C_EM/`COOK-C.cumsum`*100,
    SINAI_H_EM.rate = SINAI_H_EM/`SINAI-H.cumsum`*100
  )

# ピボット変換
Metis_MF3_EM.rate_ADF_by_daily_pivot <- 
  pivot_longer(data = Metis_MF3_EM.rate_ADF_by_daily, cols = ends_with("EM.rate")) 

# 列要素
Metis_MF3_EM.rate_ADF_by_daily_pivot %>% 
  distinct(name)

# フォント設定
# windowsFonts(Japan1GothicBBB = windowsFont("Japan1GothicBBB"))
par(family="Noto Sans")

# 折れ線グラフ
Att.labs <- c("COOK-C", "SINAI-H")
names(Att.labs) <- c("COOK_C_EM.rate", "SINAI_H_EM.rate")
g_EM.rate_ADF <- 
  ggplot(data = Metis_MF3_EM.rate_ADF_by_daily_pivot, 
         mapping = aes(x=Maintenance_date,y=value,colour=name)) +
  geom_line(linewidth = 0.8) +
  theme_bw() + 
  theme(text = element_text(size = 16), title = element_text(size = 14)) +
  # theme(legend.position = c(0.05, 0.98), legend.justification = c(0, 1)) +
  # scale_color_hue(name = "機種", 
  #                 labels = c(VOLGA_E_EM="VOLGA-E",AMUR_C_EM="AMUR-C中綴じ",AMUR_CHY_EM="AMUR-C(HY)")) +
  labs(x = "年月日",y = "EM率（％）", title = "Metis-MF3_ADF EM率") +
  scale_x_date(breaks = seq(as.Date("2019-02-01"), as.Date("2022-11-01"), by="6 month"),labels=date_format("%Y/%m")) +
  scale_y_continuous(breaks=seq(0, 0.2, 0.05)) +
  coord_cartesian(ylim = c(0, 0.2)) +
  theme(legend.position = "none") +
  theme(strip.background = element_blank(), strip.text = element_text(size = 16)) +
  facet_wrap(~name, ncol = 1, labeller = labeller(name = Att.labs))

# グラフ保存
file_path='./PDF/Metis-MF3_ADF_EM.rate-dayly.pdf'
ggsave(file=file_path, device=cairo_pdf, plot=g_EM.rate_ADF, dpi=300, w=10, h=6)
