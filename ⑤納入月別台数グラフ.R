# ファイル読込
Metis_MIF_2211 <- read_tsv("./tsv_data/Metis_MIF_2211.tsv")

names(Metis_MIF_2211)
# 関数使用
# ADF
Create.graph_for.number_by.month(df = Metis_MIF_2211,
                                 field = "ADF",
                                 choice = c("COOK-C","SINAI-H"),
                                 sel_month = "納入月",
                                 date.name = "month",
                                 file.save = FALSE,
                                 write.file_path = "./tsv_data/Metis_MF3_ADF_number_by_month-2.tsv",
                                 graph_title = "Metis-MF3 ADF 時系列納品台数",
                                 legend.position = c(0.85, 0.95),
                                 breaks = seq(as.Date("2018-10-01"), as.Date("2022-10-01"), by="6 month"),
                                 graph.save = FALSE,
                                 save.graph_path='./PDF/Metis-MF3_ADF_時系列台数.pdf',
                                 graph.width = 10, 
                                 graph.height = 6)
# FIN
Create.graph_for.number_by.month(df = Metis_MIF_2211,
                                 field = "フィニッシャー",
                                 choice = c("AMUR-C中綴じ","AMUR-C(HY)","VOLGA-E"),
                                 sel_month = "納入月",
                                 date.name = "month",
                                 file.save = FALSE,
                                 write.file_path = "./tsv_data/Metis_MF3_FIN_number_by_month-2.tsv",
                                 graph_title = "Metis-MF3 FIN 時系列納品台数",
                                 legend.position = c(0.85, 0.95),
                                 breaks = seq(as.Date("2018-10-01"), as.Date("2022-10-01"), by="6 month"),
                                 graph.save = FALSE,
                                 save.graph_path='./PDF/Metis-MF3_FIN_時系列台数.pdf',
                                 graph.width = 10, 
                                 graph.height = 6)
# ADF納入月別台数
Metis_MF3_ADF_number_pivot <- 
  Metis_MIF_2211 %>% 
  filter(ADF == "COOK-C" |
           ADF == "SINAI-H") %>%
  select(納入月,ADF,機種機番) %>% 
  group_by(納入月, ADF) %>%
  summarise(台数 = n(), .groups = "drop") %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

Metis_MF3_ADF_number_by_delivery_month <- 
  Metis_MF3_ADF_number_pivot %>% 
  pivot_wider(
    names_from = ADF,
    values_from = 台数
  )%>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  rename(delivery_month = 納入月)
# 累積値へ変換
Metis_MF3_ADF_number_by_month <- 
  Metis_MF3_ADF_number_by_delivery_month %>% 
  mutate(
    `COOK-C` = cumsum(`COOK-C`),
    `SINAI-H` = cumsum(`SINAI-H`)
  ) %>% 
  rename(month = delivery_month)

# ファイル保存
# write_tsv(Metis_MF3_ADF_number_by_month, 
#           "./tsv_data/Metis_MF3_ADF_number_by_month.tsv")

# フォント設定
# windowsFonts(Japan1GothicBBB = windowsFont("Japan1GothicBBB"))
par(family="Noto Sans")

# 折れ線グラフ
g_deli_ADF <- 
  ggplot(data = Metis_MF3_ADF_number_pivot, 
         mapping = aes(x=納入月,y=台数,colour=ADF)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.0) +
  theme_bw(base_family = "Japan1GothicBBB") + 
  theme(text = element_text(size = 20), title = element_text(size = 16)) +
  theme(legend.position = c(0.82, 0.95), legend.justification = c(0, 1)) +
  labs(x = "年月日",y = "台数", title = "Metis-MF3 ADF 納入台数") +
  scale_x_date(breaks = seq(as.Date("2018-10-01"), as.Date("2022-10-01"), by="6 month"),labels=date_format("%Y/%m")) +
  scale_color_hue(name = "機種") +
  coord_cartesian(ylim = c(0, 7000)) +
  scale_y_continuous(breaks = seq(0, 7000, 1000))
g_deli_ADF
# グラフ保存
file_path='./PDF/Metis-MF3_ADF_納入年月別台数-2.pdf'
ggsave(file=file_path, device=cairo_pdf, plot=g_deli_ADF, dpi=300, w=10, h=6)

# FIN納入月別台数
Metis_MF3_FIN_number_pivot <- 
  Metis_MIF_2211 %>% 
  filter(フィニッシャー == "VOLGA-E" |
           フィニッシャー == "AMUR-C(HY)" |
           フィニッシャー == "AMUR-C中綴じ") %>%
  select(納入月,フィニッシャー,機種機番) %>% 
  group_by(納入月, フィニッシャー) %>%
  summarise(台数 = n(), .groups = "drop") %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

Metis_MF3_FIN_number_by_delivery_month <- 
  Metis_MF3_FIN_number_pivot %>% 
  pivot_wider(
    names_from = フィニッシャー,
    values_from = 台数
  )%>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  rename(delivery_month = 納入月)
# 累積値へ変換
Metis_MF3_FIN_number_by_month <- 
  Metis_MF3_FIN_number_by_delivery_month %>% 
  mutate(
    `AMUR-C(HY)` = cumsum(`AMUR-C(HY)`),
    `AMUR-C中綴じ` = cumsum(`AMUR-C中綴じ`),
    `VOLGA-E` = cumsum(`VOLGA-E`)
  ) %>% 
  rename(month = delivery_month)
  
# ファイル保存
# write_tsv(Metis_MF3_FIN_number_by_month, 
#           "./tsv_data/Metis_MF3_FIN_number_by_month.tsv")

# フォント設定
# windowsFonts(Japan1GothicBBB = windowsFont("Japan1GothicBBB"))
par(family="Noto Sans")

# 折れ線グラフ
g_deli_FIN <- 
  ggplot(data = Metis_MF3_FIN_number_pivot, 
         mapping = aes(x=納入月,y=台数,colour=フィニッシャー)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.0) +
  theme_bw(base_family = "Japan1GothicBBB") + 
  theme(text = element_text(size = 20), title = element_text(size = 16)) +
  theme(legend.position = c(0.75, 0.95), legend.justification = c(0, 1)) +
  labs(x = "年月日",y = "台数", title = "Metis-MF3 Finisher 納入台数") +
  scale_x_date(breaks = seq(as.Date("2018-10-01"), as.Date("2022-10-01"), by="6 month"),labels=date_format("%Y/%m")) +
  scale_color_hue(name = "機種") +
  coord_cartesian(ylim = c(0, 1000)) +
  scale_y_continuous(breaks = seq(0, 1000, 200))
g_deli_FIN
# グラフ保存
file_path='./PDF/Metis-MF3_FIN_納入年月別台数-2.pdf'
ggsave(file=file_path, device=cairo_pdf, plot=g_deli_FIN, dpi=300, w=10, h=6)

