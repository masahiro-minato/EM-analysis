####ファイル読込####
Metis_sheet3_2211 <- read_tsv("./tsv_data/Metis_sheet3_2211.tsv")
Metis_MIF_2211 <- read_tsv("./tsv_data/Metis_MIF_2211.tsv")
names(Metis_sheet3_2211)

# 日付へ変換
Metis_sheet3_2211$製造年月 <- 
  paste(str_sub(Metis_sheet3_2211$製造年月, start=1, end=4),
        str_sub(Metis_sheet3_2211$製造年月, start=5, end=-1),("01"),
        sep="-") %>% 
  as.POSIXct() %>% 
  as.Date(tz = "Asia/Tokyo")

names(Metis_MIF_2211)
Metis_MIF_2211 %>% 
  distinct(フィニッシャー)
# 関数使用
Create.graph_for.number_by.month(df = Metis_MIF_2211,
                                 field = "フィニッシャー",
                                 choice = c("AMUR-C中綴じ","AMUR-C(HY)","VOLGA-E"),
                                 sel_month = "製造年月",
                                 date.name = "Manufacturing_date",
                                 file.save = FALSE,
                                 write.file_path = "./tsv_data/Metis_MF3_FIN_number_by_Manufacturing_date.tsv",
                                 graph_title = "Metis-MF3 ADF 製造年月別台数",
                                 legend.position = c(0.75, 0.95),
                                 breaks = seq(as.Date("2018-10-01"), as.Date("2022-10-01"), by="6 month"),
                                 graph.save = FALSE,
                                 save.graph_path='./PDF/Metis-MF3_FIN_製造年月別台数.pdf',
                                 graph.width = 10, 
                                 graph.height = 6)

# 製造月別台数
Metis_MF3_FIN_number_pivot <- 
  Metis_sheet3_2211 %>% 
  filter(フィニッシャー == "VOLGA-E" |
           フィニッシャー == "AMUR-C(HY)" |
           フィニッシャー == "AMUR-C中綴じ") %>%
  select(製造年月,フィニッシャー,機種機番) %>% 
  group_by(製造年月, フィニッシャー) %>%
  summarise(台数 = n(), .groups = "drop") %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

Metis_MF3_FIN_number_by_Manufacturing_date <- 
  Metis_MF3_FIN_number_pivot %>% 
  pivot_wider(
    names_from = フィニッシャー,
    values_from = 台数
  )%>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  rename(Manufacturing_date = 製造年月)

####ファイル保存####
write_tsv(Metis_MF3_FIN_number_by_Manufacturing_date, 
          "./tsv_data/Metis_MF3_FIN_number_by_Manufacturing_date.tsv")

# フォント設定
# windowsFonts(Japan1GothicBBB = windowsFont("Japan1GothicBBB"))
par(family="Noto Sans")

# 折れ線グラフ
g_Mf <- 
  ggplot(data = Metis_MF3_FIN_number_pivot, 
         mapping = aes(x=製造年月,y=台数,colour=フィニッシャー)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.0) +
  theme_bw() + 
  theme(text = element_text(size = 20), title = element_text(size = 18)) +
  theme(legend.position = c(0.75, 0.95), legend.justification = c(0, 1)) +
  labs(x = "年月日",y = "台数", title = "Metis-MF3 Finisher 製造年月別台数") +
  scale_x_date(breaks = seq(as.Date("2018-10-01"), as.Date("2022-10-01"), by="6 month"),labels=date_format("%Y/%m")) +
  scale_color_hue(name = "機種") +
  coord_cartesian(ylim = c(0, 1300)) +
  scale_y_continuous(breaks = seq(0, 1300, 200))

# グラフ保存
file_path='./PDF/Metis-MF3_FIN_製造年月別台数-2.pdf'
ggsave(file=file_path, device=cairo_pdf, plot=g_Mf, dpi=300, w=10, h=6)

# 合計台数
sum(Metis_MF3_FIN_number_by_Manufacturing_date$`VOLGA-E`, na.rm = TRUE)        # 18393
sum(Metis_MF3_FIN_number_by_Manufacturing_date$`AMUR-C(HY)`, na.rm = TRUE)     # 2456
sum(Metis_MF3_FIN_number_by_Manufacturing_date$`AMUR-C中綴じ`, na.rm = TRUE)   # 8854

#### 別の方法 ----------------------------

# VOLGA-E 製造月別台数
Metis_MF3_VOLGA_number <-
  Metis_sheet3_2211 %>%
  filter(フィニッシャー == "VOLGA-E") %>%
  group_by(製造年月) %>%
  summarise(
    VOLGA_E_num = n()
  )
# AMUR-C(HY) 製造月別台数
Metis_MF3_AMUR_CHY_number <-
  Metis_sheet3_2211 %>%
  filter(フィニッシャー == "AMUR-C(HY)") %>%
  group_by(製造年月) %>%
  summarise(
    AMUR_CHY_num = n()
  )
# AMUR-C中綴じ 製造月別台数
Metis_MF3_AMUR_C_number <-
  Metis_sheet3_2211 %>%
  filter(フィニッシャー == "AMUR-C中綴じ") %>%
  group_by(製造年月) %>%
  summarise(
    AMUR_C_num = n()
  )
# 結合
Metis_MF3_FIN_number_by_Manufacturing_date <-
  Metis_MF3_VOLGA_number %>%
  full_join(Metis_MF3_AMUR_CHY_number, by="製造年月") %>%
  full_join(Metis_MF3_AMUR_C_number, by="製造年月")


# 列名変更
Metis_MF3_FIN_number_by_Manufacturing_date <- 
  Metis_MF3_FIN_number_by_Manufacturing_date %>% 
  rename(Manufacturing_date = 製造年月) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

####ファイル保存####
# write_tsv(Metis_MF3_FIN_number_by_Manufacturing_date, "./tsv_data/Metis_MF3_FIN_number_by_Manufacturing_date.tsv")

# フォント設定
# windowsFonts(Japan1GothicBBB = windowsFont("Japan1GothicBBB"))
par(family="Noto Sans")

# ピボット変換
Metis_MF3_FIN_number_by_Manufacturing_date_pivot <- 
  pivot_longer(data = Metis_MF3_FIN_number_by_Manufacturing_date, cols = ends_with("num")) 

# 折れ線グラフ
g_Mf <- 
  ggplot(data = Metis_MF3_FIN_number_by_Manufacturing_date_pivot, 
         mapping = aes(x=Manufacturing_date,y=value,colour=name)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.0) +
  theme_bw() + 
  theme(text = element_text(size = 20), title = element_text(size = 18)) +
  theme(legend.position = c(0.75, 0.95), legend.justification = c(0, 1)) +
  labs(x = "年月日",y = "台数", title = "Metis-MF3 Finisher 製造年月別台数") +
  scale_x_date(breaks = seq(as.Date("2018-10-01"), as.Date("2022-10-01"), by="6 month"),labels=date_format("%Y/%m")) +
  scale_color_hue(name = "機種", labels = c(VOLGA_E_num="VOLGA-E",AMUR_CHY_num="AMUR-C(HY)",AMUR_C_num="AMUR-C中綴じ")) +
  coord_cartesian(ylim = c(0, 1300)) +
  scale_y_continuous(breaks = seq(0, 1300, 200))

# グラフ保存
file_path='./PDF/Metis-MF3_FIN_製造年月別台数.pdf'
ggsave(file=file_path, device=cairo_pdf, plot=g_Mf, dpi=300, w=10, h=6)

# 合計台数
sum(Metis_MF3_FIN_number_by_Manufacturing_date$VOLGA_E_num)  # 15243 18393
sum(Metis_MF3_FIN_number_by_Manufacturing_date$AMUR_CHY_num) # 2004 2456
sum(Metis_MF3_FIN_number_by_Manufacturing_date$AMUR_C_num)   # 7302 8854
