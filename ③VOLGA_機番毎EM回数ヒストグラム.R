####ファイル読込####

Metis_MF3_EM_VOLGA <- read_tsv("./tsv_data/Metis_MF3_EM_VOLGA.tsv")
VOLGA_maintenance <- read_tsv("./tsv_data/VOLGA_maintenance.tsv")
VOLGA_machine <- read_tsv("./tsv_data/VOLGA_machine.tsv")

Metis_sheet3 <- read_tsv("./tsv_data/Metis_sheet3.tsv")

# フォント設定
# windowsFonts(Japan1GothicBBB = windowsFont("Japan1GothicBBB"))
par(family="Noto Sans")

VOLGA_maintenance %>% 
  distinct(Treatment_location)

# 機番毎のEM数の抽出 台数1405
Metis_MF3_EM_VOLGA_by_Machine_numbers <- 
  VOLGA_maintenance %>% 
  # filter(Treatment_location == "ADF部") %>%
  # filter(Peripheral_name == "COOK-C") %>%
  group_by(Machine_numbers) %>% 
  summarise(
    VOLGA_E_EM = n()
  )

sum(Metis_MF3_EM_VOLGA_by_Machine_numbers$VOLGA_E_EM)    # 1790
length(Metis_MF3_EM_VOLGA_by_Machine_numbers$VOLGA_E_EM) # 1405

# ピボット変換
Metis_MF3_EM_VOLGA_by_Machine_numbers_pivot <- 
  pivot_longer(data = Metis_MF3_EM_VOLGA_by_Machine_numbers, cols = ends_with("EM")) 

# # NAを0へ置換
# Metis_MF3_EM_ADF_by_Machine_numbers_pivot <- 
#   replace_na(Metis_MF3_EM_ADF_by_Machine_numbers_pivot,
#            replace = list(value = 0))

# ヒストグラム
g_EMcount_hist <- 
  ggplot(data = Metis_MF3_EM_VOLGA_by_Machine_numbers_pivot,
         mapping = aes(x = value, fill = name)) +
  geom_histogram(bins = 7, colour = "black",position = position_dodge(), alpha = 0.8) +
  geom_text(aes(y = after_stat(count) + 50, label = after_stat(count)), stat = "bin", binwidth = 1) +
  # facet_wrap(~name, ncol = 1) +
  labs(x = "EM回数", y = "count", fill = "機種", title = "Metis-MF3 VOLGA EM回数") +
  scale_fill_hue(labels = c(VOLGA_E_EM="VOLGA-E")) +
  theme_bw() + 
  theme(text = element_text(size = 16)) +
  scale_y_continuous(breaks = seq(0, 1200, 200)) +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  theme(legend.position = c(0.9,0.9), legend.justification = c(1,1)) +
  guides(fill = "none") +
  theme(strip.background = element_blank(), strip.text = element_text(size = 15)) +
  coord_cartesian(xlim = c(0, 10))

# グラフ保存
file_path='./PDF/Metis-MF3_VOLGA_EMcount.pdf'
ggsave(file=file_path, device=cairo_pdf, plot=g_EMcount_hist, dpi=300, w=8, h=4)

# 市場機すべて
Metis_MF3_EM_VOLGA_by_Machine_numbers_zero <- 
  VOLGA_machine %>% 
  select(機種略機番) %>% 
  rename(Machine_numbers = 機種略機番) %>% 
  full_join(Metis_MF3_EM_VOLGA_by_Machine_numbers) %>% 
  mutate_all(~replace(., is.na(.), 0))

sum(Metis_MF3_EM_VOLGA_by_Machine_numbers_zero$VOLGA_E_EM)  # 1790
nrow(Metis_MF3_EM_VOLGA_by_Machine_numbers_zero)      # 19065

Metis_MF3_EM_VOLGA_by_Machine_numbers_zero %>% 
  distinct(Machine_numbers)

# ピボット変換
Metis_MF3_EM_VOLGA_by_Machine_numbers_zero_pivot <- 
  pivot_longer(data = Metis_MF3_EM_VOLGA_by_Machine_numbers_zero, cols = ends_with("EM")) 

# ヒストグラム
g_EMcount_hist_zero <- 
  ggplot(data = Metis_MF3_EM_VOLGA_by_Machine_numbers_zero_pivot,
         mapping = aes(x = value, fill = name)) +
  geom_histogram(bins = 8, colour = "black", position = position_dodge(), alpha = 0.8) +
  geom_text(aes(y = after_stat(count) + 800, label = after_stat(count)), stat = "bin", binwidth = 1, size = 5) +
  # facet_wrap(~name, ncol = 1) +
  labs(x = "EM回数", y = "台数", fill = "機種", title = "Metis-MF3 VOLGA-E EM回数 0回含む") +
  # scale_fill_hue(labels = c(COOK_EM_Count="COOK-C",SINAI_EM_Count="SINAI-H")) +
  theme_bw() + 
  theme(text = element_text(size = 16)) +
  scale_y_continuous(breaks = seq(0, 20000, 2000),labels = label_comma()) +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  # theme(legend.position = c(0.9,0.9), legend.justification = c(1,1)) +
  guides(fill = "none") +
  theme(strip.background = element_blank(), strip.text = element_text(size = 15)) 

# グラフ保存
file_path='./PDF/Metis-MF3_VOLGA_EMcount_zero-2.pdf'
ggsave(file=file_path, device=cairo_pdf, plot=g_EMcount_hist_zero, dpi=300, w=8, h=4)
