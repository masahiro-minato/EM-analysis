# ファイル読込
Metis_MF3_EM.count.ope <- read_tsv("./tsv_data/Metis_MF3_EM.count.ope.tsv")
# フォント設定
# windowsFonts(Japan1GothicBBB = windowsFont("Japan1GothicBBB"))
par(family="Noto Sans")

Metis_MF3_EM.count.ope %>% 
  names()
# ヒストグラム
g_EM_hist_all <- 
  ggplot(data = Metis_MF3_EM.count.ope,
         mapping = aes(x = EM.count, fill = Model)) +
  geom_histogram(bins = 28, alpha = 0.8) +
  # geom_text(aes(y = after_stat(count) + 200, label = after_stat(count)), stat = "bin", binwidth = 1, size = 2) +
  # facet_wrap(~Model, ncol = 1) +
  theme_bw() + 
  theme(legend.position = c(0.70, 0.95), legend.justification = c(0, 1)) +
  theme(text = element_text(size = 16), 
        title = element_text(size = 14)) +
  labs(x = "EM回数", y = "count", fill = "機種", 
       title = "Metis-MF3 機番毎のEM回数(2019.02～2022.11)") +
  scale_y_continuous(breaks = seq(0, 80000, 10000),labels = label_comma()) +
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  coord_cartesian(xlim = c(0,30))
  # guides(fill = "none") +
  # theme(strip.background = element_blank(), strip.text = element_text(size = 16)) 

# グラフ保存
file_path='./PDF/Metis-MF3システム_機番毎のEMcount-2.pdf'
ggsave(file=file_path, device=cairo_pdf, plot=g_EM_hist_all, dpi=300, w=12, h=6)

# ヒストグラム/個別
g_EM_hist_all_wrap <- 
  ggplot(data = Metis_MF3_EM.count.ope,
         mapping = aes(x = EM.count, fill = Model)) +
  geom_histogram(bins = 28, alpha = 0.8) +
  geom_text(aes(y = after_stat(count) + 1500, label = after_stat(count)), stat = "bin", binwidth = 1, size = 4) +
  facet_wrap(~Model, ncol = 1) +
  theme_bw() + 
  theme(legend.position = c(0.70, 0.95), legend.justification = c(0, 1)) +
  theme(text = element_text(size = 16), 
        title = element_text(size = 14)) +
  labs(x = "EM回数", y = "count", fill = "機種", 
       title = "Metis-MF3 機番毎のEM回数(2019.02～2022.11)") +
  scale_y_continuous(breaks = seq(0, 40000, 5000),labels = label_comma()) +
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  coord_cartesian(xlim = c(0,30)) +
  guides(fill = "none") +
  theme(strip.background = element_blank(), strip.text = element_text(size = 16))

# グラフ保存
file_path='./PDF/Metis-MF3システム_機番毎のEMcount_wrap-2.pdf'
ggsave(file=file_path, device=cairo_pdf, plot=g_EM_hist_all_wrap, dpi=300, w=15, h=15)
