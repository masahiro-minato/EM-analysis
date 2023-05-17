# ファイル読込
Metis_MF3_EM.count.ope <- read_tsv("./VOLGA/tsv_data/Metis_MF3_EM.count.ope.tsv")

# 最大EM件数 27
max(Metis_MF3_EM.count.ope$EM.count)
# 数値型列の抽出
EM.count.ope.nume <- Filter(is.numeric,Metis_MF3_EM.count.ope)

EM.count.ope.nume %>% 
  names()

EM.count.ope.nume <- 
  EM.count.ope.nume %>% 
  select(-機器番号,-最新訪問日,-`Monthly counter`)

EM.count.ope.nume.cor <- 
  EM.count.ope.nume %>% 
  # select(EM.count,
  #        ACV, `Jobs/PGS AVE`, `SC Ratio`, 
  #        `Paper Jam Ratio`, `Original Jam Ratio`, TCV) %>% 
  select(EM.count, ACV,
         #  `Jobs/PGS AVE`, `SC Ratio`, `Paper Jam Ratio`, `Original Jam Ratio`, 
         `A3/DLT, Larger`,`Fusing lamp ON time`, `Operation Time Ratio`,
         TCV) %>% 
  mutate(
    Model = Metis_MF3_EM.count.ope$Model
  )
# フォント設定
windowsFonts(Japan1GothicBBB = windowsFont("Japan1GothicBBB"))
# install.packages("psych")
library(psych)
psych::pairs.panels(EM.count.ope.nume.cor)

library(GGally)
ggp <- 
  ggpairs(EM.count.ope.nume.cor, 
          columns = seq_len(ncol(EM.count.ope.nume.cor)), 
          aes(color = Model, alpha = 0.5),
          upper = list(continuous = wrap("cor",size=3)),
          diag=list(continuous="barDiag"),
          lower = list(continuous = wrap("points",size=0.1)
                       # continuous = wrap("smooth",
                       #                   se = TRUE,
                       #                   size = .05)
          )) +
  theme_bw(base_family = "Japan1GothicBBB") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y= element_text(size=5),
        legend.title = element_text(size=7),
        legend.text = element_text(size=7),
        axis.title = element_text(size=7),
        plot.title = element_text(size=7),
        strip.text = element_text(size=10),
        strip.background = element_blank())
ggp
# グラフ保存
file_path='./VOLGA/PDF/Metis-MF3_相関図_ggp.pdf'
ggsave(file=file_path, device=cairo_pdf, plot=ggp, dpi=200, w=10, h=10)

# install.packages("corrplot")
library(corrplot)
EM.count.ope.nume.corrplot <- 
  EM.count.ope.nume %>% 
  select(EM.count,
         ACV, `Jobs/PGS AVE`, `SC Ratio`,
         `Paper Jam Ratio`, `Original Jam Ratio`, TCV) %>%
  mutate(
    Model = Metis_MF3_EM.count.ope$Model
  )
corrplot::corrplot(cor(EM.count.ope.nume.corrplot[,-8]))

file_path_corrp='./VOLGA/PDF/Metis-MF3_相関図_corrp_upper_2.pdf'

corrplot::corrplot(cor(EM.count.ope.nume), 
                   method = "ellipse",
                   type = "upper",
                   col = colorRampPalette(c("#f39800","white", "#4393C3"))(10),
                   tl.col = "black",
                   tl.cex = 0.6,
                   tl.srt = 45,
                   cl.cex =0.8)

corrplot::corrplot.mixed(cor(EM.count.ope.nume), 
                         lower = "ellipse", 
                         upper = "number",
                         lower.col = colorRampPalette(c("#f39800","white", "#4393C3"))(10),
                         upper.col = colorRampPalette(c("#f39800","white", "#4393C3"))(10),
                         tl.col = "black",
                         tl.cex = 0.6,
                         tl.srt = 45)
dev.copy(cairo_pdf,file=file_path_corrp, width=14, height=14)
dev.off()                  
