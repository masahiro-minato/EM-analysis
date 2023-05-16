####ファイル読込####
Metis_MF3_2211 <- read_tsv("./tsv_data/Metis_MF3_2211.tsv")
Metis_MIF_2211 <- read_tsv("./tsv_data/Metis_MIF_2211.tsv")

#### EM件数・EM率グラフ ####--------------------------------------------
Metis_MF3_EM <- 
  tibble(
    field = c("ADF","フィニッシャー","バンク"),
    field.name = c("ADF","FIN","BANK"),
    treatment.area = list(list("ADF部"),
                   list("ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部","ﾊﾟﾝﾁ部","ｽﾃｰﾌﾟﾙ部"),
                   list("第3給紙","第4給紙","第5給紙","第()給紙 ","駆動部(OP給紙)")),
    choice.mt = list(list("COOK-C","SINAI-H"),
                     list("AMUR-C中綴じ","AMUR-C(HY)","VOLGA-E"),
                     list("CANARIA-D","GOREE-D","CUBA-C")),
    y.breaks.rate = rep(list(seq(0, 1.0, 0.05)),3),
    ylim.rate = rep(list(c(0, 0.3)),3),
    y.breaks.EM = list(list(seq(0, 50, 10)),list(seq(0, 10, 2)),list(seq(0, 20, 5))),
    ylim.EM = list(list(c(0, 50)),list(c(0, 10)),list(c(0, 16))),
    graph.width = c(10, 10, 10),
    graph.height = c(6, 9, 9)
  ) %>% 
  rowwise() %>% 
  mutate(
    func.Create.graph_for.number_by.month =
      list(Create.graph_for.number_by.month),
    func.Create.graph_for.EM.rate_number =
      list(Create.graph_for.EM.rate_number),
    df.num = 
      list(
        func.Create.graph_for.number_by.month(
          df = Metis_MIF_2211,
          field = field,
          choice = choice.mt)
      ),
    fig_EM.rate = 
      list(
        func.Create.graph_for.EM.rate_number(
          df.mt = Metis_MF3_2211,
          df.num = df.num,
          field = treatment.area,
          choice.mt = choice.mt,
          EM.rate = "EM.rate",
          y.breaks = y.breaks.rate,
          ylim = ylim.rate,
          graph.save = TRUE,
          save.graph_path = str_c("./PDF/Metis-MF3_",field.name,"_EM.rate-dayly.pdf"),
          graph_title = str_c("Metis-MF3_",field.name," EM率"),
          graph.width = graph.width,
          graph.height = graph.height
        )
      ),
    fig_EM.numb = 
      list(
        func.Create.graph_for.EM.rate_number(
          df.mt = Metis_MF3_2211,
          df.num = df.num,
          field = treatment.area,
          choice.mt = choice.mt,
          EM.rate = "EM",
          y.breaks = y.breaks.EM,
          ylim = ylim.EM,
          graph.save = TRUE,
          save.graph_path = str_c("./PDF/Metis-MF3_",field.name,"_EM.numb-dayly.pdf"),
          graph_title = str_c("Metis-MF3_",field.name," EM件数"),
          graph.width = graph.width,
          graph.height = graph.height
        )
      )
  ) %>% 
  ungroup()

#### オブジェクト保存 ####------------------------------------
saveRDS(Metis_MF3_EM, file = "./output/Metis_MF3_EM.rds")
#### オブジェクト読込 ####------------------------------------
Metis_MF3_EM <- readRDS("./output/Metis_MF3_EM.rds")

# フォント設定
# windowsFonts(Japan1GothicBBB = windowsFont("Japan1GothicBBB"))
par(family="Noto Sans")


# library(plotly)
# 要素の抽出
Metis_MF3_EM %>% 
  pluck("fig_EM.numb", 1) %>% 
  plotly::ggplotly()
# グラフの合成描画
Metis_MF3_EM %>% 
  pull(fig_EM.numb) %>% 
  patchwork::wrap_plots(ncol = 1, heights = c(2, 3, 3))

  

# 保守データ・機器データの列名と要素
names(Metis_MF3_2211)
names(Metis_MIF_2211)
print(n=93,Metis_MF3_2211 %>% 
        distinct(Treatment_location))
print(n=93,Metis_MF3_2211 %>% 
        distinct(Peripheral_name))