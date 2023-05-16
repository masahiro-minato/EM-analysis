####ファイル読込####
Metis_MF3_2211 <- read_tsv("./tsv_data/Metis_MF3_2211.tsv")
Metis_MIF_2211 <- read_tsv("./tsv_data/Metis_MIF_2211.tsv")

#### EM回数ヒストグラム ####--------------------------------------------
Metis_MF3.histogram_for.EM.count <- 
  tibble(
    field = c("ADF","フィニッシャー","バンク"),
    field.name = c("ADF","FIN","BANK"),
    treatment.area = list(list("ADF部"),
                          list("ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部","ﾊﾟﾝﾁ部","ｽﾃｰﾌﾟﾙ部"),
                          list("第3給紙","第4給紙","第5給紙","第()給紙 ","駆動部(OP給紙)")),
    choice.mt = list(list("COOK-C","SINAI-H"),
                     list("AMUR-C中綴じ","AMUR-C(HY)","VOLGA-E"),
                     list("CANARIA-D","GOREE-D","CUBA-C")),
    y.breaks = list(list(seq(0, 125000, 50000)),
                    list(seq(0, 18000, 5000)),
                    list(seq(0, 150000, 50000))),
    x.breaks = list(list(seq(0, 18, 5)),
                    list(seq(0, 10, 2)),
                    list(seq(0, 7, 1))),
    graph.width = c(8, 5, 4),
    graph.height = c(4.5, 6, 6),
    hjust = c(5000,1000,7000)
  ) %>% 
  rowwise() %>% 
  mutate(
    func.Create.histogram_for.EM.count =
      list(Create.histogram_for.EM.count),
    func.One_hot_Peripheral_machine.2 =
      list(One_hot_Peripheral_machine.2),
    fig_histogram_for.EM.count = 
      list(
        func.Create.histogram_for.EM.count(
          df_EM = Metis_MF3_2211,
          df_machine = Metis_MIF_2211,
          treatment.area = treatment.area,
          choice = choice.mt,
          col.name = "Machine_numbers",
          graph_save = FALSE,
          y.breaks = y.breaks,
          x.breaks = x.breaks,
          file_path = str_c("./PDF/Metis-MF3_",field.name,".EMcount-2.pdf"),
          grapf_title = str_c("Metis-MF3_",field.name," 機番毎のEM回数(2019.2～2022.11)"),
          graph.width = graph.width,
          graph.height = graph.height
        )
      )
  ) %>% 
  ungroup()

#### オブジェクト保存 ####------------------------------------
saveRDS(Metis_MF3.histogram_for.EM.count, file = "./output/Metis_MF3.histogram_for.EM.count.rds")

Metis_MF3.histogram_for.EM.count <- readRDS("./output/Metis_MF3.histogram_for.EM.count.rds")

# フォント設定
# windowsFonts(Japan1GothicBBB = windowsFont("Japan1GothicBBB"))
par(family="Noto Sans")

# 要素の抽出
Metis_MF3.histogram_for.EM.count %>% 
  pluck("fig_histogram_for.EM.count", 1) %>% 
  plotly::ggplotly()
# グラフの合成描画
Metis_MF3.histogram_for.EM.count %>% 
  pull(fig_histogram_for.EM.count) %>% 
  patchwork::wrap_plots(ncol = 1, heights = c(2, 3, 3))
