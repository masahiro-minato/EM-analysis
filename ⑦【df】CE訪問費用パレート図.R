####ファイル読込####
Metis_MF3_2211 <- read_tsv("./tsv_data/Metis_MF3_2211.tsv")
Metis_MIF_2211 <- read_tsv("./tsv_data/Metis_MIF_2211.tsv")

#### CE訪問費用グラフ ####--------------------------------------------
Metis_MF3_EM.Cost.Pareto.chart <- 
  tibble(
    field_1 = c(rep("ADF",2),rep("フィニッシャー",3),rep("バンク",3)),
    T_location = list(list("ADF部"),
                      list("ADF部"),
                      list("ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部","ﾊﾟﾝﾁ部","ｽﾃｰﾌﾟﾙ部"),
                      list("ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部","ﾊﾟﾝﾁ部","ｽﾃｰﾌﾟﾙ部"),
                      list("ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部","ﾊﾟﾝﾁ部","ｽﾃｰﾌﾟﾙ部"),
                      list("第3給紙","第4給紙","第5給紙","第()給紙","駆動部(OP給紙)"),
                      list("第3給紙","第4給紙","第5給紙","第()給紙","駆動部(OP給紙)"),
                      list("第3給紙","第4給紙","第5給紙","第()給紙","駆動部(OP給紙)")),
    theme.name = c(c("COOK-C","SINAI-H"),
                   c("AMUR-C中綴じ","AMUR-C(HY)","VOLGA-E"),
                   c("CANARIA-D","GOREE-D","CUBA-C")),
    y1.lim = list(list(0,6000),             # グラフ第1軸の範囲
                  list(0,2200),
                  list(0,250),
                  list(0,120),
                  list(0,800),
                  list(0,750),
                  list(0,10),
                  list(0,60)),
    breaks = list(list(seq(0, 6000, 1000)),　# グラフ第1軸の目盛り
                  list(seq(0, 2200, 500)),
                  list(seq(0, 250, 50)),
                  list(seq(0, 120, 50)),
                  list(seq(0, 800, 100)),
                  list(seq(0, 750, 100)),
                  list(seq(0, 10, 2)),
                  list(seq(0, 60, 10))),
    graph.width = c(22, 18, 13, 9, 15, 19, 4, 7),
    graph.height = c(6, 6, 6, 6, 6, 6, 6, 6),
    hjust = c(100,50,5,3,20,20,0.2,1)
  ) %>% 
  rowwise() %>% 
  mutate(
    func.Pareto.chart_by.phenomenon_for.EM.cost =
      list(Pareto.chart_by.phenomenon_for.EM.cost),
    fig.Pareto.chart_by.phenomenon_for.EM.cost =
      list(
        func.Pareto.chart_by.phenomenon_for.EM.cost(
          df = Metis_MF3_2211,               # 保守データ
          T_location = T_location,           # 処置部
          P_name = theme.name,               # 周辺機名
          breaks = breaks,　                 # グラフ第1軸の目盛り
          y1.lim = y1.lim,                   # グラフ第1軸の範囲
          file.save = FALSE,                 # ファイルの保存
          file_path = str_c("./tsv_data/Metis_MF3_",theme.name,"_CECOST.tsv"), # ファイル保存パス
          graph.save = TRUE,                # グラフの保存
          graph.title = str_c("Metis-MF3 ",theme.name," CE訪問費用 (2019.2～2022.11)"), # グラフタイトル
          graph.path = str_c("./PDF/Metis-MF3_",theme.name,"_CE訪問費用パレート図(2019.2～2022.11)-3.pdf"), # グラフ保存パス
          graph.width = graph.width,         # グラフの横幅
          graph.height = graph.height,       # グラフの高さ
          hjust = hjust                      # 棒グラフ上部記載の数値高さ
        )
      )
  ) %>% 
  ungroup()

# 結果保存
saveRDS(Metis_MF3_EM.Cost.Pareto.chart, file = "./output/Metis_MF3_EM.Cost.Pareto.chart.rds")
