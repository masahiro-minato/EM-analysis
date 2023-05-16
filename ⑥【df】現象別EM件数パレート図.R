####ファイル読込####
Metis_MF3_2211 <- read_tsv("./tsv_data/Metis_MF3_2211.tsv")
Metis_MIF_2211 <- read_tsv("./tsv_data/Metis_MIF_2211.tsv")

Metis_MF3_EM.Pareto.chart <- 
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
    y1.lim = list(list(0,4500),             # グラフ第1軸の範囲
                  list(0,2000),
                  list(0,200),
                  list(0,100),
                  list(0,550),
                  list(0,600),
                  list(0,6),
                  list(0,50)),
    breaks = list(list(seq(0, 4500, 500)),　# グラフ第1軸の目盛り
                  list(seq(0, 2000, 500)),
                  list(seq(0, 200, 50)),
                  list(seq(0, 100, 20)),
                  list(seq(0, 550, 100)),
                  list(seq(0, 600, 100)),
                  list(seq(0, 6, 1)),
                  list(seq(0, 50, 10))),
    graph.width = c(22, 18, 13, 9, 15, 19, 2, 7),
    graph.height = c(6, 6, 6, 6, 6, 6, 6, 6),
    hjust = c(80,30,3,2,10,10,0.2,1)
  ) %>% 
  rowwise() %>% 
  mutate(
    func.Create.graph_for.number_by.month =
      list(Create.graph_for.number_by.month),
    func.Pareto.chart_by.phenomenon =
      list(Pareto.chart_by.phenomenon),
    Number_by_Manufacturing_date = 
      list(
        func.Create.graph_for.number_by.month(
          df = Metis_MIF_2211,
          field = field_1,
          choice = theme.name,
          sel_month = "製造年月",
          date.name = "Manufacturing_date",
          graph_title = str_c("Metis-MF3 ", theme.name ," 時系列納品台数"),
          )
      ),
    fig.Pareto.chart_by.phenomenon =
      list(
        func.Pareto.chart_by.phenomenon(
          df = Metis_MF3_2211,               # 保守データ
          T_location = T_location,           # 処置部
          P_name = theme.name,               # 周辺機名
          Choices = "Manufacturing_date",    # グルーピング条件
          num = 1,                           # num未満はその他へ集約する。1の場合はすべて表示
          Number_by_Manufacturing_date = Number_by_Manufacturing_date, # 製造月別台数
          y1.lim = y1.lim,                   # グラフ第1軸の範囲
          breaks = breaks,　                 # グラフ第1軸の目盛り
          graph_save = TRUE,                # グラフの保存
          graph_title = str_c("Metis-MF3 ",theme.name," EM現象別件数 (2019.2～2022.11)"), # グラフタイトル
          graph_path = str_c("./PDF/Metis-MF3_",theme.name,"_EM現象別件数パレート図(2019.2～2022.11)-2.pdf"), # グラフ保存パス
          graph_width = graph.width,         # グラフの横幅
          graph_height = graph.height,       # グラフの高さ
          hjust = hjust                      # 棒グラフ上部記載の数値高さ
        )
      )
  ) %>% 
  ungroup()

saveRDS(Metis_MF3_EM.Pareto.chart, file = "./output/Metis_MF3_EM.Pareto.chart.rds")

Metis_MF3_EM.Pareto.chart %>% 
  pluck("fig.Pareto.chart_by.phenomenon", 3)

Metis_MF3_EM.Pareto.chart %>% 
  pull(fig.Pareto.chart_by.phenomenon) %>% 
  patchwork::wrap_plots(ncol = 3)

names(Metis_MF3_2211)
names(Metis_MIF_2211)
print(n=93,Metis_MF3_2211 %>% 
        distinct(Treatment_location))
print(n=93,Metis_MF3_2211 %>% 
        distinct(Peripheral_name))