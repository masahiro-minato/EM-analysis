# ファイル読込
Metis_MF3_2211 <- read_tsv("./tsv_data/Metis_MF3_2211.tsv")
Metis_MIF_2211 <- read_tsv("./tsv_data/Metis_MIF_2211.tsv")

#### 納品年月別台数グラフ --------------------------------------------
Metis_MF3_Num.of.units_by.delivery.date <- 
  tibble(
    field = c("ADF","フィニッシャー","バンク"),
    field.name = c("ADF","FIN","BANK"),
    treatment.area = list(list("ADF部"),
                          list("ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部","ﾊﾟﾝﾁ部","ｽﾃｰﾌﾟﾙ部"),
                          list("第3給紙","第4給紙","第5給紙","第()給紙 ","駆動部(OP給紙)")),
    choice.mt = list(list("COOK-C","SINAI-H"),
                     list("AMUR-C中綴じ","AMUR-C(HY)","VOLGA-E"),
                     list("CANARIA-D","GOREE-D","CUBA-C")),
    legend.position = list(list(0.80, 0.95),
                           list(0.75, 0.95),
                           list(0.80, 0.95)),
    y.breaks = list(list(seq(0, 7000, 1000)),
                    list(seq(0, 1000, 250)),
                    list(seq(0, 8000, 1000))),
    graph.width = c(10, 10, 10),
    graph.height = c(6, 6, 6)
  ) %>% 
  rowwise() %>% 
  mutate(
    func.Create.graph_for.number_by.month =
      list(Create.graph_for.number_by.month),
    return_Create.graph_for.number_by.month = 
      list(
        func.Create.graph_for.number_by.month(
          df = Metis_MIF_2211,
          field = field,
          choice = choice.mt,
          sel_month = "納入月",
          date.name = "delivery_date",
          file.save = FALSE,
          write.file_path = str_c("./tsv_data/Metis_MF3_",field.name,".num_by.delivery.tsv"),
          graph_title = str_c("Metis-MF3 ",field.name," 納品台数"),
          legend.position = legend.position,
          breaks = seq(as.Date("2019-01-01"), as.Date("2022-11-01"), by="6 month"),
          y.breaks = y.breaks,
          graph.save = TRUE,
          save.graph_path = str_c("./PDF/Metis-MF3_",field.name,".納品台数-2.pdf"),
          graph.width = graph.width, 
          graph.height = graph.height)),
    df.data = 
      list(return_Create.graph_for.number_by.month[["df"]]),
    graph =
      list(return_Create.graph_for.number_by.month[["graph"]])
  ) %>% 
  ungroup()

#### オブジェクト保存 ------------------------------------
saveRDS(Metis_MF3_Num.of.units_by.delivery.date, file = "./output/Metis_MF3_Num.of.units_by.delivery.date.rds")
#### オブジェクト読込 ------------------------------------
Metis_MF3_Num.of.units_by.delivery.date <- readRDS("./output/Metis_MF3_Num.of.units_by.delivery.date.rds")

# フォント設定
# windowsFonts(Japan1GothicBBB = windowsFont("Japan1GothicBBB"))
par(family="Noto Sans")

# 要素の抽出
Metis_MF3_Num.of.units_by.delivery.date %>% 
  pluck("graph", 1) %>% 
  plotly::ggplotly()
Metis_MF3_Num.of.units_by.delivery.date %>% 
  pluck("df.data", 1)
# グラフの合成描画
Metis_MF3_Num.of.units_by.delivery.date %>% 
  pull(graph) %>% 
  patchwork::wrap_plots(ncol = 1)
# リストからの要素抽出
ret.obj.1 <- 
  Metis_MF3_Num.of.units_by.delivery.date %>% 
  pluck("return_Create.graph_for.number_by.month", 2)
ret.obj.1[["graph"]]
ret.obj.1[["df"]]
