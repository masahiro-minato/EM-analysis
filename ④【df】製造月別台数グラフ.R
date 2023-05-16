####ファイル読込####
Metis_MF3_2211 <- read_tsv("./tsv_data/Metis_MF3_2211.tsv")
Metis_MIF_2211 <- read_tsv("./tsv_data/Metis_MIF_2211.tsv")

#### 製造年月別台数グラフ ####--------------------------------------------
Metis_MF3_Num.of.units_by.manufacturing.date <- 
  tibble(
    field = c("ADF","フィニッシャー","バンク"),
    field.name = c("ADF","FIN","BANK"),
    treatment.area = list(list("ADF部"),
                          list("ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部","ﾊﾟﾝﾁ部","ｽﾃｰﾌﾟﾙ部"),
                          list("第3給紙","第4給紙","第5給紙","第()給紙 ","駆動部(OP給紙)")),
    choice.mt = list(list("COOK-C","SINAI-H"),
                     list("AMUR-C中綴じ","AMUR-C(HY)","VOLGA-E"),
                     list("CANARIA-D","GOREE-D","CUBA-C")),
    # legend.position = list(list(0.80, 0.95),
    #                        list(0.75, 0.95),
    #                        list(0.80, 0.95)),
    y.breaks = list(list(seq(0, 7000, 1000)),
                    list(seq(0, 1300, 250)),
                    list(seq(0, 8000, 1000))),
    graph.width = c(10, 10, 10),
    graph.height = c(6, 6, 6),
    hjust = c(5000,1000,7000)
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
          sel_month = "製造年月",
          date.name = "Manufacturing_date",
          file.save = FALSE,
          write.file_path = str_c("./tsv_data/Metis_MF3_",field.name,"_num_by.Manufacturing.tsv"),
          graph_title = str_c("Metis-MF3 ",field.name," 製造年月別台数"),
          legend.position = c(0.75, 0.95),
          breaks = seq(as.Date("2018-10-01"), as.Date("2022-10-01"), by="6 month"),
          y.breaks = y.breaks,
          graph.save = TRUE,
          save.graph_path = str_c("./PDF/Metis-MF3_",field.name,"_製造年月別台数-2.pdf"),
          graph.width = graph.width, 
          graph.height = graph.height)),
    df.data = 
      list(return_Create.graph_for.number_by.month[["df"]]),
    graph =
      list(return_Create.graph_for.number_by.month[["graph"]])
  ) %>% 
  ungroup()

#### オブジェクト保存 ####------------------------------------
saveRDS(Metis_MF3_Num.of.units_by.manufacturing.date, file = "./output/Metis_MF3_Num.of.units_by.manufacturing.date.rds")
#### オブジェクト読込 ####------------------------------------
Metis_MF3_Num.of.units_by.manufacturing.date <- readRDS("./output/Metis_MF3_Num.of.units_by.manufacturing.date.rds")

# フォント設定
# windowsFonts(Japan1GothicBBB = windowsFont("Japan1GothicBBB"))
par(family="Noto Sans")

# 要素の抽出
Metis_MF3_Num.of.units_by.manufacturing.date %>% 
  pluck("graph", 1) %>% 
  plotly::ggplotly()
Metis_MF3_Num.of.units_by.manufacturing.date %>% 
  pluck("df.data", 1)
# グラフの合成描画
Metis_MF3_Num.of.units_by.manufacturing.date %>% 
  pull(graph) %>% 
  patchwork::wrap_plots(ncol = 1)
# リストからの要素抽出
ret.obj.1 <- 
  Metis_MF3_Num.of.units_by.manufacturing.date %>% 
  pluck("return_Create.graph_for.number_by.month", 1)
ret.obj.1[["graph"]]
ret.obj.1[["df"]]
