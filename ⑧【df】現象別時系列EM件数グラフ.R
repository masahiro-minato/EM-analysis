# ファイル読込
Metis_MF3_2211 <- read_tsv("./tsv_data/Metis_MF3_2211.tsv") # tibble: 516,414 × 20
Metis_MIF_2211 <- read_tsv("./tsv_data/Metis_MIF_2211.tsv") # tibble: 159,997 × 32

print(n=90,Metis_MF3_2211 %>% 
        distinct(Treatment_location))
print(n=90,Metis_MF3_2211 %>% 
        distinct(Peripheral_name))

names(Metis_MF3_2211)

#### EM現象件数グラフ ------------------------------------------------
Metis_MF3_EM.phenomenon <- 
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
    legend_position.1 = list(list("right"),
                             list("right"),
                             list(0.02, 0.99),
                             list(0.02, 0.99),
                             list(0.02, 0.99),
                             list(0.02, 0.99),
                             list(0.05, 0.99),
                             list(0.05, 0.99)),
    legend_position.2 = list(list(0.05, 0.99),
                             list(0.05, 0.99),
                             list(0.55, 0.99),
                             list(0.07, 0.99),
                             list(0.60, 0.99),
                             list(0.05, 0.99),
                             list(0.05, 0.99),
                             list(0.65, 0.99)),
    legend_justification.1 = list(list('top'),
                                  list('top'),
                                  list(0, 1),
                                  list(0, 1),
                                  list(0, 1),
                                  list(0, 1),
                                  list(0, 1),
                                  list(0, 1)),
    legend_justification.2 = list(list(0, 1),
                                  list(0, 1),
                                  list(0, 1),
                                  list(0, 1),
                                  list(0, 1),
                                  list(0, 1),
                                  list(0, 1),
                                  list(0, 1)),
    legend_ncol.1 = c(3,2,3,2,3,3,1,2),
    legend_ncol.2 = c(6,6,3,7,3,6,1,2),
    graph.width.1 = c(20, 18, 14, 14, 14, 14, 14, 14),
    graph.width.2 = c(14, 14, 14, 14, 14, 14, 14, 14),
    graph.width.3 = c(12, 12, 12, 12, 12, 12, 12, 12),
    graph.height.1 = c(10, 10, 10, 10, 10, 10, 10, 10),
    graph.height.2 = c(10, 10, 10, 10, 10, 10, 10, 10)
  ) %>% 
  rowwise() %>% 
  mutate(
    func.Create.graph_for.number_by.month =
      list(Create.graph_for.number_by.month),
    func.Create_Metis_MF3_SCname=
      list(Create_Metis_MF3_SCname),
    func.geom_bar_SCname =
      list(geom_bar_SCname),
    Metis_MF3_number_by_Maintenance_month = 
      list(
        func.Create.graph_for.number_by.month(
          df = Metis_MIF_2211,
          field = field_1,
          choice = theme.name,
          sel_month = "納入月",
          date.name = "Maintenance_month")$df %>% 
          mutate(dplyr::across(where(is.numeric), cumsum))
      ),
    SCname_Maintenance_date = 
      list(
        Create_Metis_MF3_SCname(
          df = Metis_MF3_2211,
          T_location = T_location, 
          P_name = theme.name,
          Choices = "Maintenance_month",
          num = 1) %>% 
          left_join(Metis_MF3_number_by_Maintenance_month, by="Maintenance_month") %>% 
          mutate(
            SC_sum_rate = Peripheral_name_SC_sum/eval(parse(text =str_c("`",theme.name,"`")))*100)
      ),
    # EM現象別月次件数
    g_SC_Maintenance_date =
      list(
        geom_bar_SCname(
          df = SCname_Maintenance_date,
          x = "Maintenance_month",
          y = "Peripheral_name_SC_sum",
          y.sum = "Peripheral_name_EM",
          fill = "SC_name_num",
          legend_name = "現象SC名称",
          x_labels = "年月",
          y_labels = "件数",
          y.breaks = NA,
          ylim = NA,
          x.breaks = seq(as.Date("2019-01-01"), as.Date("2022-11-01"), by="6 month"),
          labels = date_format("%Y/%m"),
          title = str_c("Metis-MF3 ",theme.name," EM現象別月次件数 (2019.2～2022.11)"),
          file_path = str_c("./PDF/Metis-MF3_",theme.name,"_EM現象別件数(2019.2～2022.11)-3.pdf"),
          f_wrap = FALSE, 
          ncol = 5,
          g_save = TRUE,
          w = graph.width.1,
          h = graph.height.1,
          legend_position = legend_position.1,
          legend_justification = legend_justification.1,
          legend_ncol = legend_ncol.1
        )
      ),
    # EM現象別月次件数 個別グラフ
    g.wrap_SC_Maintenance_date =
      list(
        geom_bar_SCname(
          df = SCname_Maintenance_date,
          x = "Maintenance_month",
          y = "Peripheral_name_SC_sum",
          y.sum = "Peripheral_name_EM",
          fill = "SC_name_num",
          legend_name = "現象SC名称",
          x_labels = "年月",
          y_labels = "件数",
          y.breaks = NA,
          ylim = NA,
          x.breaks = seq(as.Date("2019-01-01"), as.Date("2022-11-01"), by="1 year"),
          labels = date_format("%Y"),
          title = str_c("Metis-MF3 ",theme.name," EM現象別月次件数 (2019.2～2022.11)"),
          file_path = str_c("./PDF/Metis-MF3_",theme.name,"_EM現象別件数(2019.2～2022.11)一覧-3.pdf"),
          f_wrap = TRUE,
          ncol = 5,
          g_save = TRUE,
          w = graph.width.3,
          legend_position = legend_position.1,
          legend_justification = legend_justification.1,
          legend_ncol = legend_ncol.1
        )
      ),
    # EM現象別月次比率
    g_SC.rate_Maintenance_date =
      list(
        geom_bar_SCname(
          df = SCname_Maintenance_date,
          x = "Maintenance_month",
          y = "SC_sum_rate",
          y.sum = NA,
          fill = "SC_name_num",
          legend_name = "現象SC名称",
          x_labels = "年月",
          y_labels = "台当たり比率(%)  件数/納品台数累計x100",
          y.breaks = seq(0, 1.5, 0.5),
          ylim = c(0, 1.5),
          x.breaks = seq(as.Date("2019-01-01"), as.Date("2022-11-01"), by="6 month"),
          labels = date_format("%Y/%m"),
          title = str_c("Metis-MF3 ",theme.name," EM現象別月次比率 (2019.2～2022.11)"),
          file_path = str_c("./PDF/Metis-MF3_",theme.name,"_EM現象別比率(2019.2～2022.11)-3.pdf"),
          f_wrap = FALSE, 
          ncol = 5,
          g_save = TRUE,
          w = graph.width.2,
          h = graph.height.2,
          legend_position = legend_position.2,
          legend_justification = legend_justification.2,
          legend_ncol = legend_ncol.2
        )
      ),
    # EM現象別月次比率 個別グラフ
    g.wrap_SC.rate_Maintenance_date =
      list(
        geom_bar_SCname(
          df = SCname_Maintenance_date,
          x = "Maintenance_month",
          y = "SC_sum_rate",
          y.sum = NA,
          fill = "SC_name_num",
          legend_name = "現象SC名称",
          x_labels = "年月",
          y_labels = "台当たり比率(%)  件数/納品台数累計x100",
          y.breaks = NA,
          ylim = NA,
          x.breaks = seq(as.Date("2019-01-01"), as.Date("2022-11-01"), by="1 year"),
          labels = date_format("%Y"),
          title = str_c("Metis-MF3 ",theme.name," EM現象別月次比率 (2019.2～2022.11)"),
          file_path = str_c("./PDF/Metis-MF3_",theme.name,"_EM現象別比率(2019.2～2022.11)一覧-3.pdf"),
          f_wrap = TRUE, 
          ncol = 5,
          g_save = TRUE, 
          w = graph.width.3,
          legend_position = legend_position.1,
          legend_justification = legend_justification.1,
          legend_ncol = legend_ncol.1
        )
      )
  ) %>% 
  ungroup()

saveRDS(Metis_MF3_EM.phenomenon, file = "./output/Metis_MF3_EM.phenomenon.rds")

Metis_MF3_EM.phenomenon %>% 
  pluck("g_SC.rate_Maintenance_date", 3)

#### 処置月別EM件数 -----------------------------------
SCname_Maintenance_date <- 
  Create_Metis_MF3_SCname(
    df = Metis_MF3_2211,
    T_location = "ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部", 
    P_name = "VOLGA-E",
    Choices = "Maintenance_month",
    num = 1) %>% 
  left_join(Metis_MF3_FIN_number_by_Maintenance_month, by="Maintenance_month") %>% 
  mutate(
    SC_sum_rate = Peripheral_name_SC_sum/`VOLGA-E`)

# EM総数　1734
sum(SCname_Maintenance_date$Peripheral_name_SC_sum)

# 時系列グラフ
# 現象件数
g_VOLGA_SC_Maintenance_date <- 
  geom_bar_SCname(df = "SCname_Maintenance_date",
                  x = "Maintenance_month",
                  y = "Peripheral_name_SC_sum",
                  fill = "SC_name_num",
                  legend_name = "現象SC名称",
                  x_labels = "年月",
                  y_labels = "件数",
                  title = "Metis-MF3 VOLGA-E EMにおける現象件数 (2019.2～2022.11)",
                  file_path = "./PDF/Metis-MF3-VOLGA_EMの時系列現象件数(2019.2～2022.11)一覧-3.pdf",
                  # f_wrap = TRUE, ncol = 2,
                  # g_save = TRUE, w = 16, h = 12,
                  f_wrap = FALSE, ncol = 5,
                  g_save = FALSE, w = 16, h = 14,
                  # g_save = F, w = 30, h = 15,
                  legend_position = c(0.05, 0.97),
                  legend_justification = c(0, 1)
  )
#現象比率
g_VOLGA_SC_Maintenance_date_rate <- 
  geom_bar_SCname(df = "SCname_Maintenance_date",
                  x = "Maintenance_month",
                  y = "SC_sum_rate",
                  fill = "SC_name_num",
                  legend_name = "現象SC名称",
                  x_labels = "年月",
                  y_labels = "台当たり比率（件数/製造台数）",
                  title = "Metis-MF3 VOLGA-E EMにおける現象比率 (2019.2～2022.11)",
                  file_path = "./PDF/Metis-MF3-VOLGA_EMの時系列現象比率(2019.2～2022.11)一覧-3.pdf",
                  # f_wrap = TRUE, ncol = 2,
                  # g_save = TRUE, w = 16, h = 12,
                  f_wrap = FALSE, ncol = 5,
                  g_save = FALSE, w = 16, h = 14,
                  # g_save = F, w = 30, h = 15,
                  legend_position = c(0.47, 0.97),
                  legend_justification = c(0, 1)
  )