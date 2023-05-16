####ファイル読込####
Metis_MIF_2211 <- read_tsv("./tsv_data/Metis_MIF_2211.tsv")
Metis_MF3_EM_ADF_by_Maintenance_date <- read_tsv("./tsv_data/Metis_MF3_EM_ADF_by_Maintenance_date.tsv")
Metis_MF3_EM_FIN_by_Maintenance_date <- read_tsv("./tsv_data/Metis_MF3_EM_FIN_by_Maintenance_date.tsv")

# ADF
Metis_MF3_ADF_number_by_Due_date <- 
  Create.graph_for.number_by.month(df = Metis_MIF_2211,
                                   field = "ADF",
                                   choice = c("COOK-C","SINAI-H"),
                                   sel_month = "納品年月日",
                                   date.name = "Due_date",
                                   file.save = FALSE,
                                   write.file_path = "./tsv_data/Metis_MF3_ADF_number_by_Due_date.tsv",
                                   graph_title = "Metis-MF3 ADF 製造年月別台数",
                                   legend.position = c(0.75, 0.95),
                                   breaks = seq(as.Date("2018-10-01"), as.Date("2022-10-01"), by="6 month"),
                                   graph.save = FALSE,
                                   save.graph_path='./PDF/Metis-MF3_ADF_製造年月別台数.pdf',
                                   graph.width = 10, 
                                   graph.height = 6)

Create.graph_for.EM_number(df.num = Metis_MF3_ADF_number_by_Due_date,
                           df.EM = Metis_MF3_EM_ADF_by_Maintenance_date,
                           field = "ADF",
                           choice = c("`COOK-C`","`SINAI-H`"),
                           choice2 = c("COOK_C_EM","SINAI_H_EM"),
                           choice3 = c("COOK_C_EM.rate","SINAI_H_EM.rate"),
                           X_date = seq(as.Date("2019-01-01"), as.Date("2022-11-30"), by = "day"),
                           file.save = FALSE,
                           write.file_path = "./tsv_data/Metis_MF3_ADF_number_by_date.tsv",
                           graph_title = "Metis-MF3_ADF EM件数",
                           legend.position = c(0.85, 0.95),
                           x.breaks = seq(as.Date("2019-02-01"), as.Date("2022-11-01"), by="6 month"),
                           y.breaks = seq(0, 50, 10),
                           ylim = c(0, 50),
                           graph.save = FALSE,
                           save.graph_path = './PDF/Metis-MF3_ADF_EM.rate-dayly.pdf',
                           graph.width = 10, 
                           graph.height = 6)

# FIN
Metis_MF3_FIN_number_by_Due_date <- 
  Create.graph_for.number_by.month(df = Metis_MIF_2211,
                                   field = "フィニッシャー",
                                   choice = c("AMUR-C中綴じ","AMUR-C(HY)","VOLGA-E"),
                                   sel_month = "納品年月日",
                                   date.name = "Due_date",
                                   file.save = FALSE,
                                   write.file_path = "./VOLGA/tsv_data/Metis_MF3_FIN_number_by_Due_date.tsv",
                                   graph_title = "Metis-MF3 FIN 製造年月別台数",
                                   legend.position = c(0.75, 0.95),
                                   breaks = seq(as.Date("2018-10-01"), as.Date("2022-10-01"), by="6 month"),
                                   graph.save = FALSE,
                                   save.graph_path='./VOLGA/PDF/Metis-MF3_FIN_製造年月別台数.pdf',
                                   graph.width = 10, 
                                   graph.height = 6)

Create.graph_for.EM_number(
  df.num = Metis_MF3_FIN_number_by_Due_date,
  df.EM = Metis_MF3_EM_FIN_by_Maintenance_date,
  field = "フィニッシャー",
  choice = c("`AMUR-C中綴じ`","`AMUR-C(HY)`","`VOLGA-E`"),
  choice2 = c("AMUR_C_EM","AMUR_CHY_EM","VOLGA_E_EM"),
  choice3 = c("AMUR_C_EM.rate","AMUR_CHY_EM.rate","VOLGA_E_EM.rate"),
  X_date = seq(as.Date("2019-01-01"), as.Date("2022-11-30"), by = "day"),
  file.save = FALSE,
  write.file_path = "./VOLGA/tsv_data/Metis_MF3_FIN_Nmber_by_date.tsv",
  graph_title = "Metis-MF3_FIN EM件数",
  legend.position = c(0.85, 0.95),
  x.breaks = seq(as.Date("2019-02-01"), as.Date("2022-11-01"), by="6 month"),
  y.breaks = seq(0, 10, 2),
  ylim = c(0, 10),
  graph.save = FALSE,
  save.graph_path = './VOLGA/PDF/Metis-MF3_FIN_EM-dayly.pdf',
  graph.width = 10, 
  graph.height = 9
)
