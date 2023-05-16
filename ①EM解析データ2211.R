####ファイル読込####
Metis_sheet1 <- read_tsv("./tsv_data/Metis_sheet1.tsv")
Metis_sheet1_2 <- read_tsv("./tsv_data/Metis_sheet1_2.tsv")
Metis_sheet1_3 <- read_tsv("./tsv_data/Metis_sheet1_3.tsv")

####エクセルファイルの読み込み####
Metis_sheet1_2211 <- 
  read_excel("./Excel/保守データ/Metis-MF3保守データ_(2021.12月-2022.11月).xlsx",
             sheet = 1,
             skip = 1) # A tibble: 96,487 × 147
Metis_sheet2_2211 <- 
  read_excel("./Excel/保守データ/Metis-MF3保守データ_(2021.12月-2022.11月).xlsx",
             # sheet="□データ貼り付け　②周辺機データ", 
             sheet = 2,
             skip = 0)
Metis_sheet3_2211 <- 
  read_excel("./Excel/保守データ/Metis-MF3保守データ_(2021.12月-2022.11月).xlsx",
             # sheet="□データ貼り付け　③機器", 
             sheet = 3,
             skip = 0)

# 初回訪問区分他のtype変更
Metis_sheet1_3 <- 
  Metis_sheet1_3 %>% 
  mutate(
    初回訪問区分 = as.logical(初回訪問区分),
    ｵﾌﾟｼｮﾝ機種略号= as.logical(ｵﾌﾟｼｮﾝ機種略号),
    ｵﾌﾟｼｮﾝ機番= as.logical(ｵﾌﾟｼｮﾝ機番)
  )
Metis_sheet1_2211 <- 
  Metis_sheet1_2211 %>% 
  mutate(
    初回訪問区分 = as.logical(初回訪問区分),
    ｵﾌﾟｼｮﾝ機種略号= as.logical(ｵﾌﾟｼｮﾝ機種略号),
    ｵﾌﾟｼｮﾝ機番= as.logical(ｵﾌﾟｼｮﾝ機番)
  )

####ファイル保存####
# write_tsv(Metis_sheet1_2211, "./tsv_data/Metis_sheet1_2211.tsv")
# write_tsv(Metis_sheet2_2211, "./tsv_data/Metis_sheet2_2211.tsv")
# write_tsv(Metis_sheet3_2211, "./tsv_data/Metis_sheet3_2211.tsv")

####ファイル読込####
Metis_sheet1_2211 <- read_tsv("./tsv_data/Metis_sheet1_2211.tsv") # A tibble: 73,486 × 147
Metis_sheet2_2211 <- read_tsv("./tsv_data/Metis_sheet2_2211.tsv") # A tibble: 977,227 × 19
Metis_sheet3_2211 <- read_tsv("./tsv_data/Metis_sheet3_2211.tsv") # A tibble: 159,997 × 32

Metis_sheet1234 <- 
  bind_rows(Metis_sheet1, Metis_sheet1_2, Metis_sheet1_3, Metis_sheet1_2211)

Metis_sheet1a <- 
  Metis_sheet1234 %>% 
  mutate(
    機種略機番 = paste(機種略号, 機番, sep="")
  ) %>% 
  distinct_all()

# Metis_sheet2に重複行あり
Metis_sheet2a <-
  Metis_sheet2_2211 %>%
  distinct_all()

# 結合
Metis_sheet1a2join <- left_join(Metis_sheet1a, Metis_sheet2a, by="機種略機番") # A tibble: 1,535,448 × 166
Metis_sheet1a2join %>% distinct_all() 

# colnames(Metis_sheet1a)
# colnames(Metis_sheet1a2join)
# 
# Metis_sheet1a2join$納入日.x
# Metis_sheet1a2join$納入日.y
# sum(Metis_sheet1a2join_a$納入日 - Metis_sheet1a2join$納入日.x)

# 列の選定
Metis_MF3 <- Metis_sheet1a2join %>% 
  select(機種略号.x,機種略機番,機種ｺｰﾄﾞ,訪問区分,
         年月度,製造年月,納入日.x,稼動月,保守実施日,
         CE作業時間,ｺｰﾙ,EM,現象,現象SC名称,処置場所,...17,周辺機名) %>%
  filter(EM == 1) %>% 
  distinct_all() # A tibble: 516,414 × 17

# 日付へ変換
Metis_MF3$年月度 <- 
  paste(str_sub(Metis_MF3$年月度, start=1, end=4),
        str_sub(Metis_MF3$年月度, start=5, end=-1),("01"),
        sep="-") %>% 
  as.POSIXct() %>% 
  as.Date(tz = "Asia/Tokyo")

Metis_MF3$製造年月 <- 
  paste(str_sub(Metis_MF3$製造年月, start=1, end=4),
        str_sub(Metis_MF3$製造年月, start=5, end=-1),("01"),
        sep="-") %>% 
  as.POSIXct() %>% 
  as.Date(tz = "Asia/Tokyo")

Metis_MF3$納入日.x <- 
  paste(str_sub(Metis_MF3$納入日.x, start=1, end=4),
        str_sub(Metis_MF3$納入日.x, start=5, end=6),
        str_sub(Metis_MF3$納入日.x, start=7, end=-1),
        sep="-") %>% 
  as.POSIXct() %>% 
  as.Date(tz = "Asia/Tokyo")

Metis_MF3$保守実施日 <- 
  paste(str_sub(Metis_MF3$保守実施日, start=1, end=4),
        str_sub(Metis_MF3$保守実施日, start=5, end=6),
        str_sub(Metis_MF3$保守実施日, start=7, end=-1),
        sep="-") %>% 
  as.POSIXct() %>% 
  as.Date(tz = "Asia/Tokyo")

# 列名
colnames(Metis_MF3)
Metis_MF3_2211 <- 
  rename(Metis_MF3, 
         Model_abbreviation = 機種略号.x,
         Machine_numbers = 機種略機番,
         Model_code = 機種ｺｰﾄﾞ,
         Visit_classification = 訪問区分,
         Year_and_month = 年月度,
         Manufacturing_date = 製造年月,
         Due_date = 納入日.x,
         Working_month = 稼動月,
         Maintenance_date = 保守実施日,
         CE_Working_hours = CE作業時間,
         Call = ｺｰﾙ,
         Phenomenon = 現象,
         SC_name = 現象SC名称,
         Treatment_location = 処置場所,
         Peripheral_machine = ...17,
         Peripheral_name = 周辺機名
  )

# 概要
summary(Metis_MF3_2211)

Metis_MF3_2211 %>% 
  distinct(Phenomenon)

Metis_MF3_2211 %>% 
  distinct(SC_name)

Metis_MF3_2211 %>% 
  distinct(Peripheral_name)

# ディメンジョン
dim(Metis_MF3_2211)

# 製造経過月数・稼働月数
Metis_MF3_2211 <- 
  Metis_MF3_2211 %>% 
  rowwise() %>%
  mutate(
    Elapsed_mf_months = 
      (length(seq(as.Date("2018/10/01"), as.Date(Manufacturing_date), "month"))-1),
    Working_months = 
      (length(seq(as.Date(Due_date), as.Date(Maintenance_date), "month"))-1)
  ) %>% 
  ungroup()
# 重複行の確認
tmp <- 
Metis_MF3_2211 %>% 
  group_by(Model_abbreviation,
           Machine_numbers,
           Model_code,
           Visit_classification,
           Year_and_month,
           Manufacturing_date,
           Due_date,
           Working_month,
           Maintenance_date,
           CE_Working_hours,
           Call,
           EM,
           Phenomenon,
           SC_name,
           Treatment_location,
           Peripheral_machine,
           Peripheral_name,
           Elapsed_mf_months,
           Working_months) %>%  
  filter(n() > 1)

# 重複行の削除
Metis_MF3_2211 <- 
  Metis_MF3_2211 %>%
  distinct_all()

# 機器データの日付変換　NAのある最新訪問日は変換不可
names(Metis_sheet3_2211)

Metis_MIF_2211 <- 
  Date_conversion("Metis_sheet3_2211", col_list = c("納品年月日","製造年月","納入月","リンク日"))

####ファイル保存####
write_tsv(Metis_MF3_2211, "./tsv_data/Metis_MF3_2211.tsv") # A tibble: 516,414 × 19
write_tsv(Metis_MIF_2211, "./tsv_data/Metis_MIF_2211.tsv") # A tibble: 159,997 × 32






