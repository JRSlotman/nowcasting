#### PREAMBLE ####
options(readr.show_types = FALSE)
if("librarian" %in% rownames(installed.packages()) == FALSE) {install.packages("librarian")}
librarian::shelf(here, tidyverse, readxl, lubridate, countrycode, fredr, furrr, zoo, broom, WDI, gam)
fredr_set_key(read.table(here("Data", "fred_key.txt"))[[1]]) # Obtained from FRED website 

#### Read and combine data ####

if(T){
  afr_list <- c("AGO","BDI","BEN","BFA","BWA","CAF","CIV","CMR","COD","COG","COM","CPV","DJI","DZA","EGY","ERI","ETH","GAB","GHA","GIN","GMB","GNB","GNQ","KEN","LBR","LBY","LSO","MAR","MDG","MLI","MOZ","MRT","MUS","MWI","NAM","NER","NGA","RWA","SDN","SEN","SLE","SOM","SSD","STP","SWZ","SYC","TCD","TGO","TUN","TZA","UGA","ZAF","ZMB","ZWE")
  
  zscore <- function(x){(x-mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)}
  yoy <- function(x){100*(x-dplyr::lag(x,12))/dplyr::lag(x,12)}
  ydiff <- function(x){x-dplyr::lag(x,12)}
  mdiff <- function(x){x-dplyr::lag(x,1)}
  `%notin%` <- Negate(`%in%`)
  
  #### World Bank ####

  wb_long <- read_csv(here("Data","worldbank_long.csv"), guess_max = 50000, show_col_types = FALSE) %>%
    select(-country) %>% 
    complete(iso, date = seq(ymd("1992-12-1"), max(date), by = "months")) %>%
    mutate(across(where(is.numeric), 
                  function(x){ifelse(date > today() | x == 0, NA, x)})) %>%  # These are WB estimates that we are actually trying to nowcast ourselves (date > today) or that are problematic (x == 0)
    group_by(iso) %>%
    mutate(cpi_aroll = rollmean(cpi_m, k = 12, align = "right", fill = NA), .keep = "unused") %>% 
    mutate(cpi_a_yoy = yoy(coalesce(cpi_a, cpi_aroll)), .keep = "unused") %>% 
    mutate(export_q = coalesce(export_m+lag(export_m,1)+lag(export_m,2), export_q),
           import_q = coalesce(import_m+lag(import_m,1)+lag(import_m,2), import_q),
           .keep = "unused") %>%
    mutate(
          export_a = coalesce(export_q+lag(export_q,3)+lag(export_q,6)+lag(export_q,9), export_a),
          import_a = coalesce(import_q+lag(import_q,3)+lag(import_q,6)+lag(import_q,9), import_a),
          .keep = "unused") %>% 
    mutate(gdp_a = ifelse(is.na(gdp_q) | is.na(lag(gdp_q,9)), NA, gdp_a), # The first and last observation are sometimes wrong
           gdp_a = coalesce(gdp_q+lag(gdp_q,3)+lag(gdp_q,6)+lag(gdp_q,9), gdp_a)) %>% 
    mutate(across(!ends_with("yoy") & !date, list(yoy = yoy))) %>% 
    mutate(gdp_a_yoy = coalesce(gdp_a_yoy,
                               100*((gdp_q + lag(gdp_q, 3) + lag(gdp_q, 6) + lag(gdp_q, 9))/(lag(gdp_q,12) + lag(gdp_q, 15) + lag(gdp_q, 18) + lag(gdp_q, 21))-1))) %>%
    ungroup() %>% 
    select(iso, date, unemployment_m, ends_with("yoy"), -unemployment_m_yoy, -gdp_q_yoy, -export_a_yoy, -import_a_yoy)
  
  #### FRED ####
  # Select indicators from: https://fred.stlouisfed.org/
  
  fred_indicators <- c("PALLFNFINDEXM", # Global Price Index of All Commodities
                       "MCOILBRENTEU", # Crude Oil Prices: Brent - Europe 
                       #"WTISPLC", #  Spot Crude Oil Price: West Texas Intermediate 
                       "PNGASEUUSDM", # Global price of Natural gas, EU
                       "PCOPPUSDM", #  Global price of Copper
                       "PWHEAMTUSDM", # Global price of Wheat
                       "PFOODINDEXM", # Global price of Food index
                       "PCU325311325311", # Producer Price Index by Industry: Nitrogenous Fertilizer Manufacturing
                       "WPS0652", # Producer Price Index by Commodity: Chemicals and Allied Products: Fertilizer Materials
                       "PRAWMINDEXM", # Global price of Agr. Raw Material Index
           #            "PNRGINDEXM", # Global price of Energy index
                       "IR14270", # Import Price Index (End Use): Nonmonetary Gold
                       "IGREA") #  Index of Global Real Economic Activity
                          
  fred <- map_df(fred_indicators, fredr) %>%
    complete(date = seq(min(date), max(date), by = "months")) %>%
    select(date, series_id, value) %>% 
    pivot_wider(names_from = series_id, values_from = value) %>% 
    rename(global_commodity_price_index = PALLFNFINDEXM,
           global_brent_price = MCOILBRENTEU,
           global_gas_price = PNGASEUUSDM,
           global_copper_price = PCOPPUSDM,
           global_wheat_price = PWHEAMTUSDM,
           global_food_price = PFOODINDEXM,
           global_nitro_fertilizer_price = PCU325311325311,
           global_fertilizer_materials_price = WPS0652,
           global_agr_raw_materials_price = PRAWMINDEXM,
           usa_gold_import_price = IR14270,
           global_economic_activity = IGREA) %>%
    mutate(across(where(is.numeric) & !global_economic_activity, list(yoy = yoy)), .keep = "unused")
  
  #### World Development Indicators ####
  
  wdi <- read_csv(here("Data","wdi.csv"), show_col_types = FALSE) %>%
    complete(iso3c, date = seq(min(date), today(), by = "months")) %>% 
    filter(iso3c %in% afr_list) %>% 
    rename(iso = iso3c) %>%
    mutate(exchange_rate_wdi_yoy = yoy(exchange_rate),
            food_production_yoy = yoy(food_production_wdi),
            undernourishment_yoy = yoy(undernourishment),
           .keep = "unused")
  
  #### Haver ####
  haver <- read_csv(here("Data","haver.csv"), guess_max = 18000, show_col_types = FALSE) %>%
    filter(iso %in% afr_list) %>% 
    complete(iso, date = seq(min(date), max(date), by = "months")) %>%
    mutate(industrial_production_yoy = coalesce(yoy(industrial_production), industrial_production_yoy), .keep = "unused") %>% 
    mutate(across(-any_of(c("date", "iso", "industrial_production_yoy", "unemployment", "consumer_confidence", "business_confidence", "capacity_utilization", "interest_rate", "manufacturing_pmi")), 
                  list(yoy = yoy)), .keep = "unused") %>% 
    select(-tourist_arrivals_yoy)
  
  #### AfDB ####
  afdb_cpi <- read_csv(here("Data","afdb_cpi.csv"), guess_max = 15000, show_col_types = FALSE) %>% 
    complete(iso, date = seq(min(date), max(date), by = "months")) %>%
    select(-alcoholic_beverages_tobacco_and_narcotics) %>% 
    mutate(cpi_yoy_afdb = yoy(consumer_price_index_all_items), 
           food_price_yoy = yoy(food_and_nonalcoholic_beverages),
           .keep = "unused")
  
  #### FAO ###
  
  fao_production <- read_csv(here("Data","fao.csv"), guess_max = 5000, show_col_types = FALSE) %>% 
    complete(iso, date = seq(min(date), max(date), by = "months")) %>%
    mutate(across(where(is.numeric), list(yoy = yoy)), .keep = "unused")
  
  #### Join data ####
  
  data <- wdi %>%
    left_join(wb_long, by = c("date", "iso")) %>%
    left_join(haver, by = c("iso", "date"), suffix = c("", "_haver")) %>%
    left_join(afdb_cpi, by = c("iso", "date")) %>% 
    left_join(fao_production, by = c("iso", "date")) %>% 
    left_join(fred, by = "date") %>% 
    mutate(across(where(is.double), na_if, NaN),
           across(where(is.double), na_if, -Inf),
           across(where(is.double), na_if, -Inf)) %>% 
    mutate(gdp_yoy = coalesce(gdp_a_yoy, gdp_yoy_haver, gdp_yoy), 
           unemployment = coalesce(unemployment_m, unemployment_a_wdi, unemployment),
           unemployment = unemployment - lag(unemployment, 12),
           cpi_yoy = coalesce(cpi_a_yoy, cpi_a_yoy_wdi, inflation_yoy, cpi_yoy_afdb),
           core_cpi_yoy = coalesce(core_cpi_m_yoy, core_inflation_yoy),
           exchange_rate_yoy = coalesce(exchange_rate_yoy, exchange_rate_wdi_yoy),
           embi_yoy = coalesce(embi_m_yoy, embi_yoy),
           industrial_production_yoy = coalesce(industry_m_yoy, industrial_production_yoy),
           food_production_yoy = coalesce(food_production_yoy, food_production_index_yoy),
           retail_sales_yoy = coalesce(retail_sales_yoy, retail_sales_m_yoy),
           stock_markets_yoy = coalesce(stock_markets_m_yoy, stock_market_yoy),
           .keep = "unused") %>% 
    relocate(all_of(c("food_price_yoy", "gdp_yoy")), .after = date) #%>% 
    # pivot_longer(cols = -c("date", "iso")) %>% 
    # mutate(name = str_c(iso, name, sep = "_"), .keep = "unused") %>% 
    # pivot_wider() %>% 
    # left_join(fred_complete, by = "date") %>% 
    # select(where(function(x)!all(is.na(x))))
  
  return(data)
}

#### PCA and Smoothing ####
#
# preprocesss_values_pca <- preProcess(fred_complete %>% select(-global_economic_activity), method = c("pca"))
# fred_pca <- stats::predict(preprocesss_values_pca, fred_complete)
# #heatmap(preprocesss_values_pca$rotation, Rowv = NA, Colv = NA, scale = "none")
# 
# my_loess <- function(groupdata, var){
#   
#   foo <- groupdata %>% filter(!is.na(!!ensym(var)))
#   if(dim(foo)[1] == 0){return(NULL)}
# 
#   y <- foo %>% pull(.data[[var]])
# 
#   loess(y ~ as.numeric(date),
#         data = foo,
#         degree = 1,
#         span = max(12/dim(foo)[1],0.05)
#         )
#   # Span is proportion of observations used for smoothing. We should adjusts it to data availability to ensure it roughly covers a similar period for each country (e.g. half a year).
#   # Function will throw an error when span is too low. May need to be refined.
# }
# 
# loessfit <- function(var, df){
#   
#   print(paste("Fitting", var))
#   
#   if("iso" %in% colnames(df)){
#     df %>%
#       nest(groupdata = -iso) %>%
#       mutate(model = map2(.x = groupdata, .y = var, .f = my_loess),
#              fit = map(model, augment)) %>%
#       unnest(fit) %>%
#       select(-c("model","groupdata")) %>%
#       mutate(date = as.Date(`as.numeric(date)`),
#              var = paste(var,"fit", sep = "_"),
#              fit = .fitted,
#              .keep = "unused")
#   }else{
#     df %>%
#       nest(groupdata = colnames(df)) %>%
#       mutate(model = map2(.x = groupdata, .y = var, .f = my_loess),
#              fit = map(model, augment)) %>%
#       unnest(fit) %>%
#       select(-c("model","groupdata")) %>%
#       mutate(date = as.Date(`as.numeric(date)`),
#              var = paste(var,"fit", sep = "_"),
#              fit = .fitted,
#              .keep = "unused")
#   }
# }
# 
# fit_variables <- c("retail_sales_yoy",
#                    "industrial_production_yoy",
#                    "manufacturing_pmi",
#                    "exchange_rate_yoy",
#                    "msci_yoy",
#                    "stock_markets_yoy",
#                    colnames(fred_pca)[2:ncol(fred_pca)])
# 
# loessfit(fit_variables[4], data) %>% filter(iso == "ZAF") %>% plot()
# 
# loessfit <- map_df(fit_variables,
#                    loessfit, df = data) %>%
#   select(-y) %>%
#   pivot_wider(names_from = "var", values_from = "fit")
# 
# data <- data %>%
#           left_join(loessfit, by = c("date"))
# 
#loessfit %>% filter(iso == "AGO") %>%  ggplot(aes(date, nitro_fertilizer_price_yoy_fit)) + geom_line()

# wigglyvars <- c("leading_economic_index_ydiff",
#                 "wage_growth_ydiff",
#                 #"composite_pmi",
#                 "services_pmi", "manufacturing_pmi",
#                 #"crude_oil_production_yoy",
#                 #"wui",
#                 "gas_yoy",
#                 "wti_yoy",
#                 "brent_yoy",
#                 "job_vacancies_yoy",
#                 "construction_output_ydiff",
#                 "stock_markets_m_yoy",
#                 "private_sector_credit_yoy",
#                 "embi_m_yoy",
#                 "s&p_gsci_yoy",
#                 #"copper_yoy",
#                 #"gold_yoy",
#                 #"copper_gold_ratio",
#                 "wheat_price_yoy",
#                 #"agr_raw_materials_price_yoy",
#                 "commodity_price_index_yoy"
# )

# fred_complete <- fred %>%  
#   mutate(across(where(is.numeric), na.approx, na.rm = FALSE),
#          across(where(is.numeric), ~replace_na(.x, replace = 0)),
#          .keep = "unused") 

# train_control <- trainControl(method = "cv", number = 10)
# global_economic_activity_loess <- train(global_economic_activity ~ date, data = fred_complete, method = "gamLoess", span = 0.05, degree = 1)
# nitro_fertilizer_price_yoy_loess <- train(nitro_fertilizer_price_yoy ~ date, data = fred_complete, method = "gamLoess", span = 0.05, degree = 1)
# fertilizer_materials_price_yoy_loess <- train(fertilizer_materials_price_yoy ~ date, data = fred_complete, method = "gamLoess", span = 0.05, degree = 1)
# copper_price_yoy_loess <- train(copper_price_yoy ~ date, data = fred_complete, method = "gamLoess", span = 0.05, degree = 1)
# wheat_price_yoy_loess <- train(wheat_price_yoy ~ date, data = fred_complete, method = "gamLoess", span = 0.05, degree = 1)
# agr_raw_materials_price_yoy_loess <- train(agr_raw_materials_price_yoy ~ date, data = fred_complete, method = "gamLoess", span = 0.05, degree = 1)
# gas_price_yoy_loess <- train(gas_price_yoy ~ date, data = fred_complete, method = "gamLoess", span = 0.05, degree = 1)
# brent_price_yoy_loess <- train(brent_price_yoy ~ date, data = fred_complete, method = "gamLoess", span = 0.05, degree = 1)
# food_price_yoy_loess <- train(food_price_yoy ~ date, data = fred_complete, method = "gamLoess", span = 0.05, degree = 1)
# commodity_price_index_yoy_loess <- train(commodity_price_index_yoy ~ date, data = fred_complete, method = "gamLoess", span = 0.05, degree = 1)
# gold_import_price_yoy_loess <- train(gold_import_price_yoy ~ date, data = fred_complete, method = "gamLoess", span = 0.05, degree = 1)
# 
# fred_complete <- fred_complete %>% mutate(global_economic_activity_fit = stats::predict(global_economic_activity_loess),
#                          nitro_fertilizer_price_yoy_fit = stats::predict(nitro_fertilizer_price_yoy_loess),
#                          fertilizer_materials_price_yoy_fit = stats::predict(fertilizer_materials_price_yoy_loess),
#                          copper_price_yoy_fit = stats::predict(copper_price_yoy_loess),
#                          wheat_price_yoy_fit = stats::predict(wheat_price_yoy_loess),
#                          agr_raw_materials_price_yoy_fit = stats::predict(agr_raw_materials_price_yoy_loess),
#                          gas_price_yoy_fit = stats::predict(gas_price_yoy_loess),
#                          brent_price_yoy_fit = stats::predict(brent_price_yoy_loess),
#                          food_price_yoy_fit = stats::predict(food_price_yoy_loess),
#                          commodity_price_index_yoy_fit = stats::predict(commodity_price_index_yoy_loess),
#                          gold_import_price_yoy_fit = stats::predict(gold_import_price_yoy_loess),
#                          .keep = "unused"
#                          )
  
#### Other sources ####

# owid <- read_csv("owid_data.csv") %>%
#   select(c("iso_code", "date", "PC1", "PC2")) %>%
#   mutate(PC1 = log(PC1), PC2 = log(PC2))
# 
# mobility <- read_csv("mobility_data.csv") %>%
#   left_join(regions %>% select(c("ISO2", "ISO")), by = c("country_region_code" = "ISO2")) %>%
#   select(-country_region_code)
# 
# data %>% 
#   filter(iso == "FRA") %>% 
#   select(date, gdp_a_yoy, manufacturing_production) %>% 
#   mutate(across(where(is.numeric), zscore)) %>% 
#   pivot_longer(cols = -date, names_to = "variable", values_to = "value") %>% 
#   filter(!is.na(value)) %>% 
#   ggplot(aes(y = value, x = date, color = variable)) + 
#   geom_line() +
#   geom_smooth(span = 0.1)
#
# data %>% 
#   filter(iso == "BRA") %>%
#   select(-contains("gdp") & -iso) %>%
#   mutate(across(where(is.numeric), zscore)) %>%
#   pivot_longer(cols = -date, names_to = "feature", values_to = "value") %>% 
#   filter(!is.na(value)) %>% 
#   ggplot(aes(y = value, x = date, color = feature)) + 
#   geom_line() + 
#   facet_wrap(~feature, ncol = 4, shrink = TRUE) + 
#   #geom_line(aes(data = countrydata, y = gdp_a_yoy, x = date, na.rm = TRUE)) +
#   theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank())

