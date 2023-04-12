#### PREAMBLE ####
options(future.globals.maxSize= 891289600, future.rng.onMisuse = "ignore")
if("librarian" %in% rownames(installed.packages()) == FALSE) {install.packages("librarian")}
librarian::shelf(here, tidyverse, beepr, furrr, corrr, caret, GGally, RColorBrewer) # Note that some required sub-dependencies may need to be updated for packages to work. This will return a warning. If this is the case, it is best to restart the session and install the packages mentioned in the warning separately.

future::plan(multisession, workers = max(availableCores(),1)) # Helps to speed up certain processes

#### SET TEST PARAMETERS AND DATA ####

#source("afdb_nowcast_load_data.R")
source("helper_functions.R")

i <- "DZA"
train_test_data <- initialize_data(countrydata = data[data$iso == i,],
                                   target = "food_price_yoy", 
                                   train_size = 0.7, 
                                   test_end_date = "2019-12-1", 
                                   scale_target_tf = T, 
                                   explanatory_variables = NA, # tune_variables$col_names
                                   parameters = fast)

#### DATA INSPECTION ####

data %>% group_by(iso, date) %>% mutate(N = n()) %>% filter(N > 1) %>% count() # Check for duplicates. First line of output should be # A tibble: 0 x 3

data %>% 
  group_by(iso) %>% 
  summarize(across(where(is.numeric), ~mean(!is.na(.x)))) %>% 
  ungroup() %>% 
  pivot_longer(-iso) %>%
  ggplot(aes(iso, name, fill = value)) +
  geom_tile(color = "grey90") +
  scale_fill_gradientn(colors = brewer.pal(9, "RdYlGn")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("") + 
  ylab("") 

countrydata <- data[data$iso == i,]

View(countrydata)
summary(countrydata)
colnames(countrydata)
heatmap(1*is.na(countrydata), Rowv = NA, Colv = NA, scale = "none", margins = c(12, 1), labRow = as.character(year(countrydata$date)))

hist(countrydata$food_price_afdb_yoy, breaks = 30)

#### Feature comparison ####
train <- train_test_data$train
train %>% 
  select(date, food_price_yoy, global_commodity_price_index_yoy) %>%
  mutate(across(where(is.numeric), na.approx, na.rm = FALSE)) %>% 
  pivot_longer(cols = -date) %>% 
  ggplot(aes(y = value, x = date, colour = name)) + geom_line() +  theme(legend.position = "top")

train <- train_test_data$train
featurePlot(x = train[,3:ncol(train)], 
            y = train$food_price_afdb_yoy, 
            type = c("p", "smooth"),
            span = .5)

train %>% 
  select(where(is.numeric)) %>%
  ggcorr(method = c("pairwise", "pearson"), legend.position = c(0,0.8)) +
  theme(plot.margin=unit(c(1,1,1,1),"cm"), axis.text.y = element_text(angle = 90))

countrydata %>% 
  mutate(across(where(is.numeric), zscore),
         across(where(is.numeric), abs)) %>% 
  pivot_longer(cols = -date) %>% 
  ggplot(aes(date, name, color = value)) + 
  geom_tile() +
  scale_colour_gradientn(colours=brewer.pal(9, "OrRd"))

sds <- countrydata %>% summarise_if(is.numeric,  sd, na.rm = TRUE) %>% pivot_longer(cols = everything(), names_to = "variable", values_to = "sd") %>% arrange(desc(sd))

## Unit root test
# 1. ADF stores test result for Augmented Dickey-Fuller tests on all features for a given country i, filtering the features for which the type1 test statistic is higher than 0.1 (see adf.test reference material for interpretation of test statistics).
# 2. First difference is taken of variables with high likelihood of unit root
# 3. Compare the variables with unit root with their first difference

acf(countrydata$consumer_credit_yoyfdiff, lag = 12, na.action = na.pass)
adf <- future_map2_dfr(countrydata[,2:ncol(countrydata)], adf.test, nlag = 12, output = FALSE, .id = "variable") %>% filter(type1[,1] == 11 & type1[,3] > 0.05) %>%  arrange(desc(type1[,3]))

countrydata %>% ggplot(aes(date, gdp_q_yoy)) + geom_point(na.rm = TRUE)
qplot(data = countrydata, x = date, y = crude_oil_yoy_fdiff)