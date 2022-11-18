
# prep morphometric data


set.seed(123)

## read-in data

ctr_original <-
  data.frame(readRDS(
    here::here("../../raw_data/081120_Demetrius/ctr_t1.rds")),
    check.names = FALSE)

sa_original <-
  data.frame(readRDS(
    here::here("../../raw_data/081120_Demetrius/sa_t1.rds")),
    check.names = FALSE)

# convert to long format
# equivalent to MRI input data

sa_long_t1 <-
  sa_original %>%
  select(-Machine, -Type, -Subject, -Gender,
         -Age, -Manufacturer, -Model, -Date_GraphICA) %>%
  mutate(id = 1:n()) %>%
  melt(id.var = "id",
       variable.name = "region") %>%
  merge(regions, by.x = "region", by.y = "Region")

ct_long_t1 <-
  ctr_original %>%
  select(-Machine, -Type, -Subject, -Gender,
         -Age, -Manufacturer, -Model, -Date_GraphICA) %>%
  mutate(id = 1:n()) %>%
  melt(id.var = "id",
       variable.name = "region") %>%
  merge(regions, by.x = "region", by.y = "Region")

# combine
# need to do like this to match network format
dat_t1 <-
  list(list(ct_long = ct_long_t1,
            sa_long = sa_long_t1))

saveRDS(dat_t1, file = glue("data/dat_t1.RDS"))


##TODO: is this what we want to do in the t1 case?...
# separate dataframes in to list for each network
# filter by network region map

NETWORK_NAMES <-
  data.frame(network = 1:11,
             network_name =
               c('Auditory',
                 'DMN',
                 'ECN_L',
                 'ECN_R',
                 'Hippocampal',
                 'Language',
                 'Salience',
                 'Sensorimotor',
                 'Visual_lateral',
                 'Visual_medial',
                 'Visual_occipital'))

# network masks (subset of regions)
list_nodes_network_male <-
  readRDS("~/Jasmina UCL/superagers/raw_data/list_nodes_network_male.rds") %>%
  setNames(NETWORK_NAMES$network_name)

keep_network <- c("DMN", "Salience", "ECN_L", "ECN_R", "Hippocampal", "Language")

ct_list_t1 <-
  map(list_nodes_network_male,
      ~ merge(ct_long_t1, data.frame(Label = .x),
              by.x = "Label"))

sa_list_t1 <-
  map(list_nodes_network_male,
      ~ merge(sa_long_t1, data.frame(Label = .x),
              by.x = "Label"))

# networks of interest
ct_list_t1 <- ct_list_t1[keep_network]
sa_list_t1 <- sa_list_t1[keep_network]

# combine
dat_list_t1 <-
  list(ct_long = ct_list_t1,
       sa_long = sa_list_t1) %>%
  transpose()

saveRDS(dat_list_t1, file = glue("data/dat_list_t1.RDS"))


