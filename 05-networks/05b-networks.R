
# Inspect the world trade network
## 1. BACI data available at http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37
data_path= "~/work/data/comtrade_cepii/BACI_HS07/"

## Use 2017 Baci and attach conversion codes
file1 = "BACI_HS07_Y2017_V202001_p1.csv.gz"
file2 = "BACI_HS07_Y2017_V202001_p2.csv.gz"

# Read and prepare the conversion dataset
conversion <- fread(paste0(data_path, "country_codes_cepii_V2021.csv.gz"))[, .(isocode3 = iso_3digit_alpha, country_code)]

# Load and preprocess the BACI dataset
data_baci <- rbind(fread(paste0(data_path,file1)), fread(paste0(data_path, file2)))

# Filter out unwanted product codes
data_baci <- data_baci[!grepl("^98", k) & !grepl("^99", k), .(t, i, j, k, v)]

# Perform the join operations and rename columns using data.table syntax
data_baci <- data_baci[conversion, on = .(i = country_code), nomatch = 0][conversion, on = .(j = country_code), nomatch = 0]

# Rename columns 'i' and 'j' to 'exp' and 'imp' respectively to reflect the ISO country codes
setnames(data_baci, old = c("isocode3", "i.isocode3"), new = c("exp", "imp"))

# Print the first few rows to check the results
print(head(data_baci))

head(data_baci)
data_baci[, c("i", "j") := NULL]

setnames(data_baci, old = c("t", "k", "v", "exp", "imp"),
         new = c("year", "product_code", "trade_value", "exp", "imp"))
# remove the conversion file
rm(conversion)

# plot the degree distribution 
data_baci %>% select(exp,imp) %>% distinct() %>% group_by(exp) %>% 
  mutate(degree=n()) %>% select(exp, degree) %>% distinct() %>% 
  ggplot(., aes(x = degree)) +
  geom_histogram(aes(y = ..density..), bins = 10, color="white", fill="blue") +
  xlab("Degrees") + ylab("Frequencies") + ggtitle("Out-Degree distribution of the Trade Network in 2017") +
  theme_minimal()

# compute out- in-degree 
data_baci %>% select(exp,imp) %>% distinct() %>% group_by(exp) %>% 
  mutate(outdegree=n()) %>% select(exp, outdegree) %>% distinct() %>% 
  arrange(-outdegree)

data_baci %>% select(exp,imp) %>% distinct() %>% group_by(imp) %>% 
  mutate(indegree=n()) %>% select(imp, indegree) %>% distinct() %>% 
  arrange(-indegree)

# compute closeness centrality
trade_network <- data_baci %>% select(exp,imp) %>% distinct() %>% 
  graph_from_data_frame(., directed = TRUE) %>% na.omit()

setNames(rownames_to_column(data.frame(closeness( # assign names coutry and centrality to the output
  trade_network,
  mode = c("in"),
  weights = NULL,
  normalized = TRUE))), c("country", "centrality")) %>% 
  arrange(-centrality)  %>% 
  head()

# account for trade flows weights
w_trade_network <- data_baci %>% group_by(exp,imp) %>% mutate(weight=sum(trade_value, na.rm=TRUE)) %>% 
  select(exp,imp,weight=trade_value) %>% distinct() %>% na.omit() %>% 
  graph_from_data_frame(., directed = TRUE) 

is_weighted(w_trade_network)

out_d <- setNames(rownames_to_column(data.frame(strength(
  w_trade_network,
  mode = c("out"),
  loops = TRUE,
  weights = w_trade_network$weight))), 
  c("country", "out_degree")) 
head(arrange(out_d, -out_degree))

in_d <- setNames(rownames_to_column(data.frame(strength(
  w_trade_network,
  mode = c("in"),
  loops = TRUE,
  weights = w_trade_network$weight))), 
  c("country", "in_degree")) 
head(arrange(in_d, -in_degree))

# what is the out-in degree rank correlation?
out_d %>% full_join(in_d, by=c("country")) %>% 
  summarise(cor(in_degree, out_degree, method = c("pearson")))
rm(out_d, in_d)

# nb when you have weights, the centrality is not the average number of edges of 
# the shortest paths to each other node, but the average sum of weights on the shortest path for the distance.

# Produce a table with the closeness index computed on the weighted network
setNames(rownames_to_column(data.frame(closeness(
  w_trade_network,
  mode = c("out"),
  weights = w_trade_network$weight,
  normalized = TRUE,
  cutoff = -1))), 
  c("country", "centrality")) %>% 
  arrange(-centrality)  %>% 
  head()

# What has changed? Why ?



