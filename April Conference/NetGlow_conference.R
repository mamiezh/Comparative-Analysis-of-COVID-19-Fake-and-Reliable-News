library('tidyverse')
library('ggplot2')
library('tidytext')
library('widyr')
library('ggraph')
library('igraph')
library('ggplot2')
library('foreign')
library('readxl')
library('intergraph')
library('extrafont')
library('gridExtra')
library('reshape2')
library('wordcloud')    
library('wordcloud2')
library('RColorBrewer')

# ---- Databases ----

covid <- read_excel("fake_new_dataset.xlsx")
covid_true <- subset(covid, subset = covid$label == "1")
covid_fake <- subset(covid, subset = covid$label == "0")

bing <- read_csv("Bing.csv",
                 col_types = cols(word = col_character(), sentiment = col_character()))
nrc <- read_csv("NRC.csv",
                col_types = cols(word = col_character(), sentiment = col_character()))

# ---- Most frequent words in fake news ----

covid_fake_new <- covid_fake %>%
  filter(!is.na(title)) %>%
  transmute(post_id = row_number(), title) %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words, by = "word") %>%
  filter(word != "de",
         str_detect(word, "[a-z]"))

covid_fake_new %>%
  count(word, sort = TRUE) %>%
  mutate(word = fct_reorder(word, n)) %>%
  head(20) %>%
  ggplot(aes(word, n))+
  geom_col(width = 0.75,
           col = "white",
           fill = "#99d8c9")+
  coord_flip()+
  labs(title = "Common words in fake news titles")+
  theme_classic()+
  theme(text = element_text(family = "Times", face = "bold", size = 14))

covid_fake_filtered <- covid_fake_new %>%
  add_count(word) %>%
  filter(n >= 10)

covid_fake_all <- covid_fake_filtered %>%
  select(post_id, word) %>%
  pairwise_cor(word, post_id, sort = TRUE) 

top_word_covid_fake <- covid_fake_filtered %>%
  select(post_id, word) %>%
  pairwise_cor(word, post_id, sort = TRUE) %>%
  head(100)

# ---- Most frequent words in reliable news ----

covid_true_new <- covid_true %>%
  filter(!is.na(title)) %>%
  transmute(post_id = row_number(), title) %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words, by = "word") %>%
  filter(word != "de",
         str_detect(word, "[a-z]"))

covid_true_new %>%
  count(word, sort = TRUE) %>%
  mutate(word = fct_reorder(word, n)) %>%
  head(20) %>%
  ggplot(aes(word, n))+
  geom_col(width = 0.75,
           col = "white",
           fill = "#99d8c9")+
  coord_flip()+
  labs(title = "Common words in true news titles")+
  theme_classic()+
  theme(text = element_text(family = "Times", face = "bold", size = 14))

covid_true_filtered <- covid_true_new %>%
  add_count(word) %>%
  filter(n >= 10)

top_word_covid_true <- covid_true_filtered %>%
  select(post_id, word) %>%
  pairwise_cor(word, post_id, sort = TRUE) %>%
  head(100)

# ---- Fake graphs ----

top_word_covid_fake %>%
  graph_from_data_frame() %>%
  ggraph(layout = 'nicely')+
  geom_edge_link()+
  geom_node_point()+
  theme_void(base_size = 18)+
  geom_node_text(aes(label = name), repel = TRUE)

top_word_covid_fake %>%
  graph_from_data_frame() %>%
  ggraph(layout = 'linear', circular = TRUE)+
  geom_edge_arc(aes(colour=correlation, alpha=correlation))+
  coord_fixed()+
  geom_node_text(aes(label = name), repel = TRUE)+
  geom_node_point()+
  theme_void(base_size = 18)+
  scale_edge_color_continuous(low="red", high="green")+
  theme(legend.position = "none")

# ---- Reliable graphs ----

top_word_covid_true %>%
  graph_from_data_frame() %>%
  ggraph(layout = 'nicely')+
  geom_edge_link()+
  geom_node_point()+
  theme_void(base_size = 18)+
  geom_node_text(aes(label = name), repel = TRUE)

top_word_covid_true %>%
  graph_from_data_frame() %>%
  ggraph(layout = 'linear', circular = TRUE)+
  geom_edge_arc(aes(colour=correlation, alpha=correlation))+
  coord_fixed()+
  geom_node_text(aes(label = name), repel = TRUE)+
  geom_node_point()+
  theme_void(base_size = 18)+
  scale_edge_color_continuous(low="red", high="green")+
  theme(legend.position = "none")

# ---- Sentimental analysis for fakes----

options(repr.plot.width=15, repr.plot.height=15)
colsR_B<- brewer.pal(4,name = 'RdBu')
display.brewer.pal(4,name = 'RdBu')

covid_fake_new %>% 
  inner_join(bing, by="word") %>%
  count(word, sentiment, sort=T) %>% 
  acast(word ~ sentiment, value.var = "n", fill=0) %>% 
  comparison.cloud(colors=colsR_B[c(1, 4)], max.words = 400, title.size = 2,
                    scale = c(3,.5))


# ---- Sentimental analysis for reliable ----

covid_true_new %>% 
  inner_join(bing, by="word") %>%
  count(word, sentiment, sort=T) %>% 
  acast(word ~ sentiment, value.var = "n", fill=0) %>% 
  comparison.cloud(colors=colsR_B[c(1, 4)], max.words = 400, title.size = 2,
                   scale = c(3,.5))


# ---- Radar Chart ----
library('radarchart')
covid_true_new1<-covid_true_new
covid_fake_new1<-covid_fake_new 

FAKE <-c()
for (i in 1:length(covid_fake_new1$post_id)){
  FAKE<- append(FAKE,'FAKE')
}
FAKE

RIGHT <-c()
for (i in 1:length(covid_true_new1$post_id)){
  RIGHT<- append(RIGHT,'TRUE')
}
RIGHT

covid_fake_new1$status <- FAKE 
covid_true_new1$status<-RIGHT

covid_new<-rbind(covid_fake_new1, covid_true_new1)

char_sentiment <- covid_new %>% 
  inner_join(nrc, "word") %>% 
  filter(!sentiment %in% c("positive", "negative")) %>% 
  group_by(status, sentiment) %>% 
  count(status, sentiment) %>% 
  select(status, sentiment, char_sentiment_count=n)

total_char <- covid_new %>% 
  inner_join(nrc, "word") %>% 
  filter(!sentiment %in% c("positive", "negative")) %>% 
  count(status) %>% 
  select(status, total=n)

plt <- char_sentiment %>% 
  inner_join(total_char, by="status") %>% 
  mutate(percent = char_sentiment_count / total * 100 ) %>% 
  select(-char_sentiment_count, -total) %>% 
  spread(status, percent)  %>% 
  chartJSRadar(showToolTipLabel = T, maxScale=25, responsive=T,
               addDots = T, 
               colMatrix = grDevices::col2rgb(colsR_B[c(1, 4)]),
               lineAlpha = 0.7, polyAlpha = 0.2)
plt
colsR_B[c(1, 4)]


# ---- ERGM ----

detach(package:igraph)
detach(package:ergm)
detach(package:network)
detach(package:sna)
library('network')
library('sna')
library('ergm')
#library('igraph')


library(knitr)
library('gridExtra')
pdf("trade.pdf", height=11, width=8.5)
grid.table(df)
dev.off()
centralitiesF <- data.frame(row.names   = V(Graph_top_F)$name,
                          degree      = degree(Graph_top_F),
                          betweenness = betweenness(Graph_top_F),
                          closseness = closeness(Graph_top_F),
                          eigenvector = evcent(Graph_top_F)$vector, 
                          pagerank = page_rank(Graph_top_F)$vector)
pdf("centralities_fake.pdf", height=11, width=8.5)
grid.table(centralitiesF)
dev.off()

kable(centralities)

centralitiesT <- data.frame(row.names   = V(Graph_top_T)$name,
                           degree      = degree(Graph_top_T),
                           betweenness = betweenness(Graph_top_T),
                           #closseness = closeness(Graph_top_T),
                           eigenvector = evcent(Graph_top_T)$vector, 
                           pagerank = page_rank(Graph_top_T)$vector)
pdf("centralities_true.pdf", height=40, width=8.5)
grid.table(centralitiesT)
dev.off()

kable(centralities)




types <-c('003','012','102','021D','021U','021C','111D','111U','030T','030C','201','120D','120U','120C', '210', '300')
t<-sapply(list(Graph_top_F, Graph_top_T),triad_census)
colnames(t) <-c('FAKE', 'TRUE')
triads <- data.frame(t)
rownames(triads) <- types

pdf("triads.pdf", height=11, width=8.5)
grid.table(triads)
dev.off()
kable(triads, label="Triad Census")

tr <-sapply(list(Graph_top_F, Graph_top_T),transitivity)
transitivities <- data.frame(transitivity=tr)
rownames(transitivities) <- c("FAKE", 'TRUE')
pdf("transitivity.pdf", height=11, width=8.5)
grid.table(transitivities)
dev.off()

d<-sapply(list(Graph_top_F, Graph_top_T),dyad.census)
dyads <- data.frame(d)
colnames(dyads) <- c("FAKE", 'TRUE')
rownames(dyads) <- c("mut", 'asym', 'null')
pdf("dyads.pdf", height=1100, width=8.5)
grid.table(dyads, rows=c("mut", 'asym', 'null'))
dev.off()

den <-sapply(list(Graph_top_F, Graph_top_T),edge_density)
density <- data.frame(density=den)
rownames(density) <- c("FAKE", 'TRUE')
pdf("density.pdf", height=11, width=8.5)
grid.table(density)
dev.off()

kable(dyads, label="Dyad Census")


