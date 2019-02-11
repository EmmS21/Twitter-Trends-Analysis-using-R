install.packages("tm")
install.packages("ggplot2")
library(tm)
library(ggplot2)
library(scales)
library(textcat)
library(quanteda)
shortWordRemover <- function(x) gsub('\\b\\w{1,2}\\b','',x)
sona_corpus <- Corpus(VectorSource(new_tweetsdf))
sona_corpus <- tm_map(sona_corpus, removeWords, c(stopwords("english"),"beliees","thogh","failre","might","atch","neither","mch","becase","enjo","ith","titter","trimphs","ictor","sffer","gra","dare","contr","tilight","spirits","checkered","crod","hes"))
sona_corpus <- tm_map(sona_corpus, content_transformer(shortWordRemover))
sona_corp<- corpus(sona_corpus)
sona_dfm <- tokens(sona_corp) %>%
  tokens_remove("\\p{P}", valuetype ="regex", padding=TRUE)%>%
  tokens_remove("\\d+", padding = TRUE)%>%
  tokens_ngrams(n=3) %>%
  dfm()
#frequency table
freq_table <- textstat_frequency(sona_dfm, n=30) #top25
# Add lines to the initial dataset
freq_table$id=seq(1, nrow(freq_table))
# Get the name and the y position of each label
label_data=freq_table
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)
# Make the plot
p = ggplot(freq_table, aes(x=as.factor(id), y=frequency)) +   
  geom_bar(stat="identity", fill=alpha("green", 0.3)) +
  scale_y_continuous(limits=c(0,1500)) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar(start = 0) + 
  geom_text(data=label_data, aes(x=id, y=frequency+20, label=feature, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE )
print(p)