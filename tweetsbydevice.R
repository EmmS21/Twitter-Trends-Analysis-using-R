library(dplyr)
library(plotly)
table_tweet <- data.frame(head(sort(table(tweets$source),decreasing=T),20))
ggplot(table_tweet, aes(x=Var1, y=Freq)) +
  geom_segment( aes(x=Var1, xend=Var1, y=0, yend=Freq)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Tweets by device/source", x="Device/Source",y="Frequency")+
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) 
