#Association Rules
#library
library(arules)
groceries <- read.transactions("grocery.csv", sep = ",")
itemFrequencyPlot(groceries, topN = 25, type = "relative", 
                  main = "Most Frequently Purchased Items", 
                  ylab = "Relative Item Frequency", cex = 0.6)

rank <- as.data.frame(itemFrequency(groceries, type = "relative")) %>%
  rename(pct=`itemFrequency(groceries, type = "relative")`) %>%
  rownames_to_column("rhs") %>%
  arrange(-pct) %>%
  mutate(dummy=1, rank=cumsum(dummy)) %>%
  select(-dummy)
rank$rhs <- paste("{",rank$rhs,"}",sep = "")

top_coups <- redeem %>% 
  filter(coupon=="yes") %>%
  group_by(SUB_COMMODITY_DESC) %>% 
  summarize(num=n()) %>%
  mutate(pct=num/sum(num)) %>%
  arrange(-num) %>%
  filter(pct>0.01)
ggplot(top_coups,aes(x=reorder(SUB_COMMODITY_DESC,-pct),y=pct)) + 
  geom_bar(stat = "identity", fill = "gray", color = "black") +
  theme(axis.text.x = element_text(size=7,angle=45,hjust=1,vjust=1)) +
  ggtitle("Most Frequently Redeemed Coupons") +
  xlab("") + ylab("Relative Coupon Use")



rules <- apriori(groceries, parameter = list(supp = 0.003, conf = 0.7, maxlen=5))

rules_entree <- subset(rules, lhs %in% "FRZN SS PREMIUM ENTREES/DNRS/N")
rules_entree <- sort(rules_entree, by="confidence",decreasing = TRUE)
entree <- as.data.frame(inspect(rules_entree[1:10]))
entree <- entree[-2]
entree$rhs <- as.character(entree$rhs)
entree <- inner_join(entree,select(rank,rhs,rank),by="rhs")
write_csv(entree,"entree.csv")