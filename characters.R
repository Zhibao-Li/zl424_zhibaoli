library(ggupset)
library(tidyverse)
library(skimr)
library(plotly)

df2 <- read.csv("df2.csv")
View(table(df2$subawardee_business_types))

df2 |> filter(subawardee_business_types=="Nonprofit Organization") |> select(subawardee_name,subaward_amount)



# characters findings
# own 

total_dist<-as.data.frame(table(df2 |> select(subawardee_name,subawardee_business_types)|> distinct() |>
  separate_rows(subawardee_business_types,sep=",") |> select(subawardee_business_types))) |> arrange(desc(Freq))

total_dist$Var1<-gsub("Asian-Pacific American Owned", "Minority-Owned business", total_dist$Var1)
total_dist$Var1<-gsub("Economically Disadvantaged Women-Owned Small Business", "Minority-Owned business", total_dist$Var1)
total_dist$Var1<-gsub("Native American Owned", "Minority-Owned business", total_dist$Var1)
total_dist$Var1<-gsub("Service Disabled Veteran Owned", "Veteran Owned Business", total_dist$Var1)
total_dist$Var1<-gsub("Contracts", "Contracts and Grants", total_dist$Var1)
total_dist$Var1<-gsub("Contracts and Grants and Grants", "Contracts and Grants", total_dist$Var1)
total_dist<-total_dist |> group_by(Var1) |> summarise(Freq=sum(Freq)) |> arrange(desc(Freq))
total_dist<-total_dist |> rename(Category=Var1,Value=Freq)
write.csv(total_dist,'total_dist.csv',row.names=FALSE)
total_dist

box<-df2 |> select(subawardee_name,subawardee_business_types,subaward_amount) |> 
  group_by(subawardee_name) |> 
  summarise(average_subaward_amount=mean(subaward_amount),subawardee_business_types=unique(subawardee_business_types)) |> 
  separate_rows(subawardee_business_types,sep=",") |> distinct()

box$subawardee_business_types<-gsub("Asian-Pacific American Owned", "Minority-Owned business", box$subawardee_business_types)
box$subawardee_business_types<-gsub("Economically Disadvantaged Women-Owned Small Business", "Minority-Owned business", box$subawardee_business_types)
box$subawardee_business_types<-gsub("Native American Owned", "Minority-Owned business", box$subawardee_business_types)
box$subawardee_business_types<-gsub("Service Disabled Veteran Owned", "Veteran Owned Business", box$subawardee_business_types)
box$subawardee_business_types<-gsub("Contracts", "Contracts and Grants", box$subawardee_business_types)
box$subawardee_business_types<-gsub("Contracts and Grants and Grants", "Contracts and Grants", box$subawardee_business_types)

tax=c("Limited Liability Company","S Corporation","Self-Certified Small Disadvantaged Business")


owned<-c("Foreign Owned and Located","Veteran Owned Business","Woman-Owned Business","Minority-Owned business")
ownbox<-box |> filter(subawardee_business_types%in%owned)
# ownbox$average_subaward_amount<-log(ownbox$average_subaward_amount)
write.csv(ownbox,'ownbox.csv',row.names=FALSE)
beautiful_colors <- c("#ffee65", "#beb9db", "#fdcce5", "#8bd3c7")
options(scipen=10000)

plt2<-ggplot(ownbox,aes(x=subawardee_business_types,y=average_subaward_amount,
                        fill=subawardee_business_types))+
  geom_violin()+
  scale_fill_manual(
    values = beautiful_colors
  ) +
  geom_jitter(color="black", size=2, alpha=0.9) + 
  labs(
    x = "Special Owned-Business Type", y = "Average Subcontract Amounts ($)"
  ) +
  theme_minimal() +
  theme(
    # plot.title = element_text(color = "#ff1654", size = 18, face = "bold.italic", hjust = 0.5),
    axis.title.x = element_text(color = "black", size = 13, face = "bold"),
    axis.text = element_text(size = 13),
    axis.title.y = element_text(color = "black", size = 13, face = "bold", vjust = 0),
    plot.caption = element_text(color = "grey", size = 13, face = "bold.italic"),
    legend.position = "None",
    legend.key.size = unit(1, "cm"), # change legend key size
    legend.key.height = unit(1, "cm"), # change legend key height
    legend.key.width = unit(1, "cm"), # change legend key width
    legend.title = element_text(size = 13,face="bold"), # change legend title font size
    legend.text = element_text(size = 13),
    text=element_text(size=13,  family="Georgia"),
    axis.text.x = element_text(angle=45, vjust=.5, hjust=1)
  )
p <- plt2 + theme(legend.position='none')
ggplotly(p,tooltip = NULL)









# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)

# create a dataset
data <- data.frame(
  name=c( rep("A",500), rep("B",500), rep("B",500), rep("C",20), rep('D', 100)  ),
  value=c( rnorm(500, 10, 5), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(20, 25, 4), rnorm(100, 12, 1) )
)

# Plot
data %>%
  ggplot( aes(x=name, y=value, fill=name)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot with jitter") +
  xlab("")









df2 |> select(subawardee_name,subawardee_business_types,subaward_amount)|> distinct() |>
  separate_rows(subawardee_business_types,sep=",") |> distinct()




cloud<-df2 |> select(top_subawardee_name,subawardee_business_types) |> filter(top_subawardee_name!="Others")|> 
  distinct() |> separate_rows(subawardee_business_types,sep=",") |> distinct() |> 
  select(subawardee_business_types) |> group_by(subawardee_business_types) |> summarise(Freq=n())

write.csv(cloud,"cloud.csv",row.names=FALSE)






as.data.frame(table(df2[grep("Contracts and Grants", df2$subawardee_business_types),] |> 
  select(top_subawardee_name,subawardee_business_types,subaward_amount) |> 
  select(subawardee_business_types) |> separate_rows(subawardee_business_types,sep=",")))
  



df_boxplot1<-df2 |> select(subawardee_name,subawardee_business_types,subaward_amount)

pro<-df_boxplot1[grep("For-Profit Organization", df_boxplot1$subawardee_business_types),]
nopro<-df_boxplot1[grep("Nonprofit Organization", df_boxplot1$subawardee_business_types),]


pro<-pro |> mutate(profit="For-Profit Organization")
nopro<-nopro |> mutate(profit="Nonprofit Organization")
df_profit<-bind_rows(pro,nopro)

df_profit$profit<-as.factor(df_profit$profit)
df_profit<-df_profit |> unique()
str(df_profit$profit)
ggplot(df_profit, aes(x=profit))+geom_bar()


write.csv(df_profit,'df_profit.csv',row.names=FALSE)








