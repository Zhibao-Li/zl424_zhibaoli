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

tax=c("Limited Liability Company","S Corporation")
taxbox<-box |> filter(subawardee_business_types%in%tax)

beautiful_colors <- c("#b2e061", "#bd7ebe")
options(scipen=10000)

plt3<-ggplot(taxbox,aes(x=subawardee_business_types,y=average_subaward_amount,
                        fill=subawardee_business_types))+
  geom_violin()+
  scale_fill_manual(
    values = beautiful_colors
  ) +
  geom_jitter(color="black", size=2, alpha=0.9) + 
  labs(
    x = "Business Scale", y = "Average Subcontract Amounts ($)"
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

p3 <- plt3 + theme(legend.position='none')

ggplotly(p3,tooltip=NULL)




compa=c("Contracts and Grants","Foreign Owned and Located","Veteran Owned Business","Woman-Owned Business","Minority-Owned business",
"Limited Liability Company","S Corporation")
compabox<-box |> filter(subawardee_business_types%in%compa)

beautiful_colors <- c("#7c1158","#ffee65","#b2e061","#beb9db","#bd7ebe",
                      "#fdcce5","#8bd3c7")
options(scipen=10000)

plt4<-ggplot(compabox,aes(x=subawardee_business_types,y=average_subaward_amount,
                        fill=subawardee_business_types))+
  geom_violin()+
  scale_fill_manual(
    values = beautiful_colors
  ) +
  geom_jitter(color="black", size=2, alpha=0.9) + 
  labs(
    x = "Business Scale", y = "Average Subcontract Amounts ($)"
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

p4 <- plt4 + theme(legend.position='none')

ggplotly(p4,tooltip=NULL)







