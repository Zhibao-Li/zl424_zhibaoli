library(ggupset)
library(tidyverse)
library(skimr)

df <- read.csv("Dataset_Contract_Sub-Awards.csv")
str(df)
skim(df)
View(df)
names(df)
df$subaward_fsrs_report_id |> unique()
df$subaward_number |> unique()
df$subaward_amount |> unique()
df_sub <- df |> select(starts_with("subaward"))
dim(df_sub)
View(df_sub)

View(table(df$subawardee_name))
View(table(df$subawardee_parent_name))
View(table(df$subawardee_highly_compensated_officer_1_name))
View(table(df$subawardee_duns))


df$subawardee_name <- gsub(" CORPORATION", "", df$subawardee_name)
df$subawardee_name <- gsub(" LLC", "", df$subawardee_name)
df$subawardee_name <- gsub(", INC.", "", df$subawardee_name)
df$subawardee_name <- gsub(", L.P.", "", df$subawardee_name)
df$subawardee_name <- gsub(" CORP.", "", df$subawardee_name)
df$subawardee_name <- gsub("CO., INC.", "", df$subawardee_name)
df$subawardee_name <- gsub(", LLC", "", df$subawardee_name)

df <- df |> mutate(top_subawardee_name = subawardee_name)

k <- df |>
  group_by(top_subawardee_name) |>
  summarise(subcontract_counts = n(), total_subcontract_amounts = sum(subaward_amount)) |>
  arrange(desc(total_subcontract_amounts)) |>
  filter(total_subcontract_amounts>=4990000)
k$top_subawardee_name
View(k)

top_companies <- c(
  "ALLIANT TECHSYSTEMS OPERATIONS","ORBITAL SCIENCES", "ENSIGN-BICKFORD AEROSPACE & DEFENSE COMPANY",
  "PACIFIC SCIENTIFIC ENERGETIC MATERIALS COMPANY (CALIFORNIA)", "AEROJET ROCKETDYNE","DUCOMMUN LABARGE TECHNOLOGIES","TRI MODELS"
)

sum(df$subaward_amount)


df_top <- df[df$top_subawardee_name %in% top_companies, ]
df_top$top_subawardee_name
df_notop <- df[!(df$top_subawardee_name %in% top_companies), ]
df_notop$top_subawardee_name <- "Others"

df2 <- bind_rows(df_top, df_notop)
top_subawardee_name = as.factor(df2$top_subawardee_name)
df2$top_subawardee_name_new = top_subawardee_name
df2<-df2 |> select(-top_subawardee_name_new)

table(df2$top_subawardee_name)
df2<-df2 |> mutate(top_subawardee_name=case_when(top_subawardee_name=="ALLIANT TECHSYSTEMS OPERATIONS"~"Alliant Techsystems Operations",
                                            top_subawardee_name=="ORBITAL SCIENCES"~"Orbital Sciences",
                                            top_subawardee_name=="ENSIGN-BICKFORD AEROSPACE & DEFENSE COMPANY"~"Ensign-Bickford Aerospace & Defense Company",
                                            top_subawardee_name=="PACIFIC SCIENTIFIC ENERGETIC MATERIALS COMPANY (CALIFORNIA)"~"Pacific Scientific Energetic Materials Company (California)",
                                            top_subawardee_name=="AEROJET ROCKETDYNE"~"Aerojet Rocketdyne",
                                            top_subawardee_name=="DUCOMMUN LABARGE TECHNOLOGIES"~"Ducommun Labarge Technologies",
                                            top_subawardee_name=="TRI MODELS"~"Tri Models",
                                            top_subawardee_name=="Others"~"Others"))

write.csv(df2,'df2.csv',row.names=FALSE)

df_ggupset<-df2 |> select(prime_award_awarding_office_name, prime_award_funding_office_name, top_subawardee_name,subaward_amount)
table(df_ggupset$prime_award_awarding_office_name)
table(df_ggupset$prime_award_funding_office_name)

df_ggupset<-df_ggupset |> mutate(id=seq(1:nrow(df_ggupset)))
table(df_ggupset$prime_award_awarding_office_name)
table(df_ggupset$prime_award_funding_office_name)
category.plotdata <- df_ggupset |> pivot_longer(cols=c("prime_award_awarding_office_name","prime_award_funding_office_name"),
                                         names_to="sponser_class",values_to="sponsor_offices")

category.plotdata_new <- category.plotdata |> 
  group_by(id) |> 
  mutate(sponsor_offices = list(sort(sponsor_offices))) |> 
  select(id,top_subawardee_name,subaward_amount,sponsor_offices) |> 
  unique()
View(category.plotdata_new)

category.plotdata_new2<-category.plotdata_new |> 
  group_by(sponsor_offices,top_subawardee_name) |> 
  summarise(total_subcontract_amounts = sum(subaward_amount),subcontract_counts = n())

category.plotdata_new2$top_subawardee_name
category.plotdata_new2$top_subawardee_name <- factor(category.plotdata_new2$top_subawardee_name, 
                                         levels = c("Alliant Techsystems Operations","Orbital Sciences", 
                                                    "Ensign-Bickford Aerospace & Defense Company",
                                                    "Pacific Scientific Energetic Materials Company (California)", 
                                                    "Aerojet Rocketdyne","Ducommun Labarge Technologies","Tri Models","Others"))

beautiful_colors <- c("#e60049", "#0bb4ff", "#50e991", "#e6d800", "#9b19f5", "#ffa300", "#dc0ab4", "#999999")

plt <- ggplot(category.plotdata_new2, aes(x = sponsor_offices, y=total_subcontract_amounts,fill = top_subawardee_name, width = 0.6)) +
  geom_bar(stat="identity") +
  scale_x_upset(n_intersections = 100) +
  labs(
    x = "Awarder/Funder Office", y = "Total Subcontract Amounts ($)"
  ) +
  theme_minimal() +
  scale_fill_manual(
    name = "Main Subcontractors", 
    labels = c("Alliant Techsystems Operations","Orbital Sciences", 
    "Ensign-Bickford Aerospace & Defense Company",
    "Pacific Scientific Energetic Materials Company (California)", 
    "Aerojet Rocketdyne","Ducommun Labarge Technologies","Tri Models","Others"),
    values = beautiful_colors
  )+
  theme_combmatrix(
    combmatrix.label.make_space = TRUE,
    combmatrix.panel.point.size = 2,
    combmatrix.panel.line.size = 0.4,
    combmatrix.panel.point.color.fill = "#549490",
    combmatrix.panel.point.color.empty = "#f6e0d5",
    combmatrix.label.text = element_text(size = 15,  family="Georgia"),
    panel.grid = element_blank(),
    # plot.title = element_text(color = "#ff1654", size = 18, face = "bold.italic", hjust = 0.5),
    axis.title.x = element_text(color = "black", size = 15, face = "bold"),
    axis.text = element_text(size = 15),
    axis.title.y = element_text(color = "black", size = 15, face = "bold", vjust = 0),
    plot.caption = element_text(color = "grey", size = 15, face = "bold.italic"),
    legend.position = c(0.7, 0.68),
    legend.key.size = unit(1, "cm"), # change legend key size
    legend.key.height = unit(1, "cm"), # change legend key height
    legend.key.width = unit(1, "cm"), # change legend key width
    legend.title = element_text(size = 15,face="bold"), # change legend title font size
    legend.text = element_text(size = 15),
    text=element_text(size=15,  family="Georgia")
  )+ scale_y_continuous(labels = comma)


ggsave("category_v3.png", 
       plot = plt, 
       device = "png", 
       bg = "white")







