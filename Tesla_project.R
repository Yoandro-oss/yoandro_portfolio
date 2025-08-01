#installing packages
install.packages("tidyverse")
#libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
#reading in the data

df <- read.csv("C:\\Users\\yoand\\Documents\\R_project(tesla)\\Electric_Vehicle_Population_Data.csv")

#Data cleaning

clean_df <- df %>%
  rename_all(~ gsub("\\.", "_", .)) %>%
  mutate(
    Tesla = ifelse(Make == "TESLA", "TESLA", "OTHER")
  )

# 1. What percentage of EVs in Washington are Tesla? tesla/all vehicles * 100
total_vehicles <- nrow(clean_df)
tesla_vehicles <- clean_df %>% filter(Tesla == "TESLA") %>% nrow()
tesla_market_share <- round((tesla_vehicles/total_vehicles) * 100, 1)


market_df <- data.frame(
  Group = c("TESLA", "OTHER"),
  Market_share = c(tesla_market_share,100 - tesla_market_share)
)

market_df <- market_df %>%
  mutate(
    Label = paste0(Market_share, "%"),
    ypos = cumsum(Market_share) - 0.5 * Market_share
  )

ggplot(market_df, aes(x= "", y = Market_share, fill = Group)) + 
  geom_col(width = 1) +
  coord_polar("y") +
  geom_text(aes(y= ypos,label= Label)) +
  labs(title = "Tesla Market Share in Washington(%)" +
  theme_void()
)

# 2. Top tesla models selling in the area
#-How much are we making from these models

make_model_sold <- clean_df %>%
  filter(Tesla=="TESLA") %>%
  group_by(Make,Model) %>%
  summarise(count_sold = n(), .groups = "drop") %>%
  arrange(desc(count_sold))


ggplot(make_model_sold, aes(x= reorder(Model, count_sold), y= count_sold)) +
  geom_col(fill = "dodgerblue") +
  labs(title = "Tesla models sold - All time") +
  theme_minimal()




msrp_by_year <- clean_df %>%
  filter(Base_MSRP>0) %>%
  filter(Tesla == "TESLA") %>%
  group_by(Model_Year, Make, Model) %>%
  summarise(min(Base_MSRP)) %>%
  arrange(Model_Year, Make, Model)

#There weren't good base msrp data  in this dataset - supplementing with online data

df_tesla_prices <- read.csv("C:\\Users\\yoand\\Documents\\R_project(tesla)\\Tesla_Current_Base_Prices.csv")

df_tesla_prices <- df_tesla_prices %>%
  mutate(
    Model = toupper(Model)
  )
  
#getting our numbers
Top_tesla_model_Priced <-  make_model_sold %>% 
  left_join(df_tesla_prices, by = "Model") %>%
  mutate(
    Estimated_revenue = as.numeric(count_sold) * Price
  )
#Final estimates for liftime sales
Top_tesla_model_Priced <- Top_tesla_model_Priced %>%
  filter(!is.na(Estimated_revenue))%>%
  group_by(Model)%>%
  slice_min(Price)%>%
  ungroup()

# Next viz
ggplot(Top_tesla_model_Priced, aes(x= reorder(Model,Estimated_revenue ), y= Estimated_revenue)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Estimated Revenue per Model") +
  scale_y_continuous(labels = label_comma())+
  theme_minimal()


# ADD IN
  
tesla_sold_by_year <- clean_df %>%
  filter(Tesla == "TESLA") %>%
  group_by(Model_Year)%>%
  summarise(Count = n()) %>%
  arrange(Model_Year)
  

ggplot(tesla_sold_by_year, aes(x=Model_Year, y= Count))+
  geom_line(color= "dodgerblue", size= 1.2)+
  geom_point(color= "red", size=2)+
  labs(title= "Tesla vehicles Sold by year")+
  theme_minimal()

# 3. Tesla vs Competitors - Range and MSRP
#-AVG and Median

#AVG Range
Average_Range_For_EVs = clean_df %>%
  filter(Electric_Vehicle_Type == "Battery Electric Vehicle (BEV)",
         Electric_Range != 0,
         Base_MSRP !=0) %>%
  group_by(Tesla) %>%
  summarise(AVG_Range = mean(Electric_Range),
            AVG_MSRP= mean(Base_MSRP))

#Median Range
Median_Range_For_EVs = clean_df %>%
  filter(Electric_Vehicle_Type == "Battery Electric Vehicle (BEV)",
         Electric_Range != 0,
         Base_MSRP != 0) %>%
  group_by(Tesla) %>%
  summarise(AVG_Range = median(Electric_Range),
            Median_MSRP= median(Base_MSRP))

#MSRP data isnt fully populated and may skew results


#Visualizations for MSRP and Range AVG

#Range
ggplot(Average_Range_For_EVs, aes(x= Tesla, y = AVG_Range, fill = Tesla)) +
  geom_col() +
  labs(title = "AVG Electric Range (BEV)") +
  theme_minimal()
         
#MSRP
ggplot(Average_Range_For_EVs, aes(x= Tesla, y = AVG_MSRP, fill = Tesla)) +
  geom_col() +
  labs(title = "AVG MSRP (BEV)") +
  theme_minimal()






#4. PHEV vs BEV Trends

bhev_vs_bev <- clean_df %>%
  group_by(Model_Year, Electric_Vehicle_Type)%>%
  summarise(count = n(), .groups = "drop")




ggplot(bhev_vs_bev, aes(x=Model_Year, y= count, color= Electric_Vehicle_Type))+
  geom_line(size= 1.1)+
  geom_point(size=2)+
  labs(title= "PHEV vs BEV Trends Over Time")+
  theme_minimal()



# 5. Top Electric utilities in Washington for Tesla

Utility_Count_for_Teslas <- clean_df %>%
  filter(Tesla== "TESLA") %>%
  group_by(Electric_Utility)%>%
  summarise(count = n(), .groups = "drop")%>%
  arrange(desc(count))%>%
  head(5)

ggplot(Utility_Count_for_Teslas, aes(x = Electric_Utility, count, y = count)) +
  geom_col(fill = "steelblue") +
  coord_flip()+
  labs(title= "Top Electric Utilities for Tesla in Washington", x = "Utility", y = "Tesla Count") +
  theme_minimal()+
  theme(axis.text.y = element_text(size = 4))
