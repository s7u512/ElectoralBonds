# source: https://github.com/DigitalIndiaArchiver/ElectoralBondsData

rm(list = ls())

library(tidyverse)
library(readr)


purchased <- read_csv("Resources/purchased.csv")
encashed <- read_csv("Resources/encashed.csv")


###########

df1 <- encashed %>% group_by(`Name of the Political Party`) %>% summarise(sum = sum(Denominations))


df2 <- purchased %>% group_by(`Name of the Purchaser`) %>% summarise(sum = sum(Denominations))


############
all_matched <- left_join(encashed, purchased)

# Step 1: Calculate total encashed per party
total_encashed <- encashed %>%
  group_by(`Name of the Political Party`) %>%
  summarise(TotalEncashed = sum(Denominations), .groups = 'drop')



# Generating a dataframe with sums of purchases for each purchaser per party
purchaser_sums <- all_matched %>%
  group_by(`Name of the Political Party`, `Name of the Purchaser`) %>%
  summarise(TotalPurchased = sum(Denominations), .groups = 'drop')

# Ranking purchasers within each party based on the total purchase amount
purchaser_rankings <- purchaser_sums %>%
  arrange(`Name of the Political Party`, desc(TotalPurchased)) %>%
  group_by(`Name of the Political Party`) %>%
  mutate(Rank = row_number())


# Summarizing the key statistics from purchaser_rankings
party_stats <- purchaser_rankings %>%
  group_by(`Name of the Political Party`) %>%
  summarise(
    NumberOfPurchasers = n(),
    TopPurchaser = first(`Name of the Purchaser`),
    TopPurchaserAmount = first(TotalPurchased),
    TopTenPurchaserAmount = sum(ifelse(Rank <= 10, TotalPurchased, 0))
  )

# Merging with the total encashed amounts
partywise_stats <- total_encashed %>%
  left_join(party_stats, by = "Name of the Political Party") %>%
  mutate(ShareOfTopTenPurchasersInTotalEncashed = round(TopTenPurchaserAmount / TotalEncashed * 100, 2))

# Adding serial numbers and arranging columns
partywise_stats <- partywise_stats %>%
  mutate(`Sl No.` = row_number()) %>%
  select(`Sl No.`, `Name of the Political Party`, TotalEncashed, NumberOfPurchasers, 
         TopPurchaser, TopPurchaserAmount, TopTenPurchaserAmount, ShareOfTopTenPurchasersInTotalEncashed)


write.csv(partywise_stats, "party_wise_stats.csv", row.names = FALSE)
