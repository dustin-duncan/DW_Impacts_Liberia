#April 17th 2025
#Taylor Cook
#DistantWaterImapct GP


# load libs
library(ggplot2)
library(dplyr)
library(tidyr)


#Graphing the B/BMSY results

#create dataframe with info above

bmsy_df <- data.frame(
  Category = c("Biomass"),
  Indicator = c("End B/BMSY"),
  V1 = c(0.204),
  V2 = c(0.205),
  V3 = c(0.207),
  V4 = c(0.220),
  check.names = FALSE  # prevents automatic renaming
)

# Now set the column names to what you want, even with % and spaces
colnames(bmsy_df) <- c(
  "Category", 
  "Indicator", 
  "Business as Usual (10%)", 
  "50% Access Fee", 
  "100% Access Fee", 
  "Immediate Ban"
)



# Pivot the table for the plot
bmsy_df_long <- bmsy_df %>%
  pivot_longer(
    cols = -c(Category, Indicator),
    names_to = "Scenario",
    values_to = "Value"
  )

bmsy_df_long$Scenario <- factor(
  bmsy_df_long$Scenario,
  levels = c("Business as Usual (10%)","Immediate Ban", "50% Access Fee", "100% Access Fee")
)



# Create the bar plot
bmsy_plot <- ggplot(bmsy_df_long, aes(x = Scenario, y = Value, fill = Scenario)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", linewidth = 0.1) +
  geom_text(aes(label = round(Value, 6)), vjust = -0.5) +
  scale_fill_manual(values = c("Business as Usual (10%)" = "tan4", 
                                "50% Access Fee" = "forestgreen", 
                                "100% Access Fee" = "steelblue", 
                                "Immediate Ban" = "lightblue1")) +
  scale_y_continuous(limits = c(0, 0.25), breaks = seq(0, 0.25, by = 0.05)) +
  labs(title = "End B/BMSY by Scenario",
       x = "Scenario",
       y = "End B/BMSY") +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

bmsy_plot


#export plot as jpeg
ggsave(here::here("plots/bbmsy_bar_plot.jpeg"), plot = bmsy_plot, width = 8, height = 4.5)

bmsy_df_long1 <- bmsy_df_long %>% 
  filter(Scenario != "100% Access Fee")

# Create the bar plot
bmsy_plot1 <- ggplot(bmsy_df_long1, aes(x = Scenario, y = Value, fill = Scenario)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", linewidth = 0.1) +
  geom_text(aes(label = round(Value, 6)), vjust = -0.5) +
  scale_fill_manual(values = c("Business as Usual (10%)" = "tan4", 
                               "50% Access Fee" = "forestgreen", 
                               # "100% Access Fee" = "steelblue", 
                               "Immediate Ban" = "lightblue1")) +
  scale_y_continuous(limits = c(0, 0.25), breaks = seq(0, 0.25, by = 0.05)) +
  labs(title = "End B/BMSY by Scenario",
       x = "Scenario",
       y = "End B/BMSY") +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

bmsy_plot1


#export plot as jpeg
ggsave(here::here("plots/bbmsy_bar_plot_no_optimization.jpeg"), plot = bmsy_plot1, width = 8, height = 4.5)


################################################################

#Graphing the model USD value outputs

# Create a data frame with the values
modelvalue_df <- data.frame(
  Scenario = c("Business As Usual (10%)", "50% Access Fee", "100% Access Fee", "Immediate Ban"),
  `SSF Value ($USD)` = c(913470, 914287, 915273, 926229),
  `NaFAA Value ($USD)` = c(486210, 597352, 720837, 472079),
  check.names = FALSE  # <-- important!
)


#plot in a bar graph
modelvalue_df_long <- modelvalue_df %>%
  pivot_longer(
    cols = -Scenario,
    names_to = "Model",
    values_to = "Value"
  )
modelvalue_df_long$Model <- factor(
  modelvalue_df_long$Model,
  levels = c("SSF Value ($USD)", "NaFAA Value ($USD)")
)

modelvalue_df_long$Scenario <- factor(
  modelvalue_df_long$Scenario,
  levels = c("Business As Usual (10%)", "Immediate Ban", "50% Access Fee", "100% Access Fee")
)

# Calculate total stack height per scenario
stack_heights <- modelvalue_df_long %>%
  group_by(Scenario) %>%
  summarise(total = sum(Value))

# Get the max total height
ymax <- max(stack_heights$total)

# Update the plot
modelvalue_plot <- ggplot(modelvalue_df_long, aes(x = Scenario, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = "stack", color="black", linewidth=0.1) +
  geom_text(aes(label = paste0("$", round(Value / 1000), "K")), 
            vjust = 0.5, position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("SSF Value ($USD)" = "burlywood", 
                               "NaFAA Value ($USD)" = "steelblue")) +
  scale_y_continuous(limits = c(0, ymax * 1.1),  # Add a little buffer
                     breaks = seq(0, ymax * 1.1, by = 200000)) +
  labs(title = "Projected Benefits by Scenario",
       x = "Scenario",
       y = "Value ($USD)",
       fill = NULL) +
  theme_bw() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  )

modelvalue_plot


#export plot as jpeg
ggsave(here::here("plots/modelvalue_bar_plot.jpeg"), plot = modelvalue_plot, width = 8, height = 4.5)

modelvalue_df_long1 <- modelvalue_df_long %>% 
  filter(Scenario != "100% Access Fee")

## one more time 
modelvalue_plot1 <- ggplot(modelvalue_df_long1, aes(x = Scenario, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = "stack", color="black", linewidth=0.1) +
  geom_text(aes(label = paste0("$", round(Value / 1000), "K")), 
            vjust = 0.5, position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("SSF Value ($USD)" = "burlywood", 
                               "NaFAA Value ($USD)" = "steelblue")) +
  scale_y_continuous(limits = c(0, ymax * 1.1),  # Add a little buffer
                     breaks = seq(0, ymax * 1.1, by = 200000)) +
  labs(title = "Projected Benefits by Scenario",
       x = "Scenario",
       y = "Value ($USD)",
       fill = NULL) +
  theme_bw() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  )

modelvalue_plot1


#export plot as jpeg
ggsave(here::here("plots/modelvalue_bar_plot_no_optimization.jpeg"), plot = modelvalue_plot1, width = 8, height = 4.5)
