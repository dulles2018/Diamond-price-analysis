############################################################
# Diamond Price Analysis
# Author: Sruthirekha Ashokkumar
# Description:
# This project explores the relationship between diamond
# characteristics and price using the built-in diamonds
# dataset from the ggplot2 package.
############################################################

# ===============================
# Load Required Packages
# ===============================

library(ggplot2)
library(dplyr)
library(GGally)
library(corrplot)

# ===============================
# Load Dataset
# ===============================

data("diamonds")

# ===============================
# Explore the Dataset
# ===============================

head(diamonds)
str(diamonds)
summary(diamonds)
dim(diamonds)

# Check for missing values
colSums(is.na(diamonds))

# ===============================
# Histogram of Diamond Prices
# ===============================

price_hist <- ggplot(diamonds, aes(price)) +
  geom_histogram(binwidth = 500, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of Diamond Prices",
    x = "Price",
    y = "Count"
  )

price_hist

# Save Plot
ggsave(
  filename = "plots/histogram_price.png",
  plot = price_hist,
  width = 8,
  height = 5
)

# ===============================
# Histogram of Carat
# ===============================

carat_hist <- ggplot(diamonds, aes(carat)) +
  geom_histogram(bins = 30, fill = "darkgreen", color = "black") +
  labs(
    title = "Distribution of Diamond Carat",
    x = "Carat",
    y = "Count"
  )

carat_hist

ggsave(
  filename = "plots/histogram_carat.png",
  plot = carat_hist,
  width = 8,
  height = 5
)

# ===============================
# Boxplot by Cut
# ===============================

cut_plot <- ggplot(diamonds,
                   aes(cut, price, fill = cut)) +
  geom_boxplot() +
  theme(legend.position = "none") +
  labs(
    title = "Diamond Price by Cut",
    x = "Cut",
    y = "Price"
  )

cut_plot

ggsave(
  filename = "plots/boxplot_cut.png",
  plot = cut_plot,
  width = 8,
  height = 5
)

# ===============================
# Boxplot by Color
# ===============================

color_plot <- ggplot(diamonds,
                     aes(color, price, fill = color)) +
  geom_boxplot() +
  theme(legend.position = "none") +
  labs(
    title = "Diamond Price by Color",
    x = "Color",
    y = "Price"
  )

color_plot

ggsave(
  filename = "plots/boxplot_color.png",
  plot = color_plot,
  width = 8,
  height = 5
)

# ===============================
# Boxplot by Clarity
# ===============================

clarity_plot <- ggplot(diamonds,
                       aes(clarity, price, fill = clarity)) +
  geom_boxplot() +
  theme(legend.position = "none") +
  labs(
    title = "Diamond Price by Clarity",
    x = "Clarity",
    y = "Price"
  )

clarity_plot

ggsave(
  filename = "plots/boxplot_clarity.png",
  plot = clarity_plot,
  width = 8,
  height = 5
)

# ===============================
# Scatter Plot
# ===============================

scatter_plot <- ggplot(diamonds,
                       aes(carat, price)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Relationship Between Carat and Price",
    x = "Carat",
    y = "Price"
  )

scatter_plot

ggsave(
  filename = "plots/scatter_carat_price.png",
  plot = scatter_plot,
  width = 8,
  height = 5
)

# ===============================
# Correlation Matrix
# ===============================

numeric_data <- diamonds %>%
  select(carat, depth, table, price, x, y, z)

cor_matrix <- cor(numeric_data)

print(cor_matrix)

corrplot(
  cor_matrix,
  method = "color",
  tl.cex = 0.8
)

png(
  filename = "plots/correlation_heatmap.png",
  width = 800,
  height = 600
)

corrplot(
  cor_matrix,
  method = "color",
  tl.cex = 0.8
)

dev.off()

# ===============================
# Summary Statistics
# ===============================

summary(diamonds)

diamonds %>%
  group_by(cut) %>%
  summarise(
    AveragePrice = mean(price),
    MedianPrice = median(price)
  )

diamonds %>%
  group_by(color) %>%
  summarise(
    AveragePrice = mean(price)
  )

diamonds %>%
  group_by(clarity) %>%
  summarise(
    AveragePrice = mean(price)
  )

# ===============================
# Linear Regression Model
# ===============================

model <- lm(
  price ~ carat + depth + table + x + y + z,
  data = diamonds
)

summary(model)

# ===============================
# Predict Diamond Price
# ===============================

prediction <- predict(
  model,
  newdata = data.frame(
    carat = 1,
    depth = 61,
    table = 57,
    x = 6.4,
    y = 6.4,
    z = 3.9
  )
)

prediction

# ===============================
# End of Analysis
# ===============================
