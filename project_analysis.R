cosmetics <- read.csv("/Users/naish/naishvi_shah_project/cosmetics 2.csv")
nrow(cosmetics)

# ANALYSIS 1: How many products are in each category?
category_counts <- table(cosmetics$Label)
print(category_counts)
barplot(category_counts,
        main = "Number of Products by Category",
        xlab = "Category",
        ylab = "Number of Products",
        col = "pink",
        las = 2,          # rotates x-axis labels
        cex.names = 0.7)  # shrinks label text so it fits

# ANALYSIS 2: What is the average price per category?
avg_price <- tapply(cosmetics$Price, cosmetics$Label, mean, na.rm = TRUE)
avg_price_rounded <- round(avg_price, 2)
print(avg_price_rounded)
barplot(sort(avg_price_rounded, decreasing = TRUE),
        main = "Average Price by Product Category",
        xlab = "Category",
        ylab = "Average Price (USD)",
        col = "lightcoral",
        las = 2,
        cex.names = 0.7)

# ANALYSIS 3: What is the average rating per category?
avg_rating <- tapply(cosmetics$Rank, cosmetics$Label, mean, na.rm = TRUE)
avg_rating_rounded <- round(avg_rating, 2)
print(avg_rating_rounded)
barplot(sort(avg_rating_rounded, decreasing = TRUE),
        main = "Average Rating by Product Category",
        xlab = "Category",
        ylab = "Average Rating",
        col = "lightyellow",
        las = 2,
        cex.names = 0.7,
        ylim = c(0, 5))

# ANALYSIS 4: Price distribution (histogram)
hist(cosmetics$Price,
     main = "Distribution of Product Prices",
     xlab = "Price (USD)",
     ylab = "Number of Products",
     col = "lavender",
     breaks = 20)


# ANALYSIS 5: How many products work for each skin type?

skin_types <- c("Combination", "Dry", "Normal", "Oily", "Sensitive")
skin_counts <- colSums(cosmetics[, skin_types], na.rm = TRUE)
print(skin_counts)
barplot(skin_counts,
        main = "Number of Products Suitable per Skin Type",
        xlab = "Skin Type",
        ylab = "Number of Products",
        col = c("peachpuff", "lightblue", "lightgreen", "lightyellow", "plum"))


# ANALYSIS 6: Does higher price mean better rating?

plot(cosmetics$Price, cosmetics$Rank,
     main = "Price vs. Rating",
     xlab = "Price (USD)",
     ylab = "Rating",
     col = "steelblue",
     pch = 16)  # filled circle points
# Add a trend line
abline(lm(cosmetics$Rank ~ cosmetics$Price), col = "red", lwd = 2)

# ANALYSIS 7: Top 10 highest rated products
top10_rated <- cosmetics[order(cosmetics$Rank, decreasing = TRUE), ]
top10_rated <- top10_rated[1:10, c("Brand", "Name", "Price", "Rank")]
print(top10_rated)
