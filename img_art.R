# Load an image into R (make sure your working directory is set!)

# Packages (for the entire project)
library(imager)    # image loading and processing
library(dplyr)     # data manipulation
library(ggplot2)   # data visualization
library(tidyr)     # data wrangling
library(ggvoronoi) # visualization
library(knitr)
library(kableExtra)
library(here)
# Load an image into R
img <- load.image(file = here('images', 'beach2.jpg'))

# Print the image object out
print(img)

# View the image
plot(img)

# Represent the image as a data frame
img_df <- as.data.frame(img)

# Show a table of the first 10 rows of the data frame
img_df %>% 
  arrange(x, y, cc) %>% # sort by columns for viewing
  filter(row_number() < 10) %>% # Select top 10 columns
  kable("html") %>%  # Display table in R Markdown
  kable_styling(full_width = F) # Don't take up full width

# Add more expressive labels to the colors
img_df <- img_df %>% 
  mutate(channel = case_when(
    cc == 1 ~ "Red",
    cc == 2 ~ "Green", 
    cc == 3 ~ "Blue"
  ))

# Reshape the data frame so that each row is a point
img_wide <- img_df %>%
  select(x, y, channel, value) %>%
  spread(key = channel, value = value) %>%
  mutate(
    color = rgb(Red, Green, Blue)
  )


# Take a sample of rows from the data frame
sample_size <- 200000
img_sample <- img_wide[sample(nrow(img_wide), sample_size), ]

# Plot only the sampled points
ggplot(img_sample) +
  geom_point(mapping = aes(x = x, y = y, color = color),size = 1.1, alpha = .75) +
  scale_color_identity() + # use the actual value in the `color` column
  scale_y_reverse() + # Orient the image properly (it's upside down!)
  theme_void() # Remove axes, background

# Create random weights for point size
img_sample$size <- runif(sample_size)

# Plot only the sampled points
ggplot(img_sample) +
  geom_point(mapping = aes(x = x, y = y, color = color, size = size)) +
  guides(size = FALSE) + # don't show the legend
  scale_color_identity() + # use the actual value in the `color` column
  scale_y_reverse() + # Orient the image properly (it's upside down!)
  theme_void()

# Use the amount of blue present in each point to determine the size
ggplot(img_sample) +
  geom_point(mapping = aes(x = x, y = y, color = color, size = Red)) +
  guides(size = FALSE) + # don't show the legend
  scale_color_identity() + # use the actual value in the `color` column
  scale_y_reverse() + # Orient the image properly (it's upside down!)
  theme_void() # Remove axes, background

ggsave(file = 'pgh_plot.png', bg = 'transparent')
