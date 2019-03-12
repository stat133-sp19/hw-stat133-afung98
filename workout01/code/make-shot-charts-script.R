#Shot Charts Code Script

#importing court image to use as background
court_file <- "~/workout01/images/nba-court.jpg"

# create raste object
library(grid)
court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, "npc"),
  height = unit(1, "npc"))

#create chart for Curry
library(ggplot2)
curry_shot_chart <- ggplot(data = curry) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Stephen Curry (2016 season)') +
  theme_minimal()
pdf(file = "~/workout01/images/stephen-curry-shot-chart.pdf", width = 6.5, height = 5)
curry_shot_chart
dev.off()

#create chart for Thompson
thompson_shot_chart <- ggplot(data = thompson) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Klay Thompson (2016 season)') +
  theme_minimal()
pdf(file = "~/workout01/images/klay-thompson-shot-chart.pdf", width = 6.5, height = 5)
curry_shot_chart
dev.off()

#create chart for Durant
durant_shot_chart <- ggplot(data = durant) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Kevin Durant (2016 season)') +
  theme_minimal()
pdf(file = "~/workout01/images/kevin-durant-shot-chart.pdf", width = 6.5, height = 5)
curry_shot_chart
dev.off()

#create chart for Green
green_shot_chart <- ggplot(data = green) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Draymond Green (2016 season)') +
  theme_minimal()
pdf(file = "~/workout01/images/draymond-green-shot-chart.pdf", width = 6.5, height = 5)
curry_shot_chart
dev.off()

#create chart for Iguodala
iguodala_shot_chart <- ggplot(data = iguodala) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Andre Iguodala (2016 season)') +
  theme_minimal()
pdf(file = "~/workout01/images/andre-iguodala-shot-chart.pdf", width = 6.5, height = 5)
curry_shot_chart
dev.off()

#create facetted shot chart
facetted_shot_chart <- ggplot(data = bind_dataframe)+
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Facetted by Player (2016 season)') +
  theme_minimal() +
  facet_wrap(~ name)
facetted_shot_chart
pdf(file = "~/workout01/images/gsw-shot-chart.pdf", width = 8, height = 7)
facetted_shot_chart
dev.off()