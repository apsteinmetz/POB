# get estimated publish date of gutenberg books


gutenberg_metadata_2 <- gutenberg_metadata |>
  left_join(gutenberg_authors, by = c("author","gutenberg_author_id")) |>
  mutate(author_lifespan = deathdate - birthdate) |>
  mutate(est_pubish_date = round(birthdate + 18 + (author_lifespan-18) / 2))


gutenberg_metadata_2 |>
  # plot birthdate vs lifespan
  ggplot(aes(x = birthdate, y = author_lifespan)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Author Lifespan vs Birthdate",
    x = "Birth Year",
    y = "Author Lifespan (years)"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(-500,2000, by = 100))



# plot histogram of estimated publish date
gutenberg_metadata_2 |>
  filter(est_publish_date > 1500) |>
  ggplot(aes(x = est_pubish_date)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(
    title = "Estimated Publish Date of Gutenberg Books",
    x = "Estimated Publish Year",
    y = "Count"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(-500,2000, by = 100))


