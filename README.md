# GV300 Assignment 6

## Text analysis and data visualisation

This repository contains my work for Assignment 6, in which I apply text analysis techniques in R to explore political language and identify key themes.

The analysis includes:
- Tokenising text into individual words  
- Counting word frequency  
- Visualising dominant political concepts  

## View my portfolio website

You can view my interactive portfolio here:  
👉 https://arisarasuksawang-ui.github.io/gv300-website/

## Key skills demonstrated
- Text analysis (tidytext)  
- Data visualisation (ggplot2)  
- Reproducible analysis in R  

## Example output
This assignment includes text analysis and data visualisation carried out in R.

# This is assignment 1 (How family advantage and Maternal IQ shape children's test scores)

```{r}
ggplot(kidiq_2, aes(x = mom_age)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 2,
                 fill = "#2C7FB8",
                 colour = "white",
                 alpha = 0.8) +
  geom_density(colour = "#08306B", linewidth = 1.2) +
  geom_vline(aes(xintercept = mean(mom_age, na.rm = TRUE)),
             colour = "red",
             linetype = "dashed",
             linewidth = 1) +
  labs(
    title = "Distribution of Mothers' Ages",
    subtitle = "Histogram with density curve and mean age",
    x = "Mother's Age",
    y = "Density",
    caption = "Source: kidiq dataset"
  ) +
  theme_minimal(base_size = 14)
```
https://arisarasuksawang-ui.github.io/gv300-website/profile_files/figure-html5/unnamed-chunk-1-1.png

```{r} 
your_plot <- kidiq %>%
  filter(!is.na(family_advantage),
         !is.na(iq_group),
         !is.na(kid_score),
         !is.na(work_status)) %>%
  ggplot(aes(x = family_advantage, 
             y = kid_score, 
             colour = iq_group, 
             size = mom_iq)) +
  geom_jitter(alpha = 0.4, width = 0.15, height = 0) + 
  stat_summary(fun = mean, geom = "point", colour = "black", 
               shape = 18, size = 3) + 
  facet_wrap(~ work_status) +
  scale_size_continuous(name = "Mother's IQ", range = c(0.5, 3)) +
  labs(
    title = "How Family Advantage and Maternal IQ Shape Children's Test Scores",
    subtitle = "Children from higher-advantage families tend to achieve higher test scores. each group, higher maternal IQ is associated with better outcomes.",
    x = "Family Advantage (Mother's Education × Work Status)",
    y = "Child's Test Score",
    colour = "Maternal IQ Group",
    caption = "Data: kidiq-2.csv (National Longitudinal Survey of Youth sample)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 15, hjust = 1, colour = "black")
  )

#Uncomment to view your plot
print(your_plot)
```
https://34de43ac144c4f43b2dc18abdc6d286a.app.posit.cloud/graphics/26c53ca6-a1c4-4d80-88a5-c6fcee8e3dee.png

# This is assignment 1 (Beyond ability: structure shifts the IQ-scores relationship) 
```{r}
kidiq <- kidiq %>%
  mutate(
    # ability bands (already in your starter)
    ability_level = case_when(
      mom_iq <= quantile(mom_iq, 0.25, na.rm = TRUE) ~ "Low ability",
      mom_iq <= quantile(mom_iq, 0.75, na.rm = TRUE) ~ "Medium ability",
      TRUE ~ "High ability"
    ),
    # outcome bands (already in your starter)
    outcome_level = case_when(
      kid_score <= quantile(kid_score, 0.25, na.rm = TRUE) ~ "Low outcome",
      kid_score <= quantile(kid_score, 0.75, na.rm = TRUE) ~ "Medium outcome",
      TRUE ~ "High outcome"
    ),
    # surprising cases (already in your starter)
    surprising_case = case_when(
      ability_level == "High ability" & outcome_level == "Low outcome" ~ "High ability, low outcome",
      ability_level == "Low ability"  & outcome_level == "High outcome" ~ "Low ability, high outcome",
      TRUE ~ "Expected pattern"
    ),

# >>> Make another one yourself: a simple structural index
# Higher is "more structure": HS (1) + working 3/4 (1), else 0–2
    hs_flag   = if_else(mom_hs == 1, 1L, 0L),
    work_flag = if_else(mom_work >= 3, 1L, 0L),
    structure_index = hs_flag + work_flag,  # 0..2

# Tidy labels for facets/legends (if not already created earlier)
    work_status = case_when(
      mom_work == 1 ~ "Not working",
      mom_work == 2 ~ "Part-time",
      mom_work == 3 ~ "Full-time",
      mom_work == 4 ~ "Full-time+",
      TRUE ~ NA_character_
    ),
    mom_hs_lab = if_else(mom_hs == 1, "HS or higher", "No HS"),

# Factors for stable ordering
    work_status = factor(work_status,
                         levels = c("Not working","Part-time","Full-time","Full-time+")),
    mom_hs_lab  = factor(mom_hs_lab, levels = c("No HS","HS or higher"))
  )

library(dplyr)
library(ggplot2)

k2 <- kidiq %>%
  filter(!is.na(mom_iq), !is.na(kid_score),
         !is.na(work_status), !is.na(mom_hs_lab), !is.na(surprising_case))

# Counts for the subtitle
sc <- k2 %>%
  count(surprising_case) %>%
  tidyr::pivot_wider(names_from = surprising_case, values_from = n, values_fill = 0)

subtitle_text <- sprintf(
  "IQ relates to scores, but structure shifts levels and slopes. Surprising cases: %d high IQ -> low outcome; %d low IQ -> high outcome.",
  sc[["High ability, low outcome"]], sc[["Low ability, high outcome"]]
)

your_plot_2 <- ggplot(k2, aes(x = mom_iq, y = kid_score, colour = mom_hs_lab)) +
  # individual observations
  geom_point(alpha = 0.55) +
  # highlight surprising cases with an outline
  geom_point(
    data = subset(k2, surprising_case != "Expected pattern"),
    aes(x = mom_iq, y = kid_score),
    inherit.aes = FALSE, shape = 21, fill = NA, colour = "black", size = 2.8, stroke = 0.9
  ) +
  # linear trends per education within each work context
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ work_status) +
  labs(
    title = "Beyond Ability: structure shifts the IQ–score relationship",
    subtitle = subtitle_text,
    x = "Mother's IQ (proxy for ability)",
    y = "Child's test score",
    colour = "Maternal education",
    caption = "Data: kidiq-2.csv (NLSY sample used in Gelman & Hill)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    strip.text = element_text(colour = "black"),
    axis.text = element_text(colour = "black")
  )
  
# Uncomment to view your plot
print(your_plot_2)
```
https://34de43ac144c4f43b2dc18abdc6d286a.app.posit.cloud/chunk_output/s/F7587C21/c385p5flubxol/000002.png

---
## Assignment 2
### Visualisation (democracy, voting, and representation appear most frequently)
```{r}
library(tidytext)
library(dplyr)
library(ggplot2)

text <- data.frame(
  line = 1:3,
  text = c(
    "Democracy depends on participation and representation.",
    "Voting turnout and political engagement are central to democratic politics.",
    "Representation, democracy and voting are key themes in political analysis."
  )
)

words <- text %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

word_counts <- words %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n = 10)


ggplot(word_counts, aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "#2C7FB8", width = 0.7) +
  coord_flip() +
  labs(
    title = "Most frequent political terms",
    subtitle = "Word frequency after removing common stop words",
    x = "",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, colour = "grey40"),
    panel.grid.major.y = element_blank()
  )
```
## Visualisation (Distribution of constituencies by turnout category)

```{r cats-plot, eval=TRUE}
ggplot(category_counts, aes(x = category, y = count, fill = country)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Distribution of Constituencies by Turnout Category",
    subtitle = "Most constituencies fall in the 'High' turnout category",
    x = "Turnout Category",
    y = "Number of Constituencies",
    fill = "Country"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

```
https://6a29701ade0c482c8f28f3118230c144.app.posit.cloud/chunk_output/s/9B1C6115/copf8rzjzs6ot/000004.png?fixed_size=1





---

## Assignment 3
#  Simulating the A/B Test Data 
```{r plot-fitted, fig.width=6, fig.height=4}

# Prepare a data frame for group-level summaries
group_df <- data.frame(
  treatment = c(0, 1),
  mean_signup = c(mean_control, mean_treated),
  model = "LM group means"
)

logit_df <- data.frame(
  treatment = c(0, 1),
  mean_signup = c(p_hat_control, p_hat_treated),
  model = "Logit predicted probs"
)

# Main ggplot
ggplot(party_ab, aes(x = factor(treatment), y = support_signup)) +
  geom_jitter(
    width = 0.1,
    height = 0,
    alpha = 0.1,
    color = "black"
  ) +
  geom_line(
    data = group_df,
    aes(x = factor(treatment), y = mean_signup, group = 1, color = model),
    linewidth = 1
  ) +
  geom_point(
    data = group_df,
    aes(x = factor(treatment), y = mean_signup, color = model),
    size = 3
  ) +
  geom_point(
    data = logit_df,
    aes(x = factor(treatment), y = mean_signup, color = model),
    shape = 17,
    size = 3
  ) +
  scale_x_discrete(labels = c("Control", "Treatment")) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(
    values = c(
      "LM group means" = "blue",
      "Logit predicted probs" = "red"
    )
  ) +
  labs(
    x = "Treatment",
    y = "Support signup (0/1)",
    title = "Observed outcomes and fitted probabilities",
    color = NULL
  ) +
  theme_minimal()

```
https://ec2f598bc75742c49c534698de01adf4.app.posit.cloud/chunk_output/302A5AAC4fb51ded/912C7FD0/co2j1673re0fe/000006.png?fixed_size=1


---










































