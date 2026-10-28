
library(tidyverse)

final_data |>
  count(ARID1A_mutation, ARID1B_mutation)


eda_data <- final_data |>
  filter(
    ARID1B_mutation == 0,
    !is.na(ARID1A_mutation),
    !is.na(ARID1B_effect)
  )

eda_data |>
  group_by(ARID1A_mutation) |>
  summarise(
    n = n(),
    mean = mean(ARID1B_effect),
    median = median(ARID1B_effect),
    sd = sd(ARID1B_effect),
    .groups = "drop"
  )

ggplot(
  eda_data,
  aes(
    x = factor(ARID1A_mutation),
    y = ARID1B_effect
  )
) +
  geom_boxplot() +
  labs(
    x = "ARID1A damaging mutation status (0, 1, 2)",
    y = "ARID1B gene-effect score",
    title = "ARID1B dependency by ARID1A mutation category"
  ) +
  theme_minimal()


# Welch's ANOVA
oneway.test(
  ARID1B_effect ~ factor(ARID1A_mutation),
  data = eda_data,
  var.equal = FALSE
)

# Kruskal-Wallis
kruskal.test(
  ARID1B_effect ~ factor(ARID1A_mutation),
  data = eda_data
)

# pairwise t-tests with Holm correction
pairwise.t.test(
  eda_data[["ARID1B_effect"]],
  factor(eda_data[["ARID1A_mutation"]]),
  pool.sd = FALSE,
  p.adjust.method = "holm"
)

# data functions
eda_data |>
  group_by(ARID1A_mutation) |>
  summarise(
    n = n(),
    mean = mean(ARID1B_effect),
    median = median(ARID1B_effect),
    sd = sd(ARID1B_effect),
    IQR = IQR(ARID1B_effect),
    .groups = "drop"
  )

# t-test for 1 and 2 ARID1A_mutations
result <- t.test(
  ARID1B_effect ~ factor(ARID1A_mutation),
  data = eda_data |>
    filter(
      ARID1A_mutation == 1 | ARID1A_mutation == 2
    )
)

result
