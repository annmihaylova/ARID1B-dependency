library(tidyverse)
library(data.table)

#Reading data

crispr <- read_csv(
  "data/raw/CRISPRGeneEffect.csv",
  col_select = c(
    1,
    `ARID1A (8289)`,
    `ARID1B (57492)`
  )
)

names(crispr)[1] <- "ModelID"

expression <- read_csv(
  "data/raw/OmicsExpressionTPMLogp1HumanProteinCodingGenes.csv",
  col_select = c(
    ModelID,
    IsDefaultEntryForModel,
    `ARID1A (8289)`,
    `ARID1B (57492)`
  )
) |> filter(IsDefaultEntryForModel == "Yes")

mutations <- read_csv(
  "data/raw/OmicsSomaticMutationsMatrixDamaging.csv",
  col_select = c(
    ModelID,
    IsDefaultEntryForModel,
    `ARID1A (8289)`,
    `ARID1B (57492)`
  )
) |> filter(IsDefaultEntryForModel == "Yes")

model <- read_csv(
  "data/raw/Model.csv",
  col_select = c(
    ModelID,
    CellLineName,
    OncotreeLineage,
    OncotreePrimaryDisease
  )
)

glimpse(crispr)
glimpse(model)
glimpse(mutations)
glimpse(expression)


expression |>
  count(ModelID) |>
  filter(n > 1)

mutations |>
  count(ModelID) |>
  filter(n > 1)

# Rename measurements according to the meaning

crispr <- crispr |>
  rename(
    ARID1A_effect = `ARID1A (8289)`,
    ARID1B_effect = `ARID1B (57492)`
  )

expression <- expression |>
  select(-IsDefaultEntryForModel) |>
  rename(
    ARID1A_expression = `ARID1A (8289)`,
    ARID1B_expression = `ARID1B (57492)`
  )

mutations <- mutations |>
  select(-IsDefaultEntryForModel) |>
  rename(
    ARID1A_mutation = `ARID1A (8289)`,
    ARID1B_mutation = `ARID1B (57492)`
  )

# Merge datasets by ModelID

list_of_dts <- lapply(
  list(crispr, model, mutations, expression),
  as.data.table
)

final_data <- Reduce(
  function(x, y) merge(x, y, by = "ModelID"),
  list_of_dts
)

glimpse(final_data)

