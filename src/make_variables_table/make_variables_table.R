library(broom)
library(dplyr)
library(glue)
library(gt)
library(orderly2)
library(purrr)
library(tidyr)
library(zip)


# Function to create aligned column name table
align_column_names <- function(df_list, df_names = NULL) {
  # If names not provided, use the list names or create default names
  if (is.null(df_names)) {
    df_names <- names(df_list)
    if (is.null(df_names)) {
      df_names <- paste0("df", seq_along(df_list))
    }
  }

  # Get all unique column names across all dataframes
  all_cols <- unique(unlist(lapply(df_list, names)))

  # Create a matrix to store the results
  result <- matrix("", nrow = length(all_cols), ncol = length(df_list))
  colnames(result) <- df_names

  # Fill in the matrix
  for (i in seq_along(df_list)) {
    df_cols <- names(df_list[[i]])
    for (j in seq_along(df_cols)) {
      col_name <- df_cols[j]
      # Find which row this column name should go in
      row_idx <- which(all_cols == col_name)
      result[row_idx, i] <- col_name
    }
  }

  # Convert to data frame
  result_df <- as.data.frame(result, stringsAsFactors = FALSE)

  result_df
}


orderly_dependency("process_benin", "latest", "benin_split.rds")
orderly_dependency("process_drc", "latest", "drc_baseline_split.rds")
orderly_dependency("process_drc_endline", "latest", "drc_endline_split.rds")
orderly_dependency("process_burkina_faso", "latest", "bfa_baseline_split.rds")
orderly_dependency(
  "process_burkina_faso_endline", "latest", "bfa_endline_split.rds"
)

benin <- readRDS("benin_split.rds")
drc <- readRDS("drc_baseline_split.rds")
drc_2021 <- readRDS("drc_endline_split.rds")
bfa_baseline <- readRDS("bfa_baseline_split.rds")
bfa_endline <- readRDS("bfa_endline_split.rds")


df_list <-
  list(benin[[1]], drc[[1]], drc_2021[[1]], bfa_baseline[[1]], bfa_endline[[1]])

# Create the alignment table
alignment_table <-
  align_column_names(
    df_list,
    df_names = c("Benin (2010)", "DRC (2015)", "DRC (2021)", "BFA (2013)",
                 "BFA (2017)")
  )


# Create gt table
gt_table <- alignment_table %>%
  gt() %>%
  tab_header(
    title = "Column Name Alignment Across Dataframes"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_borders(sides = "all", color = "gray80", weight = px(1)),
    locations = cells_body()
  )

  # Style the Group column with background color and bold text
  tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_fill(color = "lightblue")
    ),
    locations = cells_body(columns = Group)
  ) %>%
  # Apply CSS rotation to Group column
  opt_css(
    css = "
    .gt_col_heading:first-child {
      writing-mode: vertical-rl;
      transform: rotate(180deg);
    }
    tbody tr td:first-child {
      writing-mode: vertical-rl;
      transform: rotate(180deg);
      text-align: center;
      vertical-align: middle;
    }
    "
  ) %>%
  # Adjust column width for the rotated group column
  cols_width(
    Group ~ px(40)
  )

# Add horizontal lines between groups (midrule style)
# Find the boundaries between groups
group_boundaries <- c()
for (i in 1:(nrow(alignment_table) - 1)) {
  if (alignment_table$Group[i] != alignment_table$Group[i + 1] && 
      alignment_table$Group[i + 1] != "") {
    group_boundaries <- c(group_boundaries, i)
  }
}

# Add grey lines at group boundaries
for (boundary in group_boundaries) {
  gt_table <- gt_table %>%
    tab_style(
      style = cell_borders(
        sides = "bottom",
        color = "gray50",
        weight = px(2)
      ),
      locations = cells_body(rows = boundary)
    )
}

# Save as PNG
gtsave(gt_table, "column_alignment_table.png")

