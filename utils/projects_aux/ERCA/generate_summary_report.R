# 20251205

# This code was used to create the summary reports needed for the ERCA project.
# This is the urgent request we receive every year.
# K. sensei proposes multiple datasets and we analyse patents and papers.
# This time, we used papers from OpenAlex to get the datadownload in bulk
# The link for the files is in the drive folder "urgent"
# https://drive.google.com/drive/folders/1wyyN51IfG3wyRcTwr1Af2Ryd9k31nxSV

# This code, reads from that folder and writes the outputs there.


# Load required libraries
library(tidyverse)
library(ggrepel)

# Function to process and plot summary data
process_summary <- function(origin_directory, data_type, dataset, summary_type) {
    # Construct file path
    file_path <- file.path(
        origin_directory, data_type, dataset,
        paste0("summary_", summary_type, ".csv")
    )

    # Check if file exists
    if (!file.exists(file_path)) {
        message(paste("File not found:", file_path))
        return(NULL)
    }

    # Read the CSV file
    data_full <- read_csv(file_path, show_col_types = FALSE)

    # Convert headers to lowercase
    names(data_full) <- tolower(names(data_full))

    # Identify the label column
    if (summary_type == "countries") {
        # For countries, use the 'country' column
        label_col <- "country"
    } else {
        # For institutions/firms, find column ending with "labels"
        label_col <- names(data_full)[grepl("label$", names(data_full))]

        if (length(label_col) == 0) {
            message(paste("No label column found in:", file_path))
            return(NULL)
        }
    }

    # Create color column
    if (summary_type == "countries") {
        data_full <- data_full %>%
            mutate(color = ifelse(grepl("^Japan", .data[[label_col]], ignore.case = TRUE),
                "red", "black"
            ))
    } else {
        data_full <- data_full %>%
            mutate(color = ifelse(grepl("^JP", .data[[label_col]]),
                "red", "black"
            ))
    }

    # Keep top 100 rows for plotting
    data <- data_full %>% slice(1:min(100, n()))

    # Determine the count column name
    count_col <- ifelse(data_type == "papers", "papers", "patents")

    # Check if required columns exist
    required_cols <- c("ave_year", "ave_citations", count_col, label_col)
    missing_cols <- setdiff(required_cols, names(data))

    if (length(missing_cols) > 0) {
        message(paste(
            "Missing columns in", file_path, ":",
            paste(missing_cols, collapse = ", ")
        ))
        return(NULL)
    }

    # Create the plot
    p <- ggplot(data, aes(x = ave_year, y = ave_citations)) +
        geom_point(aes(size = .data[[count_col]], color = color), alpha = 0.6) +
        geom_text_repel(aes(label = .data[[label_col]], color = color),
            size = 3, max.overlaps = 10
        ) +
        scale_color_identity() +
        scale_size_continuous(name = ifelse(data_type == "papers",
            "Number of Papers",
            "Number of Patents"
        )) +
        labs(
            title = paste(dataset, "-", data_type, "-", summary_type),
            x = "Average Year",
            y = "Average Citations"
        ) +
        theme_bw() +
        theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "bottom"
        )

    # Save the plot
    output_file <- file.path(
        origin_directory,
        paste0(dataset, "_", data_type, "_", summary_type, ".png")
    )

    ggsave(output_file, plot = p, width = 10, height = 8, dpi = 300)
    message(paste("Saved plot:", output_file))

    # Return summary statistics for countries only
    if (summary_type == "countries") {
        # Use full data (not just top 100) for total counts
        total_count <- sum(data_full[[count_col]], na.rm = TRUE)
        japan_count <- sum(data_full[[count_col]][data_full$color == "red"], na.rm = TRUE)

        return(list(
            dataset = dataset,
            data_type = data_type,
            total = total_count,
            japan = japan_count
        ))
    }

    return(NULL)
}

# Main function to process all datasets
generate_summary_report <- function(origin_directory, datasets) {
    # Initialize consolidated summary data frame
    consolidated <- data.frame(
        datasets = character(),
        papers = numeric(),
        patents = numeric(),
        Japan_papers = numeric(),
        Japan_patents = numeric(),
        Japan_papers_percent = numeric(),
        Japan_patents_percent = numeric(),
        stringsAsFactors = FALSE
    )

    # Loop through each dataset
    for (dataset in datasets) {
        message(paste("\nProcessing dataset:", dataset))

        # Initialize counters for this dataset
        papers_total <- 0
        patents_total <- 0
        japan_papers <- 0
        japan_patents <- 0

        # Process papers
        data_type <- "papers"

        # Get actual number of papers from dataset.csv
        dataset_file <- file.path(origin_directory, data_type, dataset, "dataset.csv")
        if (file.exists(dataset_file)) {
            dataset_data <- read_csv(dataset_file, show_col_types = FALSE)
            papers_total <- nrow(dataset_data)
        } else {
            message(paste("Dataset file not found:", dataset_file))
            papers_total <- 0
        }

        # Countries summary for papers
        result <- process_summary(origin_directory, data_type, dataset, "countries")
        if (!is.null(result)) {
            japan_papers <- result$japan
        }

        # Institutions summary for papers
        process_summary(origin_directory, data_type, dataset, "institutions")

        # Process patents
        data_type <- "patents"

        # Countries summary for patents
        result <- process_summary(origin_directory, data_type, dataset, "countries")
        if (!is.null(result)) {
            patents_total <- result$total
            japan_patents <- result$japan
        }

        # Firms summary for patents
        process_summary(origin_directory, data_type, dataset, "firms")

        # Calculate percentages
        japan_papers_pct <- ifelse(papers_total > 0,
            round(japan_papers / papers_total * 100, 2), 0
        )
        japan_patents_pct <- ifelse(patents_total > 0,
            round(japan_patents / patents_total * 100, 2), 0
        )

        # Add to consolidated summary
        consolidated <- rbind(consolidated, data.frame(
            datasets = dataset,
            papers = papers_total,
            patents = patents_total,
            Japan_papers = japan_papers,
            Japan_patents = japan_patents,
            Japan_papers_percent = japan_papers_pct,
            Japan_patents_percent = japan_patents_pct
        ))
    }

    # Save consolidated summary
    output_file <- file.path(origin_directory, "consolidated_summary.csv")
    write_csv(consolidated, output_file)
    message(paste("\nSaved consolidated summary:", output_file))

    # Print summary
    print(consolidated)

    return(consolidated)
}


#####################
origin_directory <- "/Users/cristian/Library/CloudStorage/GoogleDrive-cristianmejia00@gmail.com/My Drive/urgent"

datasets <- c(
    "battery_recycling", "biodiversity", "biodiversity_remote_sensing", "cofermentation_codigest",
    "desalination", "environmental_dna", "offshore_wind", "perovskite_solar_cell", "seaweed_restoration",
    "sewage_treatment", "soil", "surface_AND_ground_water"
)

result <- generate_summary_report(origin_directory, datasets)
