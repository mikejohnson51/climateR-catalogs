library(targets)

# Loads climateR.catalogs
library(climateR.catalogs)

# future::plan(future::multicore)

# Follow 3xx redirects, prevents issues with cida.usgs.gov requests
Sys.setenv(CURLOPT_FOLLOWLOCATION = 1)

# Dynamically load data sources
lapply(
    list.files(path = here::here("sources/"),
               pattern = "ds-*",
               full.names = TRUE),
    FUN = source,
    local = FALSE
)

targets::tar_option_set(
    packages = c("climateR.catalogs", "terra", "sf", "rvest", "glue",
                 "dplyr", "jsonlite", "logger", "rvest", "climateR",
                 "RNetCDF", "data.table", "tidyr", "RCurl",
                 "arrow"),
    storage   = "worker",
    retrieval = "worker"
)

logger::log_threshold(logger::DEBUG)
logger::log_layout(logger::layout_glue_colors)

# Ensure private/ is created for outputs
if (!dir.exists("private")) {
    dir.create("private")
}

exclude <- c() # modify this to exclude data sources, i.e. c("ds_modis")

# -----------------------------------------------------------------------------
# Sub-pipeline for pulling catalog items --------------------------------------
# -----------------------------------------------------------------------------
mapped_workflow <- tarchetypes::tar_map(
    values = data.frame(src = Filter(
        function(x) !(x %in% exclude),
        ls(.GlobalEnv, pattern = "ds_*")
    )),
    names  = src,
    # Mapped Targets
    list(
        # Load Target
        targets::tar_target(
            load_,
            get(src, envir = .GlobalEnv),
            memory = "transient",
            deployment = "main"
        ),

        # Pull Target
        targets::tar_target(
            pull_,
            `$`(load_, pull)(),
            format = climateR.catalogs::tar_format_data_source,
            memory = "transient",
            deployment = "worker"
        ),

        # Tidy Target
        targets::tar_target(
            tidy_,
            `$`(pull_, tidy)(),
            format = climateR.catalogs::tar_format_data_source,
            memory = "transient",
            deployment = "worker"
        ),

        targets::tar_target(
            errors_,
            `$`(tidy_, errors),
            format = "rds",
            memory = "transient",
            deployment = "worker"
        ),

        # Result Target
        targets::tar_target(
            result_,
            `$`(tidy_, result),
            format = "feather",
            memory = "transient",
            deployment = "worker"
        )
    )
)
# -----------------------------------------------------------------------------
# Sub-pipeline for outputting catalog -----------------------------------------
# -----------------------------------------------------------------------------
outputs_workflow <- list(
    # Output Errors JSON
    targets::tar_target(
        errors_json,
        {
            jsonlite::write_json(
                errors,
                "private/errors.json",
                pretty = TRUE
            )
            "private/errors.json"
        },
        format = "file",
        deployment = "main"
    ),

    # Output JSON
    targets::tar_target(
        catalog_json,
        {
            jsonlite::stream_out(
                catalog$to_data_frame(),
                file("private/catalog.json")
            )
            "private/catalog.json"
        },
        format = "file",
        deployment = "main"
    ),

    # Output GZJSON
    targets::tar_target(
        catalog_gzjson,
        {
            jsonlite::stream_out(
                catalog$to_data_frame(),
                gzfile("private/catalog.json.gz")
            )
            "private/catalog.json.gz"
        },
        format = "file",
        deployment = "main"
    ),

    # Output Parquet
    targets::tar_target(
        catalog_parquet,
        {
            arrow::write_parquet(catalog, "private/catalog.parquet")
            "private/catalog.parquet"
        },
        format = "file",
        deployment = "main"
    )

    # # Output RDS
    # targets::tar_target(
    #     catalog_rds,
    #     {
    #         saveRDS(catalog$to_data_frame(), "private/catalog.rds")
    #         "private/catalog.rds"
    #     },
    #     format = "file"
    # )
)

# -----------------------------------------------------------------------------
# Pipeline --------------------------------------------------------------------
# -----------------------------------------------------------------------------
list(
    mapped_workflow,

    # Aggregate
    tarchetypes::tar_combine(
        catalog_initial,
        mapped_workflow[[length(mapped_workflow)]],
        command = dplyr::bind_rows(!!!.x),
        use_names = FALSE,
        format = climateR.catalogs::tar_format_arrow_ipc,
        memory = "transient",
        garbage_collection = TRUE,
        deployment = "main"
    ),

    tarchetypes::tar_combine(
        errors,
        mapped_workflow[[length(mapped_workflow) - 1]],
        command = dplyr::bind_rows(!!!.x),
        use_names = FALSE,
        format = "rds",
        memory = "transient",
        deployment = "main"
    ),

    # Fix Schema
    targets::tar_target(
        catalog,
        climateR.catalogs::rectify_schema(catalog_initial),
        format = climateR.catalogs::tar_format_arrow_ipc,
        deployment = "main"
    ),

    outputs_workflow
)
