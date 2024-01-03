####################################################################################################
#
# This script extracts the data from the zip-file and saves it as a csv-file in the bld directory.
#
####################################################################################################

extract_data <- function(ce_pumd_file = "fmli231", target_columns = c("FINATXEM")){
    
    # set zip-file and create BLD directory
    zip_file_path <- file.path(getwd(),"data", "ce_pumd", "intrvw22.zip")

    BLD <- file.path(getwd(),"bld", "intrvw22_unzipped")

    if (!dir.exists(BLD)) {
    dir.create(BLD, recursive = TRUE)
    }

    # unzip data
    unzip(zip_file_path, exdir = BLD)

    data_file_path <- file.path(BLD,"intrvw22",paste0(ce_pumd_file,".csv"))

    # read data
    data <- tryCatch({
        read_csv(data_file_path, col_select = target_columns)
    }, error = function(e) {
        warning("Error reading data file: ", e$message)
        return(NULL)
    })

    if ("FINATXEM" %in% target_columns){
        data <- clean_FINAXTEM(data)
    }

    return(data)
}

clean_FINAXTEM <- function(df){

    finatxem_col <- sym("FINATXEM")

    cleaned_income <- df %>%
        filter(!is.na(!!finatxem_col)) %>%
        filter((!!finatxem_col) > 0) %>%
        rename(FamilyIncomeAfterTaxes = !!finatxem_col)
    
    return(cleaned_income)
}