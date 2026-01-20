# --------------------------------------------------
# FROM FILE: report01_RQuarto_00_copy_PROD_RUN.qmd
# --------------------------------------------------

# # # # # Section 01 - Libraries ---------------------------------------------
  library("stats")     # General Linear Models
  library("agricolae") # Tukey test
  library("plotly")    # Advanced graphical functions
  library("dplyr")     # Developing with %>%
  library("stringr")   # Strings replacement
  library("EnvStats")  # QQplot

# El internal
# # # # # Section 02 - Import dataset ----------------------------------------
my_dataset <- get("mtcars")
head(x = my_dataset, n = 5)

# # # # # Section 03 - Settings ----------------------------------------------
  var_name_rv     <- "mpg"
  var_name_rv

  var_name_factor <- "cyl"
  var_name_factor

  alpha_value     <- 0.05
  alpha_value

  vector_ordered_levels <- c("8", "4", "6")
  vector_ordered_levels
  
  vector_ordered_colors <- c("#FF0000", "#00FF00", "#0000FF")
  vector_ordered_colors
  


# # # # # Section 04 - Dataframe for alpha and confidence values ----------------------------
  confidence_value <- 1 - alpha_value
  
  df_alpha_confidence <- data.frame(
    "order" = 1:2,
    "detail" = c("alpha value", "confidence value"),
    "probability" = c(alpha_value, confidence_value),
    "percentaje" =  paste0(c(alpha_value, confidence_value)*100, "%")
  )
  df_alpha_confidence


# # # # # Section 05 - Selected variables and roles  -------------------------
  vector_all_var_names <- colnames(my_dataset)
  vector_name_selected_vars <- c(var_name_rv, var_name_factor)
  vector_rol_vars <- c("RV", "FACTOR")
  

  df_selected_vars <- data.frame(
    "order" = 1:length(vector_name_selected_vars),
    "var_name" = vector_name_selected_vars,
    "var_number" = match(vector_name_selected_vars, vector_all_var_names),
    "var_letter" = openxlsx::int2col(match(vector_name_selected_vars, vector_all_var_names)),
    "var_role" = vector_rol_vars,
    "doble_reference" = paste0(vector_rol_vars, "(", vector_name_selected_vars, ")")
  )
  df_selected_vars

# # # # # Section 06 - minidataset ------------------------------------------------
  # Only selected variabless. 
  # Only completed rows. 
  # Factor columns as factor object in R.
  minidataset <- na.omit(my_dataset[vector_name_selected_vars])
  #colnames(minidataset) <- vector_rol_vars
  minidataset[,var_name_factor] <- as.factor(minidataset[,var_name_factor])
  minidataset[,var_name_factor] <- factor(
  x = minidataset[,var_name_factor],       # La variable original de factor
  levels = vector_ordered_levels  # El orden de los niveles que calculamos en el Paso 2
)
  
  head(x = minidataset, n = 5)

  # # # # # Section 07 - Control on minidataset ------------------------------------------------

  # # # my_dataset and minidataset reps
  # Our 'n' is from minidataset
  df_show_n <- data.frame(
    "object" = c("my_dataset", "minidataset"),
    "n_col" = c(ncol(my_dataset), ncol(minidataset)),
    "n_row" = c(nrow(my_dataset), nrow(minidataset))
  )
  df_show_n

  
  # # # Factor info
  # Default order for levels its alphabetic order.
  df_factor_info <- data.frame(
    "order" = 1:nlevels(minidataset[,var_name_factor]),
    "level" = levels(minidataset[,var_name_factor]),
    "n" = as.vector(table(minidataset[,var_name_factor])),
    "min" = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], min),
    "max" = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], max),
    "mean" = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], mean),
    "color" = vector_ordered_colors
  )
  
  rownames(df_factor_info) <- 1:nrow(df_factor_info)
  df_factor_info
  

  # # # Anova control
  # 'VR' must be numeric and 'FACTOR must be factor.
  df_control_minidataset <- data.frame(
    "order" = 1:nrow(df_selected_vars),
    "var_name" = df_selected_vars$"var_name",
    "var_role" = df_selected_vars$"var_role",
    "control" = c("is.numeric()", "is.factor()"),
    "verify" = c(is.numeric(minidataset[,var_name_rv]), is.factor(minidataset[,var_name_factor]))
  )
  df_control_minidataset
  

  # # # # # Section 08 - Anova Test ----------------------------------------------
  # # # Anova test
  the_formula <- paste0(var_name_rv,  " ~ " , var_name_factor)
  the_formula <- as.formula (the_formula)
  list_lm_anova <- lm(formula = the_formula, data = minidataset)               # Linear model
  list_aov_anova <- aov(list_lm_anova)                                 # R results for anova
  df_table_anova <- as.data.frame(summary(list_aov_anova)[[1]])   # Common anova table
  df_table_anova
  


  df_table_anova_classroom <- df_table_anova
  new_row_anova <- nrow(df_table_anova_classroom)+ 1
  df_table_anova_classroom[new_row_anova, ] <- rep(NA, ncol(df_table_anova_classroom))
  df_table_anova_classroom$"Df"[new_row_anova] <- sum(na.omit(df_table_anova_classroom$"Df"))
  df_table_anova_classroom$"Sum Sq"[new_row_anova] <- sum(na.omit(df_table_anova_classroom$"Sum Sq"))
  df_table_anova_classroom$"Mean Sq"[new_row_anova] <- df_table_anova_classroom$"Sum Sq"[new_row_anova]/df_table_anova_classroom$"Df"[new_row_anova]
  df_table_anova_classroom <- cbind.data.frame(c("Factor", "Error", "Total"), df_table_anova_classroom)
  colnames(df_table_anova_classroom)[1]  <- "Sources of Variation"
  rownames(df_table_anova_classroom)[new_row_anova]  <- "Total"
  df_table_anova_classroom


  # # # Standard error from model for each level
  model_error_var_MSE <- df_table_anova$`Mean Sq`[2]
  model_error_sd <- sqrt(model_error_var_MSE)
  
  df_model_error <- data.frame(
    "order" = df_factor_info$order,
    "level" = df_factor_info$level,
    "n" = df_factor_info$n,
    "model_error_var_MSE" = model_error_var_MSE,
    "model_error_sd" = model_error_sd
  )
  df_model_error["model_error_se"] <- df_model_error["model_error_sd"]/sqrt(df_model_error$n)
  df_model_error
  
  
  


# # # # Section 09 - Tukey test

  # Step 01 - Check unbalanced repetitions on levels
  # # # Unbalanced reps for levels?
  # # # Important information for Tukey test.
  check_unbalanced_reps <- length(unique(df_factor_info$n)) > 1
  check_unbalanced_reps
  
  phrase_yes_unbalanced <- "The design is unbalanced in repetitions. A correction is applied to the Tukey test."
  phrase_no_unbalanced  <- "The design is not unbalanced in repetitions. A correction not be applied to the Tukey test."
  
  phrase_selected_check_unbalanced <- ifelse(test = check_unbalanced_reps, 
                                  yes = phrase_yes_unbalanced,
                                  no  = phrase_no_unbalanced)
  
  phrase_selected_check_unbalanced



  ##############################################################################
  tukey01_full_groups <- agricolae::HSD.test(y = list_lm_anova,
                                             trt = var_name_factor,
                                             alpha = alpha_value,
                                             group = TRUE,
                                             console = FALSE,
                                             unbalanced = check_unbalanced_reps)
  
  
  tukey01_full_groups


  # # # Tukey test - Tukey pairs comparation - Full version
  tukey02_full_pairs <- agricolae::HSD.test(y = list_lm_anova,
                                            trt = var_name_factor,
                                            alpha = alpha_value,
                                            group = FALSE,
                                            console = FALSE,
                                            unbalanced = check_unbalanced_reps)
  
  tukey02_full_pairs


  # # Original table from R about Tukey
  df_tukey_original_table <- tukey01_full_groups$groups
  df_tukey_original_table


  # # # New table about Tukey
  df_tukey_table_classroom <- data.frame(
    "order" = 1:nrow(tukey01_full_groups$groups),
    "level" = rownames(tukey01_full_groups$groups),
    "mean" = tukey01_full_groups$groups[,1],
    "group" = tukey01_full_groups$groups[,2]
  )
   df_tukey_table_classroom[,"level"] <- factor(
  x = df_tukey_table_classroom[,"level"],       # La variable original de factor
  levels = df_tukey_table_classroom[,"level"]  # El orden de los niveles que calculamos en el Paso 2
)
  df_tukey_table_classroom


  # # # # # Section 10 - minidataset_mod --------------------------------------------
  # The i number for each data
  vector_number_i_model <- as.numeric(minidataset[,var_name_factor])
  
  # The j number for each data
  vector_number_j_model <- ave(
    x = minidataset[,var_name_rv],            # Variable a contar (no importa cuál, solo para estructura)
    FUN = seq_along,            # Función que asigna 1, 2, 3...
    by = minidataset[,var_name_factor]            # Agrupado por el factor (cyl)
  )
  
  # Rows detection from dataset on minidataset
  dt_rows_my_dataset_ok <- rowSums(!is.na(my_dataset[vector_name_selected_vars])) == ncol(minidataset)
  
  
  # Creation: Dataframe minidataset_mod
  minidataset_mod <- minidataset
  minidataset_mod$"number_i_model" <- vector_number_i_model
  minidataset_mod$"number_j_model" <- vector_number_j_model
  minidataset_mod$"lvl_color"      <- vector_ordered_colors[minidataset_mod$"number_i_model"]
  minidataset_mod$"fitted.values"  <- list_lm_anova$"fitted.values"  
  minidataset_mod$"residuals"      <- list_lm_anova$residuals
  minidataset_mod$"studres"        <- minidataset_mod$"residuals"/model_error_sd
  minidataset_mod$"id_dataset"     <- c(1:nrow(my_dataset))[dt_rows_my_dataset_ok]
  minidataset_mod$"id_minidataset" <- 1:nrow(minidataset)

  
  # First 5 rows from minidataset_mod
  head(x = minidataset_mod, n = 5)

  
  
  
  # # # # Section 11 - Requeriment - Normality on residuals - Shapiro-Wilk test
  # # # Normality test (Shapiro-Wilk)
  list_test_residuals_normality <- shapiro.test(x = minidataset_mod$"residuals")
  list_test_residuals_normality

  

df_normality <- data.frame(
  "variable" = "residuals", 
  "test" = list_test_residuals_normality$method,
  "statistic"  = list_test_residuals_normality$statistic,
  "p_value" = list_test_residuals_normality$p.value
)
str_new_name_shapiro <- paste0("statistic", "(", names(list_test_residuals_normality$statistic), ")")
colnames(df_normality)[3] <- str_new_name_shapiro
rownames(df_normality) <- NULL
df_normality


  # # # # # Section 12 - Requeriment - Homogeneity of variance from residuals - Bartlett test 
  # # # Homogeinidy test (Bartlett)
  the_formula_bartlett <- paste0("residuals", " ~ ", var_name_factor)
  the_formula_bartlett <- as.formula(the_formula_bartlett)
  list_test_residuals_homogeneity <- bartlett.test(the_formula_bartlett, data = minidataset_mod)
  list_test_residuals_homogeneity
  



df_homogeneity <- data.frame(
  "variable" = "residuals", 
  "test" = list_test_residuals_homogeneity$method,
  "statistic"  = list_test_residuals_homogeneity$statistic,
  "p_value" = list_test_residuals_homogeneity$p.value
)
str_new_name_bartlett <- paste0("statistic", "(", names(list_test_residuals_homogeneity$statistic), ")")
colnames(df_homogeneity)[3] <- str_new_name_bartlett
rownames(df_homogeneity) <- NULL
df_homogeneity



  # # # Residuals variance from levels from original residuals
  df_raw_error <- data.frame(
    "order" = 1:nlevels(minidataset_mod[,var_name_factor]),
    "level" = levels(minidataset_mod[,var_name_factor]),
    "n" = tapply(minidataset_mod$"residuals", minidataset_mod[,var_name_factor], length),
    "raw_error_var" = tapply(minidataset_mod$"residuals", minidataset_mod[,var_name_factor], var),
    "raw_error_sd" = tapply(minidataset_mod$"residuals", minidataset_mod[,var_name_factor], sd)
  )
  df_model_error["raw_error_se"] <- df_model_error["model_error_sd"]/sqrt(df_model_error$"n")
  rownames(df_raw_error) <- 1:nrow(df_raw_error)
  df_raw_error
  
  phrase_info_errors <- "
Anova and Tukey use MSE from model with 'n-k' degree of freedom.
Bartlett use variance from raw error on each level with 'n_i' defree of freedom on each level.
Only if there is homogeneity from raw error variances then is a good idea take decision from MSE in Anova and Tukey."
  
  cat(phrase_info_errors)
  # # # Sum for residuals
  sum_residuals <- sum(minidataset_mod$"residuals")
  sum_residuals
  
  
  
  # # # Mean for residuals
  mean_residuals <- mean(minidataset_mod$"residuals")
  mean_residuals
  


# --------------------------------------------------
# FROM FILE: report00_RQuarto_99_appendix01_sec13_Descriptive_RV.qmd
# --------------------------------------------------

  # # # # # Section 10 - Partitioned Measures (VR)--------------------------------
  # # # Partitioned Measures of Position (VR)
  df_rv_position_levels <- data.frame(
    "order_level"  = 1:nlevels(minidataset[,var_name_factor]),
    "level" = levels(minidataset[,var_name_factor]),
    "n"            = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], length),
    "variable"     = rep(var_name_rv, nlevels(minidataset[,var_name_factor])),
    "min"          = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], min),
    "mean"         = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], mean),
    "Q1"           = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], quantile, 0.25),
    "median"       = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], median),
    "Q3"           = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], quantile, 0.75),
    "max"          = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], max),
    stringsAsFactors = FALSE
  )
  df_rv_position_levels[,"level"] <- factor(
    x = df_rv_position_levels[,"level"],       # La variable original de factor
    levels = df_rv_position_levels[,"level"]  # El orden de los niveles que calculamos en el Paso 2
  )
  rownames(df_rv_position_levels) <- NULL
  df_rv_position_levels


  # # # Partitioned Measures of Dispersion (VR)
  df_rv_dispersion_levels <- data.frame(
    "order_level"  = 1:nlevels(minidataset[,var_name_factor]),
    "level" = levels(minidataset[,var_name_factor]),
    "n"            = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], length),
    "variable"     = rep(var_name_rv, nlevels(minidataset[,var_name_factor])),
    "range"        = NA,
    "variance"     = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], var),
    "standard_deviation" = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], sd),
    "standard_error" = NA,
    "IQR" = NA,
    "vc" = NA,
    "pvc" = NA,
    stringsAsFactors = FALSE
  )
  df_rv_dispersion_levels$"range" <- df_rv_position_levels$"max" - df_rv_position_levels$"min"
  df_rv_dispersion_levels$"standard_error" <- df_rv_dispersion_levels$"standard_deviation"/sqrt(df_rv_dispersion_levels$"n")
  df_rv_dispersion_levels$"IQR" <- df_rv_position_levels$"Q3" - df_rv_position_levels$"Q1"
  df_rv_dispersion_levels$"cv"  <- df_rv_dispersion_levels$"standard_deviation" / df_rv_position_levels$"mean"
  df_rv_dispersion_levels$"pcv" <- paste0(df_rv_dispersion_levels$"cv"*100, "%")
  #   
  df_rv_dispersion_levels[,"level"] <- factor(
        x = df_rv_dispersion_levels[,"level"],       # La variable original de factor
        levels = df_rv_dispersion_levels[,"level"]  # El orden de los niveles que calculamos en el Paso 2
  )
  rownames(df_rv_dispersion_levels) <- NULL
  df_rv_dispersion_levels

  # # # General Measures of Position (VR)
  df_rv_position_general <- data.frame(
    "variable"  = var_name_rv,
    "n"         = length(minidataset[,var_name_rv]),
    "min"       = min(minidataset[,var_name_rv]),
    "mean"      = mean(minidataset[,var_name_rv]),
    "Q1"        = quantile(minidataset[,var_name_rv], 0.25),
    "median"    = median(minidataset[,var_name_rv]),
    "Q3"        = quantile(minidataset[,var_name_rv], 0.75),
    "max"       = max(minidataset[,var_name_rv]),
    stringsAsFactors = FALSE
  )
  rownames(df_rv_position_general) <- NULL

  df_rv_position_general

  # # # General Measures of Dispersion (VR)
  df_rv_dispersion_general <- data.frame(
    "variable"           = var_name_rv, 
    "n"                  = length(minidataset[,var_name_rv]),
    "range"              = NA,
    "variance"           = var(minidataset[,var_name_rv]),
    "standard_deviation" = sd(minidataset[,var_name_rv]),
    "standard_error"    = NA,
    "IQR"               = NA,
    "cv"                = NA,
    "pcv"               = NA,
    stringsAsFactors = FALSE
  )

  df_rv_dispersion_general$"range" <- df_rv_position_general$"max" - df_rv_position_general$"min"
  df_rv_dispersion_general$"standard_error" <- df_rv_dispersion_general$"standard_deviation"/sqrt(df_rv_dispersion_general$"n")
  df_rv_dispersion_general$"IQR" <- df_rv_position_general$"Q3" - df_rv_position_general$"Q1"
  df_rv_dispersion_general$"cv"  <- df_rv_dispersion_general$"standard_deviation" / df_rv_position_general$"mean"
  df_rv_dispersion_general$"pcv" <- paste0(df_rv_dispersion_general$"cv"*100, "%")

  rownames(df_rv_dispersion_general) <- NULL
  df_rv_dispersion_general


  
  # # # # # Section 13 - Special table to plots ----------------------------------
  
  # # # Table for plot001
  df_table_factor_plot001 <- data.frame(
    "order" = df_factor_info$order,
    "level" = df_factor_info$level,
    "n" = df_factor_info$n,
    "mean" = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], mean),
    "min" = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], min),
    "max" = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], max),
    "sd" = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], sd),
    "var" = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], var)
  )
  
  df_table_factor_plot002 <- data.frame(
    "order" = df_factor_info$order,
    "level" = df_factor_info$level,
    "n" = df_factor_info$n,
    "mean" = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], mean),
    "model_error_sd" = df_model_error$model_error_sd
  )
  df_table_factor_plot002["lower_limit"] <- df_table_factor_plot002$mean - df_table_factor_plot002$model_error_sd
  df_table_factor_plot002["upper_limmit"] <- df_table_factor_plot002$mean + df_table_factor_plot002$model_error_sd
  df_table_factor_plot002["color"] <- df_factor_info$color
df_table_factor_plot002[,"level"] <- factor(
  x = df_table_factor_plot002[,"level"],       # La variable original de factor
  levels = df_table_factor_plot002[,"level"]  # El orden de los niveles que calculamos en el Paso 2
)
  df_table_factor_plot002
  
  
  
  df_table_factor_plot003 <- data.frame(
    "order" = df_factor_info$order,
    "level" = df_factor_info$level,
    "n" = df_factor_info$n,
    "mean" = tapply(minidataset[,var_name_rv], minidataset[,var_name_factor], mean),
    "model_error_se" = df_model_error$model_error_se
  )
  df_table_factor_plot003["lower_limit"] <- df_table_factor_plot003$mean - df_table_factor_plot003$model_error_se
  df_table_factor_plot003["upper_limmit"] <- df_table_factor_plot003$mean + df_table_factor_plot003$model_error_se
  df_table_factor_plot003["color"] <- df_factor_info$color
  df_table_factor_plot003
  df_table_factor_plot003[,"level"] <- factor(
  x = df_table_factor_plot003[,"level"],       # La variable original de factor
  levels = df_table_factor_plot003[,"level"]  # El orden de los niveles que calculamos en el Paso 2
)
  
  
  # # # Table for plot004
  df_table_factor_plot004 <- df_rv_position_levels
  df_table_factor_plot004["color"] <- df_factor_info$color
  
  # # # Table for plot005
  df_table_factor_plot005 <- df_table_factor_plot004
  
  # # # Table for plot006
  df_table_factor_plot006 <- df_table_factor_plot004
  
  
  df_table_factor_plot007 <- df_table_factor_plot003
  correct_pos_letters <- order(df_tukey_table_classroom$level)
  vector_letters <- df_tukey_table_classroom$group[correct_pos_letters]
  df_table_factor_plot007["group"] <- vector_letters
  


  #############################################################
  # # # Create a new plot...
  plot001_factor <- plotly::plot_ly()
  
  # # # Plot001 - Scatter plot for VR and FACTOR on minidataset_mod *****************
  plot001_factor <- plotly::add_trace(p = plot001_factor,
                                      type = "scatter",
                                      mode = "markers",
                                      x = minidataset_mod[,var_name_factor],
                                      y = minidataset_mod[,var_name_rv],
                                      color = minidataset_mod[,var_name_factor],
                                      colors = df_factor_info$color,
                                      marker = list(size = 15, opacity = 0.7))
  
  # # # Title and settings...
  plot001_factor <-   plotly::layout(p = plot001_factor,
                                     title = "Plot 001 - Scatterplot",
                                     font = list(size = 20),
                                     margin = list(t = 100))
  
  
  # # # Without zerolines
  plot001_factor <-   plotly::layout(p = plot001_factor,
                                     xaxis = list(zeroline = FALSE),
                                     yaxis = list(zeroline = FALSE))
  
  
  # # # Plot output
  plot001_factor
  

  ##############################################################################
  
  # # # Create a new plot...
  plot002_factor <- plot_ly()
  
  
  # # # Adding errors...
  plot002_factor <-   add_trace(p = plot002_factor,
                                type = "scatter",
                                mode = "markers",
                                x = df_table_factor_plot002$level,
                                y = df_table_factor_plot002$mean,
                                color = df_table_factor_plot002$level,
                                colors = df_table_factor_plot002$color,
                                marker = list(symbol = "line-ew-open",
                                              size = 50,
                                              opacity = 1,
                                              line = list(width = 5)),
                                error_y = list(type = "data", array = df_table_factor_plot002$model_error_sd)
  )
  
  
  # # # Title and settings...
  plot002_factor <- plotly::layout(p = plot002_factor,
                                   title = "Plot 002 - Mean and model standard deviation",
                                   font = list(size = 20),
                                   margin = list(t = 100))
  
  # # # Without zerolines
  plot002_factor <-plotly::layout(p = plot002_factor,
                                  xaxis = list(zeroline = FALSE),
                                  yaxis = list(zeroline = FALSE))
  
  # # # Plot output
  plot002_factor
  

  
  
  # # # Create a new plot...
  plot003_factor <- plotly::plot_ly()
  
  
  # # # Adding errors...
  plot003_factor <-   plotly::add_trace(p = plot003_factor,
                                        type = "scatter",
                                        mode = "markers",
                                        x = df_table_factor_plot003$level,
                                        y = df_table_factor_plot003$mean,
                                        color = df_table_factor_plot003$level,
                                        colors = df_table_factor_plot003$color,
                                        marker = list(symbol = "line-ew-open",
                                                      size = 50,
                                                      opacity = 1,
                                                      line = list(width = 5)),
                                        error_y = list(type = "data", array = df_table_factor_plot003$model_error_se)
  )
  
  
  # # # Title and settings...
  plot003_factor <- plotly::layout(p = plot003_factor,
                                   title = "Plot 003 - Mean y model standard error",
                                   font = list(size = 20),
                                   margin = list(t = 100))
  
  # # # Without zerolines
  plot003_factor <-plotly::layout(p = plot003_factor,
                                  xaxis = list(zeroline = FALSE),
                                  yaxis = list(zeroline = FALSE))
  
  # # # Plot output
  plot003_factor
  

  
  
  # # # New plotly...
  plot004_factor <- plotly::plot_ly()
  
  # # # Boxplot and info...
  plot004_factor <- plotly::add_trace(p = plot004_factor,
                                      type = "box",
                                      x = df_table_factor_plot004$level ,
                                      color = df_table_factor_plot004$level,
                                      colors = df_table_factor_plot004$color,
                                      lowerfence = df_table_factor_plot004$min,
                                      q1 = df_table_factor_plot004$Q1,
                                      median = df_table_factor_plot004$median,
                                      q3 = df_table_factor_plot004$Q3,
                                      upperfence = df_table_factor_plot004$max,
                                      boxmean = TRUE,
                                      boxpoints = FALSE,
                                      line = list(color = "black", width = 3)
  )
  
  # # # Title and settings...
  plot004_factor <- plotly::layout(p = plot004_factor,
                                   title = "Plot 004 - Boxplot and means",
                                   font = list(size = 20),
                                   margin = list(t = 100))
  
  
  # # # Without zerolines...
  plot004_factor <- plotly::layout(p = plot004_factor,
                                   xaxis = list(zeroline = FALSE),
                                   yaxis = list(zeroline = FALSE))
  
  # # # Output plot004_anova...
  plot004_factor
  
  ##############################################################################
  
  all_levels <- vector_ordered_levels
  n_levels <- length(all_levels)
  all_color <- vector_ordered_colors
  
  
  
  plot005_factor <- plot_ly()
  
  # Violinplot
  for (k in 1:n_levels){
    
    # Selected values
    selected_level <- all_levels[k]
    selected_color <- all_color[k]
    dt_filas <- minidataset_mod[,var_name_factor] == selected_level
    
    # Plotting selected violinplot
    plot005_factor <- plot005_factor %>%
      add_trace(x = minidataset_mod[,var_name_factor][dt_filas],
                y = minidataset_mod[,var_name_rv][dt_filas],
                type = "violin",
                name = paste0("violin", k),
                points = "all",
                marker = list(color = selected_color),
                line = list(color = selected_color),
                fillcolor = I(selected_color)
                
      )
    
    
  }
  
  
  
  
  # Boxplot
  plot005_factor <- plotly::add_trace(p = plot005_factor,
                                      type = "box",
                                      name = "boxplot",
                                      x = df_table_factor_plot005$level ,
                                      color = df_table_factor_plot005$level ,
                                      colors = df_table_factor_plot005$color,
                                      lowerfence = df_table_factor_plot005$min,
                                      q1 = df_table_factor_plot005$Q1,
                                      median = df_table_factor_plot005$median,
                                      q3 = df_table_factor_plot005$Q3,
                                      upperfence = df_table_factor_plot005$max,
                                      boxmean = TRUE,
                                      boxpoints = TRUE,
                                      fillcolor = df_table_factor_plot005$color,
                                      line = list(color = "black", width = 3),
                                      opacity = 0.5,
                                      width = 0.2)
  
  
  # # # Title and settings...
  plot005_factor <- plotly::layout(p = plot005_factor,
                                   title = "Plot 005 - Violinplot",
                                   font = list(size = 20),
                                   margin = list(t = 100))
  
  
  # # # Without zerolines...
  plot005_factor <- plotly::layout(p = plot005_factor,
                                   xaxis = list(zeroline = FALSE),
                                   yaxis = list(zeroline = FALSE))
  
  # # # Output plot003_anova...
  plot005_factor
  

  
  
  # #library(plotly)
  all_levels <- vector_ordered_levels
  n_levels <- length(all_levels)
  all_color <- vector_ordered_colors
  
  
  
  plot006_factor <- plot_ly()
  
  # Violinplot
  for (k in 1:n_levels){
    
    # Selected values
    selected_level <- all_levels[k]
    selected_color <- all_color[k]
    dt_filas <- minidataset_mod[,var_name_factor] == selected_level
    
    # Plotting selected violinplot
    plot006_factor <- plot006_factor %>%
      add_trace(y = minidataset_mod[,var_name_factor][dt_filas],
                x = minidataset_mod[,var_name_rv][dt_filas],
                type = "violin",
                orientation = 'h',
                name = paste0("violin", k),
                side = "positive",
                points = "all",
                marker = list(color = selected_color),
                line = list(color = selected_color),
                fillcolor = I(selected_color)
                
      )
    
    
  }
  plot006_factor
  # plot006_factor <- plotly::plot_ly()
  # 
  # # Add traces
  # plot006_factor <- plotly::add_trace(p = plot006_factor,
  #                                     type = "violin",
  #                                     y = minidataset_mod[,var_name_rv],
  #                                     x = minidataset_mod[,var_name_factor],
  #                                     showlegend = TRUE,
  #                                     side = "positive",
  #                                     points = "all",
  #                                     name = "Violinplot",
  #                                     color = minidataset_mod$FACTOR,
  #                                     colors = df_table_factor_plot006$color)
  # 
  # 
  # 
  # # # # Title and settings...
  # plot006_factor <- plotly::layout(p = plot006_factor,
  #                                  title = "Plot 006 - Scatterplot + Jitter +  Smoothed",
  #                                  font = list(size = 20),
  #                                  margin = list(t = 100))
  # 
  # 
  # # # # Without zerolines...
  # plot006_factor <- plotly::layout(p = plot006_factor,
  #                                  xaxis = list(zeroline = FALSE),
  #                                  yaxis = list(zeroline = FALSE))
  # 
  # # # # Output plot003_anova...
  # plot006_factor
  

  
  # # # Create a new plot...
  plot007_factor <- plotly::plot_ly()
  
  
  # # # Adding errors...
  plot007_factor <-   plotly::add_trace(p = plot007_factor,
                                        type = "scatter",
                                        mode = "markers",
                                        x = df_table_factor_plot007$level,
                                        y = df_table_factor_plot007$mean,
                                        color = df_table_factor_plot007$level,
                                        colors = df_table_factor_plot007$color,
                                        marker = list(symbol = "line-ew-open",
                                                      size = 50,
                                                      opacity = 1,
                                                      line = list(width = 5)),
                                        error_y = list(type = "data", array = df_table_factor_plot007$model_error_se)
  )
  
  
  
  plot007_factor <-  add_text(p = plot007_factor,
                              x = df_table_factor_plot007$level,
                              y = df_table_factor_plot007$mean,
                              text = df_table_factor_plot007$group, name = "Tukey Group",
                              size = 20)
  
  # # # Title and settings...
  plot007_factor <- plotly::layout(p = plot007_factor,
                                   title = "Plot 007 - Mean y model standard error",
                                   font = list(size = 20),
                                   margin = list(t = 100))
  
  # # # Without zerolines
  plot007_factor <-plotly::layout(p = plot007_factor,
                                  xaxis = list(zeroline = FALSE),
                                  yaxis = list(zeroline = FALSE))
  
  
  # # # Plot output
  plot007_factor
  


# --------------------------------------------------
# FROM FILE: report00_RQuarto_99_appendix02_sec14_Descriptive_Residuals.qmd
# --------------------------------------------------


  # # # # # Section 11 - Partitioned Measures (Residuals)-------------------------
  # # # Partitioned Measures of Position (residuals)
  df_residuals_position_levels <- data.frame(
    "order_level"  = 1:nlevels(minidataset_mod[,var_name_factor]),
    "level" = levels(minidataset_mod[,var_name_factor]),
    "n"            = tapply(minidataset_mod[,var_name_rv], minidataset_mod[,var_name_factor], length),
    "variable"     = rep("residuals", nlevels(minidataset_mod[,var_name_factor])),
    "min"          = tapply(minidataset_mod$"residuals", minidataset_mod[,var_name_factor], min),
    "mean"         = tapply(minidataset_mod$"residuals", minidataset_mod[,var_name_factor], mean),
    "Q1"           = tapply(minidataset_mod$"residuals", minidataset_mod[,var_name_factor], quantile, 0.25),
    "median"       = tapply(minidataset_mod$"residuals", minidataset_mod[,var_name_factor], median),
    "Q3"           = tapply(minidataset_mod$"residuals", minidataset_mod[,var_name_factor], quantile, 0.75),
    "max"          = tapply(minidataset_mod$"residuals", minidataset_mod[,var_name_factor], max),
    stringsAsFactors = FALSE
  )
    df_residuals_position_levels[,"level"] <- factor(
               x = df_residuals_position_levels[,"level"],       # La variable original de factor
          levels = df_residuals_position_levels[,"level"]  # El orden de los niveles que calculamos en el Paso 2
  )
    
  rownames(df_residuals_position_levels) <- NULL

  df_residuals_position_levels


  # # # Partitioned Measures of Dispersion (residuals)
  df_residual_dispersion_levels <- data.frame(
    "order_level"  = 1:nlevels(minidataset_mod[,var_name_factor]),
    "level" = levels(minidataset_mod[,var_name_factor]),
    "n"            = tapply(minidataset_mod[,"residuals"], minidataset_mod[,var_name_factor], length),
    "variable"     = rep("residuals", nlevels(minidataset[,var_name_factor])),
    "range"        = NA,
    "variance"     = tapply(minidataset_mod[,"residuals"], minidataset_mod[,var_name_factor], var),
    "standard_deviation" = tapply(minidataset_mod[,"residuals"], minidataset_mod[,var_name_factor], sd),
    "standard_error" = NA,
    "IQR" = NA,
    "vc" = NA,
    "pvc" = NA,
    stringsAsFactors = FALSE
  )
  df_residual_dispersion_levels$"range" <- df_residuals_position_levels$"max" - df_residuals_position_levels$"min"
  df_residual_dispersion_levels$"standard_error" <- df_residual_dispersion_levels$"standard_deviation"/sqrt(df_residual_dispersion_levels$"n")
  df_residual_dispersion_levels$"IQR" <- df_residuals_position_levels$"Q3" - df_residuals_position_levels$"Q1"
  df_residual_dispersion_levels$"cv"  <- df_rv_dispersion_levels$"standard_deviation" / df_residuals_position_levels$"mean"
  df_residual_dispersion_levels$"pcv" <- paste0(df_residual_dispersion_levels$"cv"*100, "%")
  #   
  df_residual_dispersion_levels[,"level"] <- factor(
        x = df_residual_dispersion_levels[,"level"],       # La variable original de factor
        levels = df_residual_dispersion_levels[,"level"]  # El orden de los niveles que calculamos en el Paso 2
  )
  rownames(df_residual_dispersion_levels) <- NULL
  df_residual_dispersion_levels

  # # # General Measures of Position (residuals)
  df_residuals_position_general <- data.frame(
    "variable"  = "residuals",
    "n"         = length(minidataset_mod$"residuals"),
    "min" = min(minidataset_mod$"residuals"),
    "mean" = mean(minidataset_mod$"residuals"),
    "Q1"        = quantile(minidataset_mod$"residuals", 0.25),
    "median" = median(minidataset_mod$"residuals"),
    "Q3"        = quantile(minidataset_mod$"residuals", 0.75),
    "max" = max(minidataset_mod$"residuals"),
    stringsAsFactors = FALSE
  )
  df_residuals_position_general

  # # # General Measures of Dispersion (residuals)
  df_residuals_dispersion_general <- data.frame(
    "variable"           = var_name_rv, 
    "n"                  = length(minidataset[,var_name_rv]),
    "range"              = NA,
    "variance"           = var(minidataset[,var_name_rv]),
    "standard_deviation" = sd(minidataset[,var_name_rv]),
    "standard_error"    = NA,
    "IQR"               = NA,
    "cv"                = NA,
    "pcv"               = NA,
    stringsAsFactors = FALSE
  )

  df_residuals_dispersion_general$"range" <- df_residuals_position_general$"max" - df_residuals_position_general$"min"
  df_residuals_dispersion_general$"standard_error" <- df_residuals_dispersion_general$"standard_deviation"/sqrt(df_residuals_dispersion_general$"n")
  df_residuals_dispersion_general$"IQR" <- df_residuals_position_general$"Q3" - df_residuals_position_general$"Q1"
  df_residuals_dispersion_general$"cv"  <- df_residuals_dispersion_general$"standard_deviation" / df_residuals_position_general$"mean"
  df_residuals_dispersion_general$"pcv" <- paste0(df_residuals_dispersion_general$"cv"*100, "%")

  rownames(df_residuals_dispersion_general) <- NULL
  
  df_residuals_dispersion_general
  


  
  
  # # # Table for plot006
  df_table_residuals_plot001 <- data.frame(
    "order" = 1:nlevels(minidataset_mod[,var_name_factor]),
    "level" = levels(minidataset_mod[,var_name_factor]),
    "n" = tapply(minidataset_mod$"residuals", minidataset_mod[,var_name_factor], length),
    "min" = tapply(minidataset_mod$"residuals", minidataset_mod[,var_name_factor], min),
    "mean" = tapply(minidataset_mod$"residuals", minidataset_mod[,var_name_factor], mean),
    "max" = tapply(minidataset_mod$"residuals", minidataset_mod[,var_name_factor], max),
    "var" = tapply(minidataset_mod$"residuals", minidataset_mod[,var_name_factor], var),
    "sd" = tapply(minidataset_mod$"residuals", minidataset_mod[,var_name_factor], sd),
    "color" = df_factor_info$color
  )
  df_table_residuals_plot001[,"level"] <- factor(
  x = df_table_residuals_plot001[,"level"],       # La variable original de factor
  levels = df_table_residuals_plot001[,"level"]  # El orden de los niveles que calculamos en el Paso 2
)
  df_table_residuals_plot001
  
  # # # Table for plot006
  df_table_residuals_plot002 <- df_table_residuals_plot001
  
  # # # Table for plot006
  df_table_residuals_plot003 <- df_table_residuals_plot001
  
  # # # Table for plot006
  df_table_residuals_plot004 <- data.frame(
    "variable" = "residuals",
    "n" = length(minidataset_mod$"residuals"),
    "min" = min(minidataset_mod$"residuals"),
    "mean" = mean(minidataset_mod$"residuals"),
    "max" = max(minidataset_mod$"residuals"),
    "var" = var(minidataset_mod$"residuals"),
    "sd" = sd(minidataset_mod$"residuals"),
    "model_error_var_MSE" = model_error_var_MSE,
    "model_error_sd" = model_error_sd
  )
  
  phrase_coment_errors <- "Model Error (MSE) "
  
  # # # Table for plot006
  df_table_residuals_plot005  <- df_table_residuals_plot004
  
  # # # Table for plot006
  df_table_residuals_plot006 <- data.frame(
    "order" = 1:nlevels(minidataset_mod[,var_name_factor]),
    "level" = levels(minidataset_mod[,var_name_factor]),
    "n" = tapply(minidataset_mod$studres, minidataset_mod[,var_name_factor], length),
    "min" = tapply(minidataset_mod$studres, minidataset_mod[,var_name_factor], min),
    "mean" = tapply(minidataset_mod$studres, minidataset_mod[,var_name_factor], mean),
    "max" = tapply(minidataset_mod$studres, minidataset_mod[,var_name_factor], max),
    "var" = tapply(minidataset_mod$studres, minidataset_mod[,var_name_factor], var),
    "sd" = tapply(minidataset_mod$studres, minidataset_mod[,var_name_factor], sd),
    "color" = df_factor_info$color
  )
   df_table_residuals_plot006[,"level"] <- factor(
  x = df_table_residuals_plot006[,"level"],       # La variable original de factor
  levels = df_table_residuals_plot006[,"level"]  # El orden de los niveles que calculamos en el Paso 2
)
  
  # # # Table for plot006
  df_table_residuals_plot007 <- df_table_residuals_plot006
  
  
  df_table_residuals_plot008 <- data.frame(
    "variable" = "studres",
    "n" = length(minidataset_mod$studres),
    "min" = min(minidataset_mod$studres),
    "mean" = mean(minidataset_mod$studres),
    "max" = max(minidataset_mod$studres),
    "var" = var(minidataset_mod$studres),
    "sd" = sd(minidataset_mod$studres)
  )
  
  
  df_table_residuals_plot009 <- df_table_residuals_plot008
  
  df_table_residuals_plot010 <- df_table_residuals_plot008
  
  #############################################################

  
  ####### DESDE ACAAAAAAAAAAAAAAAAAAAA
  # # # Create a new plot...
  plot001_residuals <- plotly::plot_ly()
  
  # # # Plot001 - Scatter plot for VR and FACTOR on minidataset_mod *****************
  plot001_residuals <- plotly::add_trace(p = plot001_residuals,
                                         type = "scatter",
                                         mode = "markers",
                                         x = minidataset_mod$FACTOR,
                                         y = minidataset_mod$"residuals",
                                         color = minidataset_mod$FACTOR,
                                         colors = df_factor_info$color,
                                         marker = list(size = 15, opacity = 0.7))
  
  # # # Title and settings...
  plot001_residuals <-   plotly::layout(p = plot001_residuals,
                                        title = "Plot 001 - Scatterplot - Residuals",
                                        font = list(size = 20),
                                        margin = list(t = 100))
  
  
  # # # Without zerolines
  plot001_residuals <-   plotly::layout(p = plot001_residuals,
                                        xaxis = list(zeroline = FALSE),
                                        yaxis = list(zeroline = TRUE))
  
  
  # # # Plot output
  plot001_residuals
  
  
  

  
  #library(plotly)
  plot002_residuals <- plotly::plot_ly()
  
  # Add traces
  plot002_residuals <- plotly::add_trace(p = plot002_residuals,
                                         type = "violin",
                                         y = minidataset_mod$"residuals",
                                         x = minidataset_mod$FACTOR,
                                         showlegend = TRUE,
                                         side = "positive",
                                         points = "all",
                                         name = "Violinplot",
                                         color = minidataset_mod$FACTOR,
                                         colors = df_table_residuals_plot002$color)
  
  
  
  # # # Title and settings...
  plot002_residuals <- plotly::layout(p = plot002_residuals,
                                      title = "Plot 002 - Residuals - Scatterplot + Jitter +  Smoothed",
                                      font = list(size = 20),
                                      margin = list(t = 100))
  
  
  # # # Without zerolines...
  plot002_residuals <- plotly::layout(p = plot002_residuals,
                                      xaxis = list(zeroline = FALSE),
                                      yaxis = list(zeroline = FALSE))
  
  # # # Output plot003_anova...
  plot002_residuals
  
  

  
  
  
  plot003_residuals <- plotly::plot_ly()
  
  # Add traces
  plot003_residuals <- plotly::add_trace(p = plot003_residuals,
                                         type = "violin",
                                         x = minidataset_mod$"residuals",
                                         showlegend = TRUE,
                                         side = "positive",
                                         points = FALSE,
                                         #name = levels(minidataset_mod$FACTOR)[minidataset_mod$lvl_order_number],
                                         color = minidataset_mod$FACTOR,
                                         colors = df_table_residuals_plot003$color)
  
  
  
  # # # Title and settings...
  plot003_residuals <- plotly::layout(p = plot003_residuals,
                                      title = "Plot 003 - Residuals - Scatterplot + Jitter +  Smoothed",
                                      font = list(size = 20),
                                      margin = list(t = 100))
  
  
  # # # Without zerolines...
  plot003residuals <- plotly::layout(p = plot003_residuals,
                                     xaxis = list(zeroline = FALSE),
                                     yaxis = list(zeroline = FALSE))
  
  # # # Output plot003_anova...
  plot003_residuals
  

  
  
  
  plot004_residuals <- plotly::plot_ly()
  
  # Add traces
  plot004_residuals <- plotly::add_trace(p = plot004_residuals,
                                         type = "violin",
                                         x = minidataset_mod$"residuals",
                                         #x = minidataset_mod$FACTOR,
                                         showlegend = TRUE,
                                         side = "positive",
                                         points = "all",
                                         name = " ")#
  #color = minidataset_mod$FACTOR,
  #colors = df_table_factor_plot006$color)
  
  
  
  # # # Title and settings...
  plot004_residuals <- plotly::layout(p = plot004_residuals,
                                      title = "Plot 004 - Residuals",
                                      font = list(size = 20),
                                      margin = list(t = 100))
  
  
  # # # Without zerolines...
  plot004_residuals <- plotly::layout(p = plot004_residuals,
                                      xaxis = list(zeroline = TRUE),
                                      yaxis = list(zeroline = FALSE))
  
  # # # Output plot003_anova...
  plot004_residuals
  
  

  
  # - el 5
  qq_info <- EnvStats::qqPlot(x = minidataset_mod$"residuals", plot.it = F,
                              param.list = list(mean = mean(minidataset_mod$"residuals"),
                                                sd = sd(minidataset_mod$"residuals")))
  
  cuantiles_teoricos <- qq_info$x
  cuantiles_observados <- qq_info$y
  
  #library(plotly)
  plot005_residuals <- plotly::plot_ly()
  
  # Crear el gráfico QQ plot
  plot005_residuals <-add_trace(p = plot005_residuals,
                                x = cuantiles_teoricos,
                                y = cuantiles_observados,
                                type = 'scatter', mode = 'markers',
                                marker = list(color = 'blue'),
                                name = "points")
  
  # Agregar la línea de identidad
  pendiente <- 1
  intercepto <- 0
  
  # Calcular las coordenadas de los extremos de la línea de identidad
  x_extremos <- range(cuantiles_teoricos)
  y_extremos <- pendiente * x_extremos + intercepto
  
  # Agregar la recta de identidad
  plot005_residuals <- add_trace(p = plot005_residuals,
                                 x = x_extremos,
                                 y = y_extremos,
                                 type = 'scatter',
                                 mode = 'lines',
                                 line = list(color = 'red'),
                                 name = "identity")
  
  
  # Establecer etiquetas de los ejes
  # plot007_residuals <- layout(p = plot007_residuals,
  #                             xaxis = list(title = 'Expected quantiles'),
  #                             yaxis = list(title = 'Observed quantiles'))
  
  plot005_residuals <- plotly::layout(p = plot005_residuals,
                                      title = "Plot 005 - QQ Plot Residuals",
                                      font = list(size = 20),
                                      margin = list(t = 100))
  
  # Mostrar el gráfico
  plot005_residuals
  # - Fin el 5
  

  
  # # # Create a new plot...
  plot006_residuals <- plotly::plot_ly()
  
  # # # Plot001 - Scatter plot for VR and FACTOR on minidataset_mod *****************
  plot006_residuals <- plotly::add_trace(p = plot006_residuals,
                                         type = "scatter",
                                         mode = "markers",
                                         x = minidataset_mod$fitted.values,
                                         y = minidataset_mod$"residuals",
                                         color = minidataset_mod$FACTOR,
                                         colors = df_factor_info$color,
                                         marker = list(size = 15, opacity = 0.7))
  
  # # # Title and settings...
  plot006_residuals <-   plotly::layout(p = plot006_residuals,
                                        title = "Plot 006 - Scatterplot - Residuals vs Fitted.values",
                                        font = list(size = 20),
                                        margin = list(t = 100))
  
  
  # # # Without zerolines
  plot006_residuals <-   plotly::layout(p = plot006_residuals,
                                        xaxis = list(zeroline = FALSE),
                                        yaxis = list(zeroline = TRUE))
  
  
  # # # Plot output
  plot006_residuals
  
  

  
  # # # Create a new plot...
  plot007_residuals <- plotly::plot_ly()
  
  # # # Plot001 - Scatter plot for VR and FACTOR on minidataset_mod *****************
  plot007_residuals <- plotly::add_trace(p = plot007_residuals,
                                         type = "scatter",
                                         mode = "markers",
                                         x = minidataset_mod$FACTOR,
                                         y = minidataset_mod$studres,
                                         color = minidataset_mod$FACTOR,
                                         colors = df_factor_info$color,
                                         marker = list(size = 15, opacity = 0.7))
  
  # # # Title and settings...
  plot007_residuals <-   plotly::layout(p = plot007_residuals,
                                        title = "Plot 007 - Scatterplot - Studentized Residuals",
                                        font = list(size = 20),
                                        margin = list(t = 100))
  
  
  # # # Without zerolines
  plot007_residuals <-   plotly::layout(p = plot007_residuals,
                                        xaxis = list(zeroline = FALSE),
                                        yaxis = list(zeroline = TRUE))
  
  
  # # # Plot output
  plot007_residuals
  
  
  

  
  
  
  #library(plotly)
  plot008_residuals <- plotly::plot_ly()
  
  # Add traces
  plot008_residuals <- plotly::add_trace(p = plot008_residuals,
                                         type = "violin",
                                         x = minidataset_mod$studres,
                                         #x = minidataset_mod$FACTOR,
                                         showlegend = TRUE,
                                         side = "positive",
                                         points = "all",
                                         name = " ")#
  #color = minidataset_mod$FACTOR,
  #colors = df_table_factor_plot006$color)
  
  
  
  # # # Title and settings...
  plot008_residuals <- plotly::layout(p = plot008_residuals,
                                      title = "Plot 008 - Studentized Residuals",
                                      font = list(size = 20),
                                      margin = list(t = 100))
  
  
  # # # Without zerolines...
  plot008_residuals <- plotly::layout(p = plot008_residuals,
                                      xaxis = list(zeroline = TRUE),
                                      yaxis = list(zeroline = FALSE))
  
  # # # Output plot003_anova...
  plot008_residuals
  
  

  
  # el 9
  
  x <- seq(-4, 4, length.out = 100)
  y <- dnorm(x, mean = 0, 1)
  #  x <- x*model_error_sd
  densidad_suavizada <- density(x, kernel = "gaussian", adjust = 0.5)
  hist_data_studres <- hist(minidataset_mod$studres, plot = FALSE)
  hist_data_studres$"rel_frec" <- hist_data_studres$counts/sum(hist_data_studres$counts)
  
  densidad_studres <-  density(x = minidataset_mod$studres, kernel = "gaussian", adjust =0.5)
  
  #library(plotly)
  plot009_residuals <- plotly::plot_ly()
  
  
  # plot005_residuals <- add_trace(p = plot005_residuals,
  #                                x = densidad_studres$x,
  #                                y = densidad_studres$y,
  #                                type = 'scatter',
  #                                mode = 'lines',
  #                                name = 'densidad_studres')
  
  plot009_residuals <- add_trace(p = plot009_residuals,
                                 x = x,
                                 y = y,
                                 type = 'scatter',
                                 mode = 'lines',
                                 name = 'Normal Standard')
  
  
  
  
  
  # # Add traces
  # plot005_residuals <- plotly::add_trace(p = plot005_residuals,
  #                                        type = "violin",
  #                                        x = minidataset_mod$"residuals",
  #                                        #x = minidataset_mod$FACTOR,
  #                                        showlegend = TRUE,
  #                                        side = "positive",
  #                                        points = FALSE,
  #                                        name = "violinplot")#
  
  plot009_residuals <- plotly::add_trace(p = plot009_residuals,
                                         type = "bar",
                                         x = hist_data_studres$"mids",
                                         y = hist_data_studres$"density",
                                         name = "hist - studres")
  
  plot009_residuals <- plotly::layout(p = plot009_residuals,
                                      bargap = 0)
  
  plot009_residuals <- plotly::layout(p = plot009_residuals,
                                      title = "Plot 009 - Studres Distribution",
                                      font = list(size = 20),
                                      margin = list(t = 100))
  
  plot009_residuals
  # fin el 9
  

  
  
  qq_info <- EnvStats::qqPlot(x = minidataset_mod$studres, plot.it = F,
                              param.list = list(mean = 0,
                                                sd = 1))
  
  cuantiles_teoricos <- qq_info$x
  cuantiles_observados <- qq_info$y
  
  #library(plotly)
  plot010_residuals <- plotly::plot_ly()
  
  # Crear el gráfico QQ plot
  plot010_residuals <-add_trace(p = plot010_residuals,
                                x = cuantiles_teoricos,
                                y = cuantiles_observados,
                                type = 'scatter', mode = 'markers',
                                marker = list(color = 'blue'),
                                name = "points")
  
  # Agregar la línea de identidad
  pendiente <- 1
  intercepto <- 0
  
  # Calcular las coordenadas de los extremos de la línea de identidad
  x_extremos <- range(cuantiles_teoricos)
  y_extremos <- pendiente * x_extremos + intercepto
  
  # Agregar la recta de identidad
  plot010_residuals <- add_trace(p = plot010_residuals,
                                 x = x_extremos,
                                 y = y_extremos,
                                 type = 'scatter',
                                 mode = 'lines',
                                 line = list(color = 'red'),
                                 name = "identity")
  
  
  # Establecer etiquetas de los ejes
  # plot007_residuals <- layout(p = plot007_residuals,
  #                             xaxis = list(title = 'Expected quantiles'),
  #                             yaxis = list(title = 'Observed quantiles'))
  
  plot010_residuals <- plotly::layout(p = plot010_residuals,
                                      title = "Plot 010 - QQ Plot - studres",
                                      font = list(size = 20),
                                      margin = list(t = 100))
  
  # Mostrar el gráfico
  plot010_residuals
  


# --------------------------------------------------
# FROM FILE: report00_RQuarto_99_appendix03_sec15_to_17_ASA.qmd
# --------------------------------------------------


  # From Shapiro test ----------------------------------------------------------
  p_value_normality_internal <- list_test_residuals_normality$"p.value"
  
  check_normality_ho_rejected  <- p_value_normality_internal < alpha_value
  status_normality_ho   <- ifelse(test = check_normality_ho_rejected, 
                                   yes = "Ho Rejected", 
                                    no = "Ho no rejected")
  
  AQ_normality_rejected_ho <- ifelse(test = check_normality_ho_rejected, 
                                      yes = "Yes", 
                                       no = "No")
    
  AQ_normality_is_valid <- "Yes, it is."

  p_value_normality_external <- ifelse(test = p_value_normality_internal < 0.01, 
                                        yes = "<<0.01", 
                                         no = as.character(p_value_normality_internal))
  # From bartlett test ---------------------------------------------------------
  p_value_homogeneity_internal  <- list_test_residuals_homogeneity$"p.value"
  check_homogeneity_ho_rejected  <- p_value_homogeneity_internal < alpha_value
  status_homogeneity_ho <- ifelse(test = check_homogeneity_ho_rejected, 
                                   yes = "Ho Rejected", 
                                    no = "Ho no rejected")
  
  AQ_homogeneity_rejected_ho <- ifelse(test = check_homogeneity_ho_rejected, 
                                        yes = "Yes", 
                                         no = "No")
    
  AQ_homogeneity_is_valid <- "Yes, it is."

  p_value_homogeneity_external <- ifelse(test = p_value_normality_external < 0.01, 
                                          yes = "<<0.01", 
                                           no = as.character(p_value_normality_external))
  
  # Check for all requeriments -------------------------------------------------
  check_ok_all_requeriments     <- sum(check_normality_ho_rejected, check_homogeneity_ho_rejected) == 0
  AQ_all_requeriments_ok <- ifelse(test = check_ok_all_requeriments, 
                                    yes = "Yes", 
                                     no = "No")

  # Anova ----------------------------------------------------------------------
  p_value_anova_internal <- df_table_anova$"Pr(>F)"[1]
  check_anova_ho_rejected <-   p_value_anova_internal < alpha_value
  status_anova_ho <- ifelse(test = check_anova_ho_rejected, 
                             yes = "Ho Rejected", 
                              no = "Ho no rejected")
  
  AQ_anova_rejected_ho <- ifelse(test = check_anova_ho_rejected, 
                                  yes = "Yes", 
                                   no = "No")
    
  AQ_anova_is_valid <- ifelse(test = check_ok_all_requeriments, 
                               yes = "Yes, it is.", 
                                no = "No, is not!!!")
  
  p_value_anova_external <- ifelse(test = p_value_anova_internal < 0.01, 
                                    yes = "<<0.01", 
                                     no = as.character(p_value_anova_internal))
  
  # Tukey ----------------------------------------------------------------------
  amount_groups_tukey <- length(unique(tukey01_full_groups$groups))
  check_tukey_groups      <- amount_groups_tukey >= 2
  status_tukey <- ifelse(test = check_tukey_groups, 
                          yes = "At least two groups of means.", 
                           no = "Only one group.")
  
  AQ_tukey_groups <- ifelse(test = check_tukey_groups, 
                             yes = "Yes", 
                              no = "No")
    
  AQ_tukey_is_valid <- ifelse(test = check_ok_all_requeriments && check_anova_ho_rejected, 
                               yes = "Yes, it is.", 
                                no = "No, is not!!!")

  # Anova and Tukey ----------------------------------------------------------------------
  check_anova_tukey_coherence      <- check_anova_ho_rejected == check_tukey_groups
  check_anova_yes_tukey_no <- check_anova_ho_rejected  & (!check_tukey_groups)
  check_anova_no_tukey_yes <- !check_anova_yes_tukey_no

  # DF summary ----------------------------------------------------------------------
  vector_p_value_internal <- c(p_value_normality_internal, p_value_homogeneity_internal, p_value_anova_internal)
  vector_p_value_external <- c(p_value_normality_external, p_value_homogeneity_external, p_value_anova_external)
  vector_logic_rejected <- vector_p_value_internal < alpha_value
  vector_ho_decision <- ifelse(test = vector_logic_rejected, yes = "Ho Rejected", "Ho no rejected")
  vector_ho_rejected <- ifelse(test = vector_logic_rejected, yes = "Yes", "No")
  
  df_summary_anova <- data.frame(
    "name"        = c("Shapiro-Wilk test"       , "Bartlett test"             , "Anova 1 way"         , "Tukey"),
    "test"        = c("Normality"               , "Homogeneity"               , "Analysis of Variance", "Mean groups"),
    "variable"    = c("residuals"               , "residuals"                 , var_name_rv           , var_name_rv),
    "p_value"     = c(p_value_normality_internal, p_value_homogeneity_internal, p_value_anova_internal, NA),
    "alpha_value" = c(alpha_value               , alpha_value                 , alpha_value           , alpha_value),
    "Decision"    = c(status_normality_ho       , status_homogeneity_ho       , status_anova_ho       , status_tukey),
    "Valid"       = c(AQ_normality_is_valid     , AQ_homogeneity_is_valid     , AQ_anova_is_valid     , AQ_tukey_is_valid)
  )
  
  df_summary_anova

  phrase01_normality_yes_rejected <- "
The null hypothesis of normal distribution of residuals is rejected.
"

  phrase01_normality_no_rejected  <- "
The null hypothesis of normal distribution of residuals is not rejected.
"
  phrase01_normality_selected     <- ifelse(test = check_normality_ho_rejected, 
                                           yes = phrase01_normality_yes_rejected, 
                                            no = phrase01_normality_no_rejected)
  
  cat(phrase01_normality_selected)

phrase02_normality_yes_rejected <- "
The p value about normality distibution is _p_value_.  
Alpha value is _alpha_value_.  
The p value is less than alpha value.  
The null hypothesis of normal distribution of residuals is rejected.  
Los residuos no poseen distribución normal.
"

phrase02_normality_no_rejected  <- "
The p value about normality distibution is _p_value_.  
Alpha value is _alpha_value_.  
The p value is equal or mayor than alpha value.  
The null hypothesis of normal distribution of residuals is not rejected.  
Los residuos poseen distribución normal.
"

phrase02_normality_selected     <- ifelse(test = check_normality_ho_rejected, 
                                        yes = phrase02_normality_yes_rejected, 
                                        no = phrase02_normality_no_rejected)

replacements_normality <- c(
  "_p_value_" = as.character(p_value_normality_internal),
  "_alpha_value_" = as.character(alpha_value)
)

# 2. Realizar el reemplazo múltiple
phrase02_normality_selected <- str_replace_all(
  string = phrase02_normality_selected,
  pattern = replacements_normality
)

cat(phrase02_normality_selected)


  phrase01_homogeneity_yes_rejected <- "
The hypothesis of homogeneity of variances (homoscedasticity) is rejected.
"
  phrase01_homogeneity_no_rejected  <- "
  The hypothesis of homogeneity of variances (homoscedasticity) is not rejected.
"
  
  phrase01_homogeneity_selected     <- ifelse(test = check_homogeneity_ho_rejected, 
                                         yes = phrase01_homogeneity_yes_rejected, 
                                         no = phrase01_homogeneity_no_rejected)
  
  cat(phrase01_homogeneity_selected)

  phrase02_homogeneity_yes_rejected <- "
The p value about homogeneity variances from residuals is _p_value_.  
Alpha value is _alpha_value_.  
The p value is less than alpha value.  
The null hypothesis of homogeneity of variances from residuals is rejected.  
Los residuos son homogeneos.
"

  phrase02_homogeneity_no_rejected  <- "
The p value about normality distibution is _p_value_.  
Alpha value is _alpha_value_.  
The p value is equal or mayor than alpha value.  
The null hypothesis of normal distribution of residuals is not rejected.  
Los residuos son homogeneos."

  phrase02_homogeneity_selected     <- ifelse(test = check_homogeneity_ho_rejected, 
                                         yes = phrase02_homogeneity_yes_rejected, 
                                         no = phrase02_homogeneity_no_rejected)


replacements_homogeneity <- c(
  "_p_value_" = as.character(p_value_homogeneity_internal),
  "_alpha_value_" = as.character(alpha_value)
)

# 2. Realizar el reemplazo múltiple
phrase02_homogeneity_selected <- str_replace_all(
  string = phrase02_homogeneity_selected,
  pattern = replacements_homogeneity
)

  cat(phrase02_homogeneity_selected)

  phrase01A_requeriments_yes_valid <- "
All requirements for the model are met.
"
  phrase01A_requeriments_no_valid  <- "
Not all requirements for the model are met.
"
  
  phrase01A_requeriments_selected  <- ifelse(test = check_ok_all_requeriments, 
                                          yes = phrase01A_requeriments_yes_valid, 
                                          no = phrase01A_requeriments_no_valid)
  
  cat(phrase01A_requeriments_selected)  

  phrase01B_requeriments_yes_valid <- "
It is valid to draw conclusions from the ANOVA test.
"
  phrase01B_requeriments_no_valid  <- "
It is NOT valid to draw conclusions from the ANOVA test.
"
  
  phrase01B_requeriments_selected  <- ifelse(test = check_ok_all_requeriments, 
                                          yes = phrase01B_requeriments_yes_valid, 
                                          no = phrase01B_requeriments_no_valid)
  
  cat(phrase01B_requeriments_selected)  

  phrase01C_requeriments_yes_valid <- "
The analysis proceeds.
"
  phrase01C_requeriments_no_valid  <- "
As the model requirements are not met, the ANOVA and Tukey analyses must be discarded, irrespective of the statistical values obtained. The literal and decontextualized interpretation of the ANOVA test and the Tukey test is detailed below for demonstrative purposes only, but holds no validity for drawing conclusions from them.
"
  
  phrase01C_requeriments_selected  <- ifelse(test = check_ok_all_requeriments, 
                                          yes = phrase01C_requeriments_yes_valid, 
                                          no = phrase01C_requeriments_no_valid)
  
  cat(phrase01C_requeriments_selected)  

  phrase02A_requeriments_yes_valid <- phrase01A_requeriments_yes_valid

  phrase02A_requeriments_no_valid  <- phrase01A_requeriments_no_valid
  
  
  phrase02A_requeriments_selected  <- ifelse(test = check_ok_all_requeriments, 
                                          yes = phrase02A_requeriments_yes_valid, 
                                          no = phrase02A_requeriments_no_valid)
  
  
  cat(phrase02A_requeriments_selected)  

  phrase02B_requeriments_yes_valid <- "
All requirements for the model are met.
"
  phrase02B_requeriments_no_valid  <- "
It is NOT valid to draw conclusions from the ANOVA test. This dataset must be analyzed with another statistical tool such as the Kruskal-Wallis test. If a more sophisticated tool is desired, the possibility of utilizing generalized linear mixed models, generalized linear models, generalized linear models, exact distribution linear models, etc., could be evaluated.
"
  phrase02B_requeriments_selected  <- ifelse(test = check_ok_all_requeriments, 
                                          yes = phrase02B_requeriments_yes_valid, 
                                          no = phrase02B_requeriments_no_valid)
  
  
  cat(phrase02B_requeriments_selected)  

  phrase02C_requeriments_yes_valid <- "
It is valid to draw conclusions from the ANOVA test
"
  phrase02C_requeriments_no_valid  <- "
It is NOT valid to draw conclusions from the ANOVA test. As the model requirements are not met, the ANOVA and Tukey analyses must be discarded, irrespective of the statistical values obtained. The literal and decontextualized interpretation of the ANOVA test and the Tukey test is detailed below for demonstrative purposes only, but holds no validity for drawing conclusions from them.
"
  
  phrase02C_requeriments_selected  <- ifelse(test = check_ok_all_requeriments, 
                                          yes = phrase02C_requeriments_yes_valid, 
                                          no = phrase02C_requeriments_no_valid)
  
  cat(phrase02C_requeriments_selected)  


  phrase01_anova_yes_rejected <- "
The null hypothesis of the ANOVA test is rejected. 
There are statistically significant differences in at least one level of the factor.
"

  phrase01_anova_no_rejected  <- "
The null hypothesis of the ANOVA test is not rejected.
All levels of the factor are statistically equal.
"
  
  phrase01_anova_selected     <- ifelse(test = check_anova_ho_rejected, 
                                      yes = phrase01_anova_yes_rejected, 
                                      no = phrase01_anova_no_rejected)
  

 cat(phrase01_anova_selected)



  phrase02_anova_yes_rejected <- "
The p-value for the ANOVA test is _p_value_. The alpha value is _alpha_value_. 
The p-value is less than the alpha value. 
The null hypothesis of equal means for the ANOVA test is rejected. 
There are statistically significant differences in at least one level of the factor. 
By rejecting the null hypothesis, the ANOVA test guarantees statistically significant differences in at least one level of the factor with the lowest mean and the level of the factor with the highest mean.
"

  phrase02_anova_no_rejected  <- "
The p-value for the ANOVA test is _p_value_. 
The alpha value is _alpha_value_. 
The p-value is greater than or equal to the alpha value. 
The null hypothesis of equal means for the ANOVA test is not rejected. 
There are not statistically significant differences between the factor levels. 
The observed differences between the factor levels occurred by chance. All factor levels are statistically equal.
"

  phrase02_anova_selected     <- ifelse(test = check_anova_ho_rejected, 
                                      yes = phrase02_anova_yes_rejected, 
                                      no = phrase02_anova_no_rejected)
  
replacements_anova <- c(
  "_p_value_" = as.character(p_value_anova_internal),
  "_alpha_value_" = as.character(alpha_value)
)

# 2. Realizar el reemplazo múltiple
phrase02_anova_selected <- str_replace_all(
  string = phrase02_anova_selected,
  pattern = replacements_anova
)

  cat(phrase02_anova_selected)



  phrase01A_tukey_yes_groups <- "
Since the ANOVA null hypothesis is rejected, the use of a multiple comparison test to accompany the ANOVA test is valid.
"
  phrase01A_tukey_no_groups  <- "
Since the ANOVA null hypothesis is not rejected, the use of a multiple comparison test is not valid.
"
  phrase01A_tukey_selected     <- ifelse(test = check_anova_ho_rejected, 
                                      yes = phrase01A_tukey_yes_groups, 
                                      no = phrase01A_tukey_no_groups)
  

  cat(phrase01A_tukey_selected)



  phrase01B_tukey_yes_groups <- "
The Tukey test is selected as the multiple comparison test for this script.
"
  phrase01B_tukey_no_groups  <- "
The literal and decontextualized interpretation of the Tukey test is detailed below for demonstrative purposes only, but holds no validity for drawing conclusions from them.
"
  phrase01B_tukey_selected     <- ifelse(test = check_anova_ho_rejected, 
                                      yes = phrase01B_tukey_yes_groups, 
                                      no = phrase01B_tukey_no_groups)
  

  cat(phrase01B_tukey_selected)



  phrase01C_tukey_yes_groups <- "
The Tukey test indicates that there are at least 2 groups among the factor levels. 
The number of groups and their structure must be analyzed in detail using the Tukey table.
"
  phrase01C_tukey_no_groups  <- "
The Tukey test indicates that there is only one statistical group among the factor levels.
"
  phrase01C_tukey_selected     <- ifelse(test = check_tukey_groups, 
                                      yes = phrase01C_tukey_yes_groups, 
                                      no = phrase01C_tukey_no_groups)
  

  cat(phrase01C_tukey_selected)



  phrase02A_tukey_yes_groups <- phrase01A_tukey_yes_groups

  phrase02A_tukey_no_groups  <- phrase01A_tukey_no_groups
  
  phrase02A_tukey_selected     <- ifelse(test = check_anova_ho_rejected, 
                                      yes = phrase02A_tukey_yes_groups, 
                                      no = phrase02A_tukey_no_groups)
  

  cat(phrase02A_tukey_selected)



  phrase02B_tukey_yes_groups <- phrase01B_tukey_yes_groups

  phrase02B_tukey_no_groups  <- phrase01B_tukey_no_groups
  
  phrase02B_tukey_selected     <- ifelse(test = check_anova_ho_rejected, 
                                      yes = phrase02B_tukey_yes_groups, 
                                      no = phrase02B_tukey_no_groups)
  

  cat(phrase02B_tukey_selected)



  phrase02C_tukey_yes_groups <- "
The Tukey test details at least 2 statistically different groups. 
The group structure detailed by the Tukey test must now be considered in order to provide a recommendation in your area of work.
"

  phrase02C_tukey_no_groups  <- "
The Tukey test details that all factor levels form a single group. 
From the Tukey perspective, all factor levels are statistically equal.
"

  phrase02C_tukey_selected     <- ifelse(test = check_tukey_groups, 
                                      yes = phrase02C_tukey_yes_groups, 
                                      no = phrase02C_tukey_no_groups)
  
  
  cat(phrase02C_tukey_selected)



  phrase01_anova_yes_tukey_no <- "
ANOVA indicates that there are at least two groups, while Tukey indicates there is only one statistical group. 
Irrespective of Tukey's failure to find groups, since the ANOVA null hypothesis was rejected, it can still be stated that there are statistically significant differences between the factor level with the highest mean and the factor level with the lowest mean.
"

  phrase01_anova_no_tukey_yes  <- "
ANOVA indicates that all factor levels are equal, while Tukey has found at least 2 groups. 
Tukey is subject to the rejection of the ANOVA hypothesis. Since the ANOVA null hypothesis is not rejected, the Tukey test must not be taken into account for decision-making. All factor levels are equal.
"


phrase01_anova_tukey_selected     <- ifelse(test = check_anova_yes_tukey_no,                                                                 yes = phrase01_anova_yes_tukey_no, 
                                              no = phrase01_anova_no_tukey_yes)
  

  cat(phrase01_anova_tukey_selected)



  phrase02_anova_yes_tukey_no <- "
Irrespective of Tukey's failure to find groups, since the ANOVA null hypothesis was rejected, it can at least be affirmed that there are statistically significant differences between the factor level with the highest mean and the factor level with the lowest mean.
"

  phrase02_anova_no_tukey_yes  <- "
Irrespective of Tukey's finding groups, since the ANOVA null hypothesis was not rejected, all factor levels are equal.
"


phrase02_anova_tukey_selected     <- ifelse(test = check_anova_yes_tukey_no,                                                                 yes = phrase02_anova_yes_tukey_no, 
                                              no = phrase02_anova_no_tukey_yes)
  

  cat(phrase01_anova_tukey_selected)


vector_short_phrase01 <- c(phrase01_normality_selected, 
    phrase01_homogeneity_selected,
    phrase01A_requeriments_selected,
    phrase01B_requeriments_selected,
    phrase01C_requeriments_selected,
    phrase01_anova_selected,
    phrase01A_tukey_selected,
    phrase01B_tukey_selected,
    phrase01C_tukey_selected,
    phrase01_anova_tukey_selected)
cat(vector_short_phrase01)

vector_long_phrase02 <- c(phrase02_normality_selected, 
    phrase02_homogeneity_selected,
    phrase02A_requeriments_selected,
    phrase02B_requeriments_selected,
    phrase02C_requeriments_selected,
    phrase02_anova_selected,
    phrase02A_tukey_selected,
    phrase02B_tukey_selected,
    phrase02C_tukey_selected,
    phrase02_anova_tukey_selected)
cat(vector_long_phrase02)



df_decision_cases_init <- data.frame(
  "Statistic Case" =          c("Case 1"         , "Case 2"         , "Case 3"         , "Case 4"    , "Case 5"    ),
  "All requeriments OK" =     c(  "No"           ,   "Yes"          ,  "Yes"           , "Yes"       ,  "Yes"      ),
  "Anova rejected" =          c("No/Yes"         ,   "No"           ,  "No"            , "Yes"       ,  "Yes"      ), 
  "Tukey at least 2 groups" = c("No/Yes"         ,   "No"           ,  "Yes"           , "No"        ,  "Yes"      ),
  "Decision" =                c("Decision Case 1", "Decision Case 2", "Decision Case 3", "Decision Case 4", "Decision Case 5")
)

df_decision_cases_init

vector_observed_details <- c(AQ_all_requeriments_ok, AQ_anova_rejected_ho,  AQ_tukey_groups)
names(vector_observed_details) <- c("All_requeriments_ok", "anova_rejected_ho", "tukey_groups")
vector_observed_details

selected_col <- 1:length(vector_observed_details) + 1

# 2. Realizar la comparación elemento por elemento
# Comparamos las columnas seleccionadas de df_decision_cases 
# con los elementos de vector_observed_details
match_matrix <- mapply(
  function(pattern, text) {
    grepl(pattern, text, fixed = TRUE)
  }, 
  pattern = vector_observed_details, 
  text = df_decision_cases_init[, selected_col]
)

# 3. Identificar la fila donde *todos* los elementos coinciden
# La coincidencia total se da cuando la suma de 'TRUE' (1) en una fila
# es igual al número total de características (length(selected_col)).
row_match_index <- which(rowSums(match_matrix) == length(selected_col))
row_match_index


df_decision_cases_selected <- df_decision_cases_init[row_match_index, ]


df_decision_cases_01_human <- df_decision_cases_init

df_decision_cases_01_human$"Decision"[1] <- "When not all model requirements (Normality and/or Homogeneity) are met, the ANOVA analysis is immediately discarded. 
This invalidation occurs irrespective of whether the failure is due to non-normality of the residuals, non-homogeneity, or both requirements. 
Indistinctly of the statistical values obtained in the ANOVA test and the Tukey test, it is NOT valid to draw conclusions of any type; the entire analysis is completely discarded. 
An alternative statistical tool or path must be sought to analyze the data, with the quickest recommended tool being the Kruskal-Wallis test, although a better analysis may allow for the use of more robust tools such as Generalized Linear Mixed Models or Exact Statistics; the alternative statistical path is to perform a transformation on the response variable and attempt the 1-Factor ANOVA test again. 
The statistical options suggested must be verified and justified for their feasibility of use. Continuing with statistical interpretations without fulfilling the model's requirements is a serious error common among those new to statistics, and a common practice among those lacking the sufficient statistical knowledge to correctly analyze the reality they intend to study. If, despite all these warnings, you continue with the analysis, you expose your work to severe criticism from colleagues, evaluation committees, and publication reviewers."

df_decision_cases_01_human$"Decision"[2] <- "As the model requirements are met, it is valid to interpret the ANOVA p-value. The conclusions obtained from ANOVA are valid. The null hypothesis of ANOVA is not rejected. According to ANOVA, all means are statistically equal. The use of any multiple comparison test is subject to the rejection of the ANOVA null hypothesis; therefore, the use of any multiple comparison test to analyze this dataset, whether it be the Tukey test or any other, is immediately discarded. Statistical theory indicates that the ANOVA test is a more powerful test than any multiple comparison test. Although in this case the Tukey output coincides with the ANOVA view, the Tukey test is not valid in this context. Nothing should be said regarding the Tukey test or any multiple comparison test that one might wish to use. In this case, asserting that there are no differences because Tukey did not detect groups or asserting that the interpretation is stronger because ANOVA and Tukey coincide is an error, since it is not valid to draw conclusions from any multiple comparison test in this context. If the operator needs to make a recommendation, they should recommend any factor level equally. The fact of not rejecting the null hypothesis indicates that the mathematical differences between the means are due only to chance. Another way of saying this is that if the experiment were repeated a second time, there is a high probability that the mean values might even switch positions between factor levels with the same mean difference as occurred in the first experiment. It is a common and serious error to insist that there are differences somewhere within the factor levels when the model requirements are met and the ANOVA H0 is not rejected. The action by the operator (you) of insisting on differences when the null hypothesis was not rejected demonstrates a total lack of statistical knowledge for interpreting the obtained results. In summary, statistical theory indicates that if the model requirements are met, the ANOVA criterion is a superior decision-making criterion to any criterion the operator might determine. In this case, the statistical criterion is the correct one in any context."


df_decision_cases_01_human$"Decision"[3] <- "As the model requirements are met, it is valid to interpret the ANOVA p-value. The conclusions obtained from ANOVA are valid. The null hypothesis of ANOVA is not rejected. According to ANOVA, all means are statistically equal. The use of any multiple comparison test is subject to the rejection of the ANOVA null hypothesis; therefore, the use of any multiple comparison test to analyze this dataset, whether it be the Tukey test or any other, is immediately discarded. Statistical theory indicates that the ANOVA test is a more powerful test than any multiple comparison test. Although in this case you have a Tukey output that indicates groups, the Tukey test is not valid. Nothing should be said regarding the Tukey test or any multiple comparison test that one might wish to use. In this case, asserting that there are differences because Tukey detected groups or asserting that the interpretation of the Tukey test is stronger than ANOVA is a very serious error, since it is not valid to draw conclusions from any multiple comparison test in this context. If the operator needs to make a recommendation, they should recommend any factor level equally. The fact of not rejecting the null hypothesis indicates that the mathematical differences between the means are due only to chance. Another way of saying this is that if the experiment were repeated a second time, there is a high probability that the mean values might even switch positions between factor levels with the same mean difference as occurred in the first experiment. It is a common and serious error to insist that there are differences somewhere within the factor levels when the model requirements are met and the ANOVA H0​ is not rejected. The action by the operator (you) of insisting on differences between factor levels when the ANOVA null hypothesis was not rejected demonstrates a ** total lack of statistical knowledge** for interpreting the obtained results. In summary, statistical theory indicates that if the model requirements are met, the ANOVA criterion is a superior decision-making criterion to any criterion the operator might determine. In this case, the statistical criterion is the correct one in any context."

df_decision_cases_01_human$"Decision"[4] <- "All ANOVA requirements are met, it is valid to interpret the ANOVA p-value. The conclusions obtained from ANOVA are valid. The null hypothesis of ANOVA is rejected; there is at least one different mean, and the differences between the means are not only due to chance. At this point, ANOVA provides sufficient evidence to say that there are statistically significant differences between the factor level with the highest mean and the factor level with the lowest mean. By rejecting the null hypothesis of ANOVA, it is valid to interpret any chosen multiple comparison test, in this case the Tukey test. In this specific case, the Tukey test does not distinguish statistical groups even though ANOVA rejected the null hypothesis. Since ANOVA is a more powerful test than Tukey, it is affirmed that there are differences at least between the factor level with the largest mean and the factor level with the smallest one, and nothing can be said about the rest of the levels. It is a rare case, but it happens. Consider changing the multiple comparison test to another one, if appropriate, with the hope of being able to detect differences between factor levels with a different comparison test than Tukey. Statistical theory indicates that the statistical differences between factor levels may not be real or significant differences in the area of study. The operator (you) must contextualize the statistical information and decide with good judgment if the statistical differences are significant in the context of the work. In terms of practical analysis, the operator's criterion is superior to the statistical criterion."

df_decision_cases_01_human$"Decision"[5] <- "All ANOVA requirements are met. The conclusions obtained from ANOVA are valid. The null hypothesis of ANOVA is rejected; there is at least one different mean, and the differences between the means are not only due to chance. At this point, ANOVA provides sufficient evidence to say that there are statistically significant differences between the factor level with the highest mean and the factor level with the lowest mean. By rejecting the null hypothesis of ANOVA, it is valid to interpret any chosen multiple comparison test, which in this opportunity is the Tukey test. In this case, the Tukey test distinguishes at least 2 statistical groups. The operator must observe the Tukey table and interpret the groups according to the letters assigned by the Tukey test, taking into account that Tukey can assign multiple letters to each factor level. The operator must make recommendations of factor levels taking into account the statistical framework and the original work framework. It is common to recommend the highest means or the lowest means according to the context of the data. In the context of Tukey, factor levels that share at least 1 letter are statistically equal. It takes some time to get used to correctly interpreting the Tukey table. There are also other contexts where the lowest or highest means are not necessarily sought, such as specifically looking for which factor levels are statistically equal to a particular level; the Tukey test can also be applied in this context. The operator (you) must contextualize the statistical information and decide with good judgment if the statistical differences are significant in the context of the work. In terms of practical analysis, the operator's criterion is superior to the statistical criterion."


colnames(df_decision_cases_01_human)[ncol(df_decision_cases_01_human)] <- "General_Decision"

df_selected_row_01_human <- df_decision_cases_01_human[row_match_index, ]
df_selected_row_01_human

text_asa_01_human <- df_selected_row_01_human[1, ncol(df_selected_row_01_human)]


df_decision_cases_02_model_requeriments <- df_decision_cases_init

df_decision_cases_02_model_requeriments$"Decision"[1] <- "The ANOVA statistical test is based on 3 points for decision-making: shape, dispersion, and position. The shape is the normal distribution of the residuals. The dispersion is the homogeneous variances in the residuals, and the position is the mean of the response variable for one of the factor levels. By guaranteeing that the shape and dispersion of the residuals have a certain particular structure, ANOVA manages to test the position in the response variable. This is why the fulfillment of normality and homogeneity of variances of the residuals is a requirement, and only with the simultaneous fulfillment of both requirements is it valid to make an interpretation of the means. If all requirements are not met simultaneously, the ANOVA analysis must be discarded, irrespective of the results it may have yielded. There is no escape, and you must seek another statistical tool to interpret these results."

df_decision_cases_02_model_requeriments$"Decision"[2] <- "As the model requirements are met, it is valid to interpret the ANOVA p-value. The conclusions obtained from ANOVA are valid. The fact that the residuals possess normal distribution and homogeneity of variances is a characteristic that is often overlooked as a minor detail. What you are observing is a special characteristic of the data. In many contexts, thinking about why the factor level residuals are normal and homogeneous leads to ideas that had not been considered and greatly enriches the work being performed. Although ANOVA is a test for means, ANOVA is an abbreviation for 'Analysis of Variance' and not 'Analysis of Means', precisely because it manages to say something about the means through the analysis of variance decomposition. Therefore, thinking about the variances of the residuals can be an interesting idea."


df_decision_cases_02_model_requeriments$"Decision"[3] <- df_decision_cases_02_model_requeriments$"Decision"[2]

df_decision_cases_02_model_requeriments$"Decision"[4] <- df_decision_cases_02_model_requeriments$"Decision"[2]

df_decision_cases_02_model_requeriments$"Decision"[5] <- df_decision_cases_02_model_requeriments$"Decision"[2]

colnames(df_decision_cases_02_model_requeriments)[ncol(df_decision_cases_02_model_requeriments)] <- "Requeriments_Decision"

df_selected_row_02_model_requeriments <- df_decision_cases_02_model_requeriments[row_match_index, ]
df_selected_row_02_model_requeriments


my_df <- df_decision_cases_02_model_requeriments
caption_text <- ""
col_align <- paste(rep("c", ncol(my_df)), collapse = "")
col_align[length(col_align)] <- "l"
  knitr::kable(my_df, format = "html", caption = caption_text, align = col_align) %>%
    # CAMBIO 2: Añadir position = "center" (centra la tabla completa en la página)
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                              full_width = FALSE,
                              position = "center",
                              font_size = 12) %>%
    kableExtra::column_spec(1, width = "8%") %>%    # Caso
    kableExtra::column_spec(2, width = "12%") %>%   # Homogeneidad
    kableExtra::column_spec(3, width = "10%") %>%   # ANOVA
    kableExtra::column_spec(4, width = "12%") %>%   # Tukey
    kableExtra::column_spec(5, width = "40%") %>%
    kableExtra::row_spec(
    row = row_match_index, # Usa el índice de la fila coincidente
    extra_css = "background-color: #FFFFCC;" # Color de fondo suave (amarillo claro)
    # Otros colores comunes: #E6F7FF (Azul claro), #E6FFE6 (Verde claro)
    )
 # ------------------------------------------------------------------
     # CÓDIGO A AGREGAR AL FINAL: Resalta la fila usando el índice calculado
    
# ------------------------------------------------------------------

text_asa_02_model_requeriments <- df_selected_row_02_model_requeriments[1, ncol(df_selected_row_02_model_requeriments)]


df_decision_cases_03_anova <- df_decision_cases_init

df_decision_cases_03_anova$"Decision"[1] <- "The model requirements are not met. The ANOVA statistical test has the indispensable requirement of residual normality and homogeneity of variances. Both requirements must be met jointly for it to be valid to draw conclusions about the factor levels. Continuing with the statistical interpretation without fulfilling all the requirements is a catastrophic error. The ANOVA model requirements are neither optional nor can they be assumed. The model requirements are the fundamental basis upon which the ANOVA test relies to make decisions about the means of the factor levels. In this case, you must seek another statistical tool to perform the analysis of your data."

df_decision_cases_03_anova$"Decision"[2] <- "As the model requirements are met, it is valid to interpret the ANOVA p-value. The conclusions obtained from ANOVA are valid. The ANOVA null hypothesis is not rejected; therefore, there are no statistically significant differences between the factor levels. All factor levels are statistically equal. If a recommendation of factor levels is needed, all factor levels must be recommended equally. In the case where the model requirements are met and the null hypothesis is not rejected, the mathematical differences observed between the factor levels are due only to chance. This means that if the experiment were repeated, one of two scenarios could occur: Scenario 1) The differences between the means would be even smaller, and the order of the means could also change if the factor levels were ordered from highest to lowest. Scenario 2) The differences and the means remain, but the means fall into different factor levels, thus changing the order of the factor levels if they are ordered from highest to lowest. Statistical theory is very clear on this and indicates that in case the null hypothesis is not rejected, then no differences exist, and if the operator perceives differences visually (by eye), the statistical criterion prevails over the operator's criterion. Actions of insisting and arguing in favor of concepts such as 'marginal differences' or 'by eye' on the part of the operator constitute a serious statistical error that demonstrates a profound lack of statistical knowledge on the part of the operator.
"


df_decision_cases_03_anova$"Decision"[3] <- df_decision_cases_03_anova$"Decision"[2]


df_decision_cases_03_anova$"Decision"[4] <- "As the model requirements are met, it is valid to interpret the ANOVA p-value. The conclusions obtained from ANOVA are valid. The ANOVA null hypothesis is rejected; therefore, statistically significant differences exist between the factor levels. The differences found between the factor levels are not due only to chance. There are at least two groups within the factor levels. ANOVA guarantees that there are statistically significant differences between the factor level with the lowest mean and the factor level with the highest mean. Only that. It says nothing about the rest of the factor levels. The use of any comparison test is subject to the rejection of the ANOVA null hypothesis. In this case, then, it is valid to use any multiple comparison test to try to visualize a group structure among the factor levels. If there is a discrepancy between the ANOVA test, which indicates that there are significant differences, and the operator, who does not see significant differences with the naked eye, statistical theory is very clear on the matter. In this framework, where ANOVA indicates differences and the operator's criterion says there are no differences, the operator's criterion always prevails, provided it is argued correctly and thoroughly. Since the ANOVA test is valid, and the ANOVA null hypothesis has been rejected, it is therefore valid to perform any multiple comparison test.
"

df_decision_cases_03_anova$"Decision"[5] <- df_decision_cases_03_anova$"Decision"[4]

colnames(df_decision_cases_03_anova)[ncol(df_decision_cases_03_anova)] <- "Anova_Decision"

df_selected_row_03_anova <- df_decision_cases_03_anova[row_match_index, ]
df_selected_row_03_anova

text_asa_03_anova <- df_selected_row_03_anova[1, ncol(df_selected_row_03_anova)]


df_decision_cases_04_tukey <- df_decision_cases_init

df_decision_cases_04_tukey$"Decision"[1] <- "The use of the Tukey test is subject to the validity of the ANOVA model and the rejection of the ANOVA hypothesis. As the model requirements are not met, the ANOVA test is not valid, and therefore the Tukey test is not valid either. The information related to the Tukey test must be discarded. As detailed before, in this context, continuing with the statistical interpretation is a serious statistical error that demonstrates a total lack of statistical criteria for decision-making."

df_decision_cases_04_tukey$"Decision"[2] <- "The use of the Tukey test is subject to the validity of the ANOVA model and the rejection of the ANOVA hypothesis. As the model is valid because the requirements are met, and the ANOVA null hypothesis has not been rejected, the use of not only Tukey but any other multiple comparison test is immediately discarded. All factor levels are statistically equal. In this Case 2, ANOVA and Tukey are coherent, in the sense that both detail that only one statistical group exists. The idea of asserting that the results have more weight because ANOVA and Tukey are coherent in only detecting one group is an error, since from the outset it is not valid to draw any conclusions or make any assertions based on any multiple comparison test, as the ANOVA null hypothesis has not been rejected.
"


df_decision_cases_04_tukey$"Decision"[3] <- "Aquí tiene la traducción del texto, manteniendo el formato de bloque continuo sin listas ni encabezados, y preservando el tono técnico y la explicación de la discrepancia.

English Translation:

The use of the Tukey test is subject to the validity of the ANOVA model and the rejection of the ANOVA hypothesis. As the model is valid because the requirements are met and the ANOVA null hypothesis has indeed been rejected, the possibility of using a multiple comparison test is immediately enabled. For our case, this is the Tukey test. When using the Tukey test, remember the importance of detailing in the R command whether or not there is an imbalance in the repetitions between the factor levels. In this Case 3, ANOVA and Tukey are not coherent, in the sense that ANOVA details that differences exist but Tukey does not find them. Statistical theory indicates that ANOVA always prevails over Tukey because ANOVA is a more powerful test; therefore, statistically significant differences exist at least between the factor level with the lowest mean and the factor level with the highest mean, and nothing can be said about the rest of the factor levels. In some contexts, this situation complicates the statistical interpretation or the ability to recommend factor levels. In such a case, if appropriate, it may help to use another multiple comparison test other than Tukey, with the idea of actually finding groups among the factor levels. The discrepancy between ANOVA and Tukey is not very common, but it is possible. Most cases occur primarily when the operator insists on running the statistical test without meeting the model requirements, or when the operator fails to specify in the R command that there is an imbalance in the factor levels so that Tukey can internally apply a correction to its estimation. But even when the requirements are met and the argument is specified correctly, there is a chance that this may occur, since ANOVA and Tukey are, after all, two different tests.
"

df_decision_cases_04_tukey$"Decision"[4] <- "The use of the Tukey test is subject to the validity of the ANOVA model and the rejection of the ANOVA hypothesis. As the model is valid because the requirements are met and the ANOVA null hypothesis has not been rejected, the possibility of applying any multiple comparison test to accompany the ANOVA test is immediately invalidated. Irrespective of the statistical groups that we might obtain from Tukey, the Tukey test must not be taken into account for drawing conclusions of any kind. Statistical theory indicates that the ANOVA test is more powerful than the Tukey test and that there is a hierarchy between them when making decisions: the Tukey test is only valid if the ANOVA H0 is rejected. This incoherence between the ANOVA test and the Tukey test is usually seen when the operator is even more incoherent and analyzes ANOVA and Tukey without fulfilling the model requirements. In this Case 4, all factor levels are statistically equal. And the use of any multiple comparison test is rendered impossible.
"


df_decision_cases_04_tukey$"Decision"[5] <- "The use of the Tukey test is subject to the validity of the ANOVA model and the rejection of the ANOVA hypothesis. As the model is valid because the requirements are met and the ANOVA null hypothesis has effectively been rejected, the possibility of applying a multiple comparison test to accompany the ANOVA test is immediately opened. In this Case 5, there is coherence between ANOVA and Tukey, in the sense that both find at least two groups among the factor levels. ANOVA's guarantee is that statistically significant differences exist between the factor level with the lowest mean and the factor level with the highest mean. Tukey then provides us with a structure of what the groups look like within the factor levels. The Tukey test has the particularity that it generates intermediate groups. Each factor level can belong to more than one statistical group. It takes some time to get used to how to interpret the Tukey test. All factor levels that share at least one grouping letter are statistically equal to each other. It is very important to contextualize the statistical groups within the framework of the work's reference.
"

colnames(df_decision_cases_04_tukey)[ncol(df_decision_cases_04_tukey)] <- "Tukey_Decision"

df_selected_row_04_tukey <- df_decision_cases_04_tukey[row_match_index, ]
df_selected_row_04_tukey

text_asa_04_tukey <- df_selected_row_04_tukey[1, ncol(df_selected_row_04_tukey)]


