# --------------------------------------------------
# FROM FILE: report01_lab01_step03_CopyMod_RQuarto_01_ANOVA.qmd
# --------------------------------------------------

# # # # # Section 01 - Libraries ---------------------------------------------
  library("stats")     # General Linear Models
  library("agricolae") # Tukey test
  library("plotly")    # Advanced graphical functions
  library("dplyr")     # Developing with %>%
  library("stringr")   # Strings replacement
  library("EnvStats")  # QQplot

# El external
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
# FROM FILE: report01_lab00_original_RQuarto_02_appendix01_sec13_Descriptive_RV.qmd
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
# FROM FILE: report01_lab00_original_RQuarto_03_appendix02_sec14_Descriptive_Residuals.qmd
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
  


