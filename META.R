##DMITRY_SERGEEV_META##




# necessary libraries
library(readxl)
library(meta)
library(dplyr)





## UPLOAD DATA ##

# read data from Excel file
df <- read_xlsx('systematic_review.xlsx')# your data 

# Name of all colums inside XSX file 
# Author/ Country /	Study design /	Intervention /	Condition	/ Type of analysis /	TE /	lower /	upper


# take logarithm of TE, lower, and upper
log_cols <- c("TE", "lower", "upper")

df[, log_cols] <- lapply(df[, log_cols], log)

# add a column with missing values for seTE

df$seTE <- NA

# split data into subgroups based on X , Y and Z

subgroups <- split(df, list(df$X, df$Y, df$Z))#change X,Y,Z for your names



## MAKE PLOTS ##

# loop over subgroups and create results for each one


for (i in seq_along(subgroups)) {
  subgroup <- subgroups[[i]]
  title <- paste("_____NAME____", subgroup$X[1], subgroup$Y[1])#change X,Y for your names
  meta <- metagen(TE = subgroup$TE,
                  seTE = subgroup$seTE,
                  lower = subgroup$lower,
                  upper = subgroup$upper,
                  data = subgroup,
                  comb.fixed = FALSE,
                  comb.random = TRUE,
                  title = title,
                  sm = "RR",
                  subgroup = subgroup$Z,#change Z for your names
                  studlab = subgroup$A)#change A for your names
  
  print(summary(meta))
}



#loop over subgroups and create forest plots for each one

for (i in seq_along(subgroups)) {
  subgroup <- subgroups[[i]]
  title <- paste("_____NAME____", subgroup$X[1], subgroup$Y[1])#change X,Y for your names
  meta <- metagen(TE = subgroup$TE,
                  seTE = subgroup$seTE,
                  lower = subgroup$lower,
                  upper = subgroup$upper,
                  data = subgroup,
                  comb.fixed = FALSE,
                  comb.random = TRUE,
                  title = title,
                  sm = "RR",
                  subgroup = subgroup$Z,#change Z for your names
                  studlab = subgroup$A)#change A for your names
  forest.meta(meta,
              leftcols = c("Author", "Study design", "Country", "Intervention" ),
              leftlabs = c("Author", "Study design", "Country", "Intervention" ),
              width = 40,# layout = "JAMA",
              height = 40)
}

## SAVE PLOTS ##

# Create a directory for the plots

if (!dir.exists("Forest plots")) {
  dir.create("Forest plots")
}


for (i in seq_along(subgroups)) {
  subgroup <- subgroups[[i]]
  title <- paste("_____NAME____", subgroup$X[1], subgroup$Y[1], subgroup$Z[1])#change X,Y for your names
  meta <- metagen(TE = subgroup$TE,
                  seTE = subgroup$seTE,
                  lower = subgroup$lower,
                  upper = subgroup$upper,
                  data = subgroup,
                  comb.fixed = FALSE,
                  comb.random = TRUE,
                  title = title,
                  sm = "RR",
                  subgroup = subgroup$Z,#change Z for your names
                  studlab = subgroup$A)#change A for your names
  
  # Create forest plot for the subgroup
  
  png(filename = paste0("Forest plots/forest_plot_", gsub(" ", "_", title), ".png"), width = 900, height = 450)
  
  forest(meta,leftcols = c("Author", "Study design", "Country", "Intervention" ),
         leftlabs = c("Author", "Study design", "Country", "Intervention" ),
         ylim=c(-2,length(meta$TE)+1),main=paste("observation:")#,        # layout = "JAMA"
  )
  
  dev.off()
}

## Identification of outliers ##

#https://raw.githubusercontent.com/MathiasHarrer/dmetar/master/R/find.outliers.R
#The “find.outliers” Function
#Make sure that the {meta} and {metafor} package is installed and loaded.

# perform Revised metanalysis using the Outlier Removal method

# define the subgroups for analysis

subgroups <- list(
  Any_site_Incidence = c('Name of colum  == "Incidence"', 'Name of the colum  == "Any site"'),
  Any_site_Mortality = c('Inter == "Mortality"', 'Type_of_analysis == "Any site"')#exmaple
  
)



# perform the analysis for each subgroup

for (name in names(subgroups)) {
  filters <- subgroups[[name]]
  df_filtered <- df %>% filter(eval(parse(text = filters[1]))) %>% filter(eval(parse(text = filters[2])))
  meta_check <- find.outliers(metagen(TE = TE,
                                      seTE = seTE,
                                      lower = lower,
                                      upper = upper,
                                      data = df_filtered,
                                      comb.fixed = FALSE,
                                      comb.random = TRUE, 
                                      title = paste0('_____NAME____ ', name),
                                      sm = 'RR', 
                                      subgroup = Intervention,
                                      studlab = Author))
  print(meta_check)
}


#Additional analysis to verify the results
#Argument Method
#method.tau = "REML" Restricted maximum-likelihood estimator (Viechtbauer, 2005)
#method.tau = "PM" Paule-Mandel estimator (Paule and Mandel, 1982)
#method.tau = "DL" DerSimonian-Laird estimator (DerSimonian and Laird, 1986)
#method.tau = "ML" Maximum-likelihood estimator (Viechtbauer, 2005)
#method.tau = "HS" Hunter-Schmidt estimator (Hunter and Schmidt, 2015)
#method.tau = "SJ" Sidik-Jonkman estimator (Sidik and Jonkman, 2005)
#method.tau = "HE" Hedges estimator (Hedges and Olkin, 1985)
#method.tau = "EB" Empirical Bayes estimator (Morris, 1983)


# Define the datasets

datasets <- list(
  list(name = "_____NAME____", data = df_1),
  list(name = "_____NAME____", data = df_2)
)

# Define the estimation methods

methods <- c("REML", "PM", "DL", "ML", "HS", "SJ", "HE", "EB")



# Define the estimation methods

methods <- c("REML", "PM", "DL", "ML", "HS", "SJ", "HE", "EB")
dir.create("Methods Plots")


# Loop over the datasets and estimation methods to generate forest plots

for (dataset in datasets) {
  for (method in methods) {
    meta_result <- metagen(
      TE = TE,
      seTE = seTE,
      lower = lower,
      upper = upper,
      data = dataset$data,
      comb.fixed = FALSE,
      comb.random = TRUE,
      title = paste("Observational", dataset$name),
      sm = "RR",
      subgroup = Intervention,
      method.tau = method,
      studlab = Author
    )
    png(file = paste0("Methods Plots/", dataset$name, "_", method, ".png"), width = 1200, height = 800)
    forest(meta_result, ylim=c(-2, length(meta_result$TE)+1),
           main=paste("Estimation Method: ", method, " for ", dataset$name))
    dev.off()
  }
}

# Loop over the datasets and estimation methods to generate forest plots

for (dataset in datasets) {
  for (method in methods) {
    meta_result <- metagen(
      TE = TE,
      seTE = seTE,
      lower = lower,
      upper = upper,
      data = dataset$data,
      comb.fixed = FALSE,
      comb.random = TRUE,
      title = paste("Observational", dataset$name),
      sm = "RR",
      subgroup = Intervention,
      method.tau = method,
      studlab = Author
    )
    
    # Print the summary results with method name
    
    print(paste("Method:", method))
    print(summary(meta_result))
    print("-------------------------------------------------")
  }
}



