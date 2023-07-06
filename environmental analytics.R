

####display only the functions from the R environment ####
library(codetools)
library(igraph)

# Get the function names from the global environment
function_names <- ls(envir = globalenv())
function_names <- function_names[sapply(function_names, function(x) is.function(get(x)))]

# Create an empty graph
graph <- graph.empty(n = length(function_names), directed = TRUE)

# Add nodes for each function
V(graph)$name <- function_names

# Iterate through function names
for (func in function_names) {
  deps <- try(codetools::findGlobals(get(func, envir = globalenv()))$functions, silent = TRUE)
  if (!inherits(deps, "try-error")) {
    # Filter dependencies to include only functions from the global environment
    deps <- intersect(deps, function_names)
    graph <- add_edges(graph, from = func, to = deps)
  }
}

# Plot the graph
plot(graph)


####display the variables used by each function in the R environment####

# Get the function names from the global environment
function_names <- ls(envir = globalenv())
function_names <- function_names[sapply(function_names, function(x) is.function(get(x)))]

# Iterate through function names
for (func in function_names) {
  # Get the function object
  function_obj <- get(func, envir = globalenv())
  
  # Get the variables used in the function
  variables <- names(formals(function_obj))
  
  # Display the function name and its variables
  cat("Function:", func, "\n")
  cat("Variables:", paste(variables, collapse = ", "), "\n\n")
}


####identify variables that are used in multiple functions from the environment and are repeated####

# Get the function names from the global environment
function_names <- ls(envir = globalenv())
function_names <- function_names[sapply(function_names, function(x) is.function(get(x)))]

# Create an empty list to store the repeated variables
repeated_variables <- list()

# Iterate through function names
for (func in function_names) {
  # Get the function object
  function_obj <- get(func, envir = globalenv())
  
  # Get the variables used in the function
  variables <- names(formals(function_obj))
  
  # Check if the variables are already in the repeated_variables list
  # If yes, add the function name to the list
  # If no, add the variables to the repeated_variables list
  for (variable in variables) {
    if (variable %in% names(repeated_variables)) {
      repeated_variables[[variable]] <- c(repeated_variables[[variable]], func)
    } else {
      repeated_variables[[variable]] <- func
    }
  }
}

# Display the repeated variables and the functions that use them
for (variable in names(repeated_variables)) {
  functions <- repeated_variables[[variable]]
  if (length(functions) > 1) {
    cat("Variable:", variable, "\n")
    cat("Functions:", paste(functions, collapse = ", "), "\n\n")
  }
}


###version of the code that displays the repeated variables and the functions that use them as a table###
# Get the function names from the global environment
function_names <- ls(envir = globalenv())
function_names <- function_names[sapply(function_names, function(x) is.function(get(x)))]

# Create an empty list to store the repeated variables
repeated_variables <- list()

# Iterate through function names
for (func in function_names) {
  # Get the function object
  function_obj <- get(func, envir = globalenv())
  
  # Get the variables used in the function
  variables <- names(formals(function_obj))
  
  # Check if the variables are already in the repeated_variables list
  # If yes, add the function name to the list
  # If no, add the variables to the repeated_variables list
  for (variable in variables) {
    if (variable %in% names(repeated_variables)) {
      repeated_variables[[variable]] <- c(repeated_variables[[variable]], func)
    } else {
      repeated_variables[[variable]] <- func
    }
  }
}

# Prepare the data for the table
variable_names <- names(repeated_variables)
function_names <- sapply(repeated_variables, function(x) paste(x, collapse = ", "))

# Create the table
table_data <- data.frame(Variable = variable_names, Functions = function_names)

# Display the table
print(table_data)
write.table(table_data, file = "repeated_variables.csv", sep = ",", quote = FALSE, row.names = FALSE)


####Determine which input variables are used by other functions in the environment####

# Get the function names from the global environment
function_names <- ls(envir = globalenv())
function_names <- function_names[sapply(function_names, function(x) is.function(get(x)))]

# Create an empty list to store the input variable dependencies
variable_dependencies <- list()

# Iterate through function names
for (func in function_names) {
  # Get the function object
  function_obj <- get(func, envir = globalenv())
  
  # Get the variables used in the function
  variables <- names(formals(function_obj))
  
  # Remove self-references
  variables <- variables[variables != func]
  
  # Store the input variables as dependencies
  variable_dependencies[[func]] <- variables
}

# Iterate through function names
for (func in function_names) {
  dependent_inputs <- c()
  
  # Find functions that use the inputs of the current function
  for (dep_func in function_names) {
    if (dep_func != func) {
      inputs <- variable_dependencies[[dep_func]]
      if (any(inputs %in% variable_dependencies[[func]])) {
        dependent_inputs <- c(dependent_inputs, dep_func)
      }
    }
  }
  
  # Display the dependent functions
  if (length(dependent_inputs) > 0) {
    cat("Function:", func, "\n")
    cat("Dependent Functions:", paste(dependent_inputs, collapse = ", "), "\n\n")
  }
}


###visualize the directed edges between functions####

library(igraph)

Dependent_Functions <- read.csv("DF_CO.csv")
Dependent_Functions <- Dependent_Functions[1:157,]

# Create the graph
plot1 <- graph_from_data_frame(Dependent_Functions, directed = FALSE)

# Set the layout using circle layout
circ <- layout_in_circle(plot1)

# Plot the graph with the desired layout
plot(plot1, layout = circ, edge.arrow.size = .1, vertex.color = "lightblue", vertex.size = 20, vertex.label.cex = 0.8)



plot(plot1, layout = circ, edge.arrow.size = 0.6, vertex.color = "lightblue", vertex.size = 10, vertex.label.cex = 0.7, edge.color = "red")


