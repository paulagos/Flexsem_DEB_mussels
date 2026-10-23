###############################################################################
# get_variables.R
#
# Returns variables matching a given type and/or group.
###############################################################################

###############################################################################
# get_variables.R
#
# Returns variables matching a given type and/or group.
###############################################################################

get_variables <- function(type = NULL,
                          group = NULL){

  metadata <- get_variable_info()

  variables <- names(metadata)

  if(!is.null(type)){

    type_vector <- sapply(
        metadata,
        function(x) x$type
      )
    
    variables <- names(type_vector[type_vector == type])
  }

  if(!is.null(group)){

    group_vector <- sapply(
      metadata[variables],
      function(x) x$group
    )
    
    variables <- variables[group_vector == group]

  }

  return(variables)

}