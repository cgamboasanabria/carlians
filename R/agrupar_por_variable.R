agrupar_por_variable <- function(variable){
    #Genera grupos en base a los cambios en el valor de otra variable
    data.table::rleid(variable)
}
