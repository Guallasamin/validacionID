validar_ID  <- function(cedula) {
    if (is.na(cedula)) {
        return(FALSE)
    }
    
    cedula <- as.character(cedula)
    
    if (nchar(cedula) != 10 || grepl('[A-Za-z[:punct:]]', cedula)) {
        return(FALSE)
    }
    
    digito_verificador <- as.numeric(substr(cedula, 10, 10))
    cedula_numerica <- as.numeric(strsplit(cedula, '')[[1]])
    
    # Pesos para la multiplicación
    pesos <- c(2, 1, 2, 1, 2, 1, 2, 1, 2)
    
    # Multiplicación y ajuste de los resultados
    resultados <- sapply(1:9, function(i) {
        valor <- cedula_numerica[i] * pesos[i]
        if (valor > 9) valor - 9 else valor
    })
    
    # Suma de los resultados
    suma_resultados <- sum(resultados)
    
    # Obtención del dígito verificador
    digito_calculado <- 10 - (suma_resultados %% 10)
    digito_calculado <- ifelse(digito_calculado == 10, 0, digito_calculado)
    
    return(digito_calculado == digito_verificador)
}