gerarCodigoSoma <- function(itens, colunas){
  codigo <- '';
  for (coluna in colunas) {
    for (item in itens) {
      codigo <- paste0(codigo,'engine.put("v',item,'_',coluna, '",resgatarValor("',item,'", "',coluna,'", r));')
    }
    
    
  }
  
  for(col in 1:length(colunas)){
    codigo <- paste0(codigo,'String calcCol', colunas[col], ' = "')
    for (i in 1:length(itens)) {
      if(i == length(itens)){
        codigo <- paste0(codigo,' v',itens[i],'_',colunas[col], '";')
      }else{
        codigo <- paste0(codigo,' v',itens[i],'_',colunas[col], ' + ')
      }
    }
    
    codigo <- paste0(codigo, 'getCalculoValores().get(', (col-1), ').setVlResultado(
					new BigDecimal(engine.eval(' ,'calcCol', colunas[col], ').toString()));')
    
  }
  
  write(codigo,"codigo.txt")
  
  codigo
  
  
}

gerarCodigoSomaSubtracaoFinal <- function(itens, colunas){
  codigo <- '';
  for (coluna in colunas) {
    for (item in itens) {
      codigo <- paste0(codigo,'engine.put("v',item,'_',coluna, '",resgatarValor("',item,'", "',coluna,'", r));')
    }
    
    
  }
  
  for(col in 1:length(colunas)){
    codigo <- paste0(codigo,'String calcCol', colunas[col], ' = "')
    for (i in 1:length(itens)) {
      if(i == 1){
        codigo <- paste0(codigo,' v',itens[i],'_',colunas[col])
      }else if(i == length(itens)){
        codigo <- paste0(codigo,' - v',itens[i],'_',colunas[col], '";')
      }else{
        codigo <- paste0(codigo,' + v',itens[i],'_',colunas[col])
      }
    }
    
    codigo <- paste0(codigo, 'getCalculoValores().get(', (col-1), ').setVlResultado(
                     new BigDecimal(engine.eval(' ,'calcCol', colunas[col], ').toString()));')
    
  }
  
  write(codigo,"codigo.txt")
  
  codigo
  
  
}

gerarCodigoSomaSubtracaoFinalNoMeio <- function(itens, colunas){
  codigo <- '';
  for (coluna in colunas) {
    for (item in itens) {
      codigo <- paste0(codigo,'engine.put("v',item,'_',coluna, '",resgatarValor("',item,'", "',coluna,'", r));')
    }
  }
  
  for(col in 1:length(colunas)){
    codigo <- paste0(codigo,'String calcCol', colunas[col], ' = "')
    
    if(col == 1 | col == 5){
      codigo <- paste0(codigo,' v',itens[1],'_',colunas[col],'";')
    }else{
      for (i in 1:length(itens)) {
        if(i == 1){
          codigo <- paste0(codigo,' v',itens[i],'_',colunas[col])
        }else if(i == 3 ){
          codigo <- paste0(codigo,' v',itens[i],'_',colunas[col])
        }else if(i == 4 | i == 5){
          codigo <- paste0(codigo,' + v',itens[i],'_',colunas[col])
        }else if(i == 2){
          codigo <- paste0(codigo,' + v',itens[i],'_',colunas[col],' - (')
        }
        
      }
      codigo <- paste0(codigo, ')";')
    }
    codigo <- paste0(codigo, 'getCalculoValores().get(', (col-1), ').setVlResultado(
                     new BigDecimal(engine.eval(' ,'calcCol', colunas[col], ').toString()));')
    
  }
  
  write(codigo,"codigo.txt")
  
  codigo
  
  
}

gerarCodigoSoma(c(1794,1596,1024,1025,1026),c(5,8,9))

gerarCodigoSomaSubtracaoFinal(c(1481,1637,2147),c(5,8,9))


gerarCodigoSomaSubtracaoFinalNoMeio(c(1050,2128,1918,1919,1920),c(1,10,2,11,12))

