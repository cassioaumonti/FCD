
# funcoes

# tipo: 2 = bi
# tipo: 1 = penta

ciclos = function(rendimento, cont, out, descarga, tipo){
  
  ccls = 0
  
  repeat{
    
    for(j in 1:4){
      
      cont = cont + rendimento[j]
      
      out = cbind(out,cont)
      
    }
    
    ccls = ccls + 1
    
    if((out[length(out)]-out[1]+sum(rendimento)) > (21+descarga*2)){
      
      out = cbind(out,tipo,ccls)
      
      break
    }
    
  }
  
  return(out)
}

# Simulador de fila

# adicionando atrasos
desv = c(0, 1, 2, 3)
final_file = NULL

for(dly in 1:length(desv)){
  # dly=1
  # frota
  npenta = 18
  nbi = 21
  ngpenta = 6; ngbi = 3
  ngd = 3
  
  # capacidade de carga liquida
  cap_penta = 117
  cap_bi = 37
  
  # tempos
  transito_penta = 2.2 + desv[dly]
  transito_bi = 2 + desv[dly]
  
  transito_cheio_penta = 2.8 + desv[dly]
  transito_cheio_bi = 2.3 + desv[dly]
  
  carga_penta = 1.1
  carga_bi = 0.8
  
  descarga_penta = 1
  descarga_bi = 0.6
  
  rendimento.penta = c(transito_penta,carga_penta,transito_cheio_penta,descarga_penta)
  rendimento.bi = c(transito_bi,carga_bi,transito_cheio_bi,descarga_bi)
  
  # intervalo de tempo em que os caminhos inicial trabalho
  horas = 5
  
  # calculo do itinerario de cada caminhao partindo ao mesmo tempo
  out = tempos = NULL
  for(i in 1:(npenta+nbi)){
    
    # penta
    if(i <= npenta){
      cont = horas
      out = cont
      
      out1=ciclos(rendimento=rendimento.penta,cont=cont,out=out,descarga=descarga_penta,tipo = 1)
      
      # bitrens
    }else{
      
      cont = horas
      out = cont
      
      out1=ciclos(rendimento=rendimento.bi,cont=cont,out=out,descarga=descarga_bi,tipo = 2)
      
    }
    
    tempos[i] = list(out1)
    
  }
  
  # resumo dos tempos de cada operacao por caminhao
  tempo1 = tempos
  
  # atualizacao do tempo de fila na carga final transportada
  fila.carga = f.carga = NULL
  for(i in 1:length(tempos)){
    # i=3
    # penta com fila
    if(i <= npenta & i > ngpenta){
      
      # tempos de fila do truck i em cada ciclo k para n gruas
      for(k in 1:tempos[[i]][length(tempos[[i]])]){
        
        a = tempos[[abs(i-ngpenta)]][(3+4*(k-1))] - tempos[[i]][(2+4*(k-1))]
        
        if(a > 0){
          
          # decidir tempo de atraso para o caminhao i
          # atraso = fuzzy
          # 
          #tempos[[i]] = tempos[[i]] + atraso
          
          b = tempos[[i]][(3+4*(k-1)):(length(tempos[[i]])-2)] + a
          
          tempos[[i]][(3+4*(k-1)):(length(tempos[[i]])-2)] = b 
          
        }else{
          
          a = 0
          
        }
        
        f.carga = cbind(f.carga,a)
        
      }
      
      fila.carga[i] = list(f.carga)
      
      f.carga = NULL
      
      # bi com fila
    }else if(i > npenta & i > (ngbi+npenta)){
      
      # tempos de fila do truck i em cada ciclo k para n gruas
      for(k in 1:tempos[[i]][length(tempos[[i]])]){
        # k=3
        # a = round(sum(sapply(1:id_grua, function(j){
        #   return(tempos[[j]][(3+4*(k-1))])})) - id_grua*tempos[[i]][(2+4*(k-1))],2)
        
        a = tempos[[abs(i-ngbi)]][(3+4*(k-1))] - tempos[[i]][(2+4*(k-1))]
        
        if(a > 0){
          
          b = tempos[[i]][(3+4*(k-1)):(length(tempos[[i]])-2)] + a
          
          tempos[[i]][(3+4*(k-1)):(length(tempos[[i]])-2)] = b 
          
        }else{
          
          a = 0
          
        }
        
        f.carga = cbind(f.carga,a)
        
      }
      
      fila.carga[i] = list(f.carga)
      
      f.carga = NULL
      
    }else{
      
      fila.carga[i] = 0
      
    }
    
  }
  
  tempo2 = tempos
  
  # orderm de chegada de caminhoes no descarregamento - ciclo k
  for(i in 1:length(tempos)){
    a[i] = tempos[[i]][4]
  }
  
  ord = order(a,decreasing = F)
  
  f.descarga = fila.descarga = NULL
  for(i in 1:length(tempos)){
    
    if(i > ngd){
      
      # tempos de fila do truck i em cada ciclo k para n gruas
      for(k in 1:tempos[[ord[i]]][length(tempos[[ord[i]]])]){
        
        # a = round(sum(sapply(1:id_grua, function(j){
        #   return(tempos[[j]][(3+4*(k-1))])})) - id_grua*tempos[[i]][(2+4*(k-1))],2)
        
        a = tempos[[ord[abs(i-ngd)]]][(5+4*(k-1))] - tempos[[ord[i]]][(4+4*(k-1))]
        
        if(round(a) > 0 & !is.na(a)){
          
          b = tempos[[ord[i]]][(5+4*(k-1)):(length(tempos[[ord[i]]])-2)] + a
          
          tempos[[ord[i]]][(5+4*(k-1)):(length(tempos[[ord[i]]])-2)] = b 
          
        }else{
          
          a = 0
          
        }
        
        f.descarga = cbind(f.descarga,a)
        
      }
      
      fila.descarga[i] = list(f.descarga)
      
      f.descarga = NULL
      
    }else{
      
      fila.descarga[i] = 0
      
    }
    
  }
  
  # Resumindo
  for(i in 1:length(tempos)){
    
    # calculo de horas uteis 
    hu = tempos[[i]][(length(tempos[[i]])-2)] - tempos[[i]][1]
    
    # calculo de volume
    vol = if(tempos[[i]][(length(tempos[[i]])-1)] == 1){
      tempos[[i]][length(tempos[[i]])]*cap_penta
    }else{
      tempos[[i]][length(tempos[[i]])]*cap_bi
    }
    
    # tempo medio de fila na carga e descarga
    fila_carga = round(sum(fila.carga[[i]]),2)
    
    fila_descarga = round(sum(fila.descarga[[i]]),2)
    
    # # cadencia carga - caminhao/hora
    # cadencia_carga_penta = (npenta/ngpenta)/(tempos[[ngpenta + 1]][3] - tempos[[1]][2])
    # 
    # cadencia_carga_bi = (nbi/ngbi)/(tempos[[length(tempos)]][3] - tempos[[(length(tempos)-ngbi)]][2])
    
    # definindo tipo
    if(i <= npenta){
      t = "B"
      tempos_penta = tempos[[i]]
      
    }else{
      t = "A"
      tempos_bi = tempos[[i]]
      
    }
    
    viagens = tempos[[i]][length(tempos[[i]])]
    
    aux = cbind(t,hu,vol,fila_carga,fila_descarga,viagens)
    
    # tabela resumo
    if(i == 1){
      penta = tempos_penta
      out = aux
      flag = T
    }else if(i > npenta & flag == T){
      
      bi = tempos_bi
      out = rbind(out,aux)
      flag = F
      
    }else if(i <= npenta){
      penta = rbind(penta,tempos_penta)
      out = rbind(out,aux)
    }else if(i > npenta &  flag == F){
      bi = rbind(bi,tempos_bi)
      out = rbind(out,aux)
      
    }
    
  }
  
  # nomeando tabela resumo
  colnames(out) = c("Tipo","Horas uteis","Volume","Fila Carga","Fila Descarga",
                    "Viagens")
  
  
  # nomeando penta e bi
  colnames(penta)[1] = "Saida"
  colnames(penta)[ncol(penta)] = "Ciclos"
  colnames(penta)[(ncol(penta)-1)] = "Tipo"
  
  op = c("Chegada Carga","Fim Carga","Chegada Descarga","Fim Descarga")
  
  colnames(penta)[2:(ncol(penta)-2)] = rep(op,penta[1,ncol(penta)])
  
  colnames(bi)[1] = "Saida"
  colnames(bi)[ncol(bi)] = "Ciclos"
  colnames(bi)[(ncol(bi)-1)] = "Tipo"
  
  colnames(bi)[2:(ncol(bi)-2)] = rep(op,bi[1,ncol(bi)])
  
  out = as.data.frame(out)
  tps = c("B","A")
  final = NULL
  # tabela final
  for(i in 1:2){
    
    temp = out[out$Tipo == tps[i],]
    Tipo = temp[1,1]
    
    if(i == 1){
      metavol = 6000#7200
    }else{
      metavol = 1500#7500
    }
    
    hubar = mean(as.numeric(temp$`Horas uteis`))
    MaxHU = max(as.numeric(temp$`Horas uteis`))
    MinHU = min(as.numeric(temp$`Horas uteis`))
    
    volbar_dia = mean(as.numeric(temp$Volume))
    voltot_dia = sum(as.numeric(temp$Volume))
    
    volbar_mes = volbar_dia*29
    voltot_mes = voltot_dia*29
    
    if(Tipo == "B"){
      nciclos = unique(as.numeric(temp$`Viagens`))
      viagens.total = unique(as.numeric(temp$`Viagens`))*npenta
      ntruck = npenta
    }else{
      nciclos = unique(as.numeric(temp$`Viagens`))
      viagens.total = unique(as.numeric(temp$`Viagens`))*nbi
      ntruck = nbi
    }
    
    filaCbar = mean(as.numeric(temp$`Fila Carga`))
    filaCtot = sum(as.numeric(temp$`Fila Carga`))
    filaDbar = mean(as.numeric(temp$`Fila Descarga`))
    filaDtot = sum(as.numeric(temp$`Fila Descarga`))
    
    aux = cbind(desv[dly],metavol,Tipo,hubar,MaxHU,MinHU,volbar_dia,voltot_dia,filaCbar,filaCtot,
                filaDbar,filaDtot,ntruck,viagens.total,nciclos)
    final = rbind(final,aux)
    
  }
  
  colnames(final)=c("Desvio","Meta de volume","Tipo","Media Horas Uteis","Maximo Horas Uteis",
                    "Minimo Horas Uteis","Volume Medio","Volume Total",
                    "Fila Carga Media","Fila Carga Total",
                    "Fila Descarga Media","Fila Descarga Total",
                    "Frota","Total Viagens","Ciclos")
  
  final_file[dly] = list(final)
  
}

# rm(list=setdiff(ls(), c("final_file")))
