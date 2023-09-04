

# funcoes

# tipo: 2 = bi
# tipo: 1 = penta

source("queue_Fuzzy.R")

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

# frota
npenta = 18
nbi = 21
# gruas carregamento para bitrem e pentatrem
ngpenta = 6; ngbi = 3
ngd = 3 # numero gruas descarregamento

# capacidade de carga liquida
cap_penta = 117
cap_bi = 37

# tempos
transito_penta = 2.2
transito_bi = 2

transito_cheio_penta = 2.8
transito_cheio_bi = 2.3

# tempo de carga
carga_penta = 1.1
carga_bi = 0.8

# tempo de descarga
descarga_penta = 1
descarga_bi = 0.6

rendimento.penta = c(transito_penta,carga_penta,transito_cheio_penta,descarga_penta)
rendimento.bi = c(transito_bi,carga_bi,transito_cheio_bi,descarga_bi)

# intervalo de tempo em que os caminhos inicial trabalho
# horas = seq(5,21,0.25)
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


#carga+desc: agendamento de caminhoes com gruas
queue_system = function(vehicle_type,crane_type,delay=0,type,temp=NULL){
  
  {
    
    if(type == "B"){ # penta
      
      npenta = vehicle_type
      ngpenta = crane_type
      
      temp_penta = matrix(unlist(lapply(1:npenta, function(j) tempo1[[j]])), ncol = 15, byrow = T)
      
      temp_penta = as.data.frame(temp_penta)
      
      names(temp_penta) = c("saida","chegada_carga","fim_carga","chegada_desc","fim_desc",
                            "chegada_carga","fim_carga","chegada_desc","fim_desc",
                            "chegada_carga","fim_carga","chegada_desc","fim_desc",
                            "tipo","ciclo")
      temp_penta=temp_penta[,-1]
      
      
    }else if(type == "A"){ # A - bitrem
      
      temp_penta = matrix(unlist(lapply((npenta+1):(npenta+nbi), function(j) tempo1[[j]])), ncol = 15, byrow = T)
      
      npenta = vehicle_type
      ngpenta = crane_type
      temp_penta = as.data.frame(temp_penta)
      
      names(temp_penta) = c("saida","chegada_carga","fim_carga","chegada_desc","fim_desc",
                            "chegada_carga","fim_carga","chegada_desc","fim_desc",
                            "chegada_carga","fim_carga","chegada_desc","fim_desc",
                            "tipo","ciclo")
      temp_penta=temp_penta[,-1]
      
      
    }else if(type == "unload"){
      
      temp_penta = temp
      
    }
    
    grua = queue_time = crane_prop = NULL
    
    # initial condition
    q = rep(0, ngpenta)
    crane = rep(0,ngpenta)
    # delay=2
    for(i in 1:temp_penta$ciclo[1]){
      
      if(i > 1){
        # decision at cycle i
        
        if(sum(q) == 0){
          g = as.data.frame(matrix(unlist(queue_time),ncol=ngpenta, byrow = T))
          q = colMeans(g)
        }
        
        p = sapply(1:ngpenta, function (y) crane_decisionMaker(qt = q[y], pcrane = crane[y]))
        p[which(is.na(p))] = min(p[!is.na(p)])
        
        if(length(unique(p)) == 1){
          p = round(runif(ngpenta,0.1,0.9),1)
        }
        
      }else{
        p = round(runif(ngpenta,0.1,0.9),1)
      }
      grua[i] = list(p)
      
      # min is better - min delay = max allocation
      grua_allocation = 1-grua[[i]]/sum(unique(grua[[i]]))
      aux=grua_allocation/sum(grua_allocation)
      
      # allocating proportion
      g_id = ceiling(npenta*aux)
      
      # checking number of wheelers per crane
      if(sum(g_id) > npenta){
        dif = sum(g_id) - npenta
        
        # if(max(g_id) >= dif){
          off = rep(1,dif)
          off = c(off,rep(0,(length(g_id)-length(off))))
          g_id = g_id - off
          # id_order = order(g_id,decreasing = T)[1:dif]
        }
      # else{
      #     id_order=which.max(g_id)
      #     g_id[id_order] = g_id[id_order] - dif
      #   }
      
      # sum(g_id)
      
      # proportion of truck per crane
      crane = round(g_id/sum(g_id),2)
      
      # potential trucks in the queue
      cycle = temp_penta[,(i+3*(i-1)):(i+3*i)]
      first_in = order(cycle[,3])
      # trucks in queue
      id_truck = first_in[(ngpenta+1):length(first_in)]
      
      # updating first in trucks when there is delay - random cycles
      # if(cycle_id_delay == i){
      #   joint[,(i+3*(i-1)):(i+3*i)] = joint[,(i+3*(i-1)):(i+3*i)] + delay
      #   cycle = joint[,(i+3*(i-1)):(i+3*i)]
      # }
      
      # potential crane with truck in queue
      cr = rep(which(g_id>1),(length(id_truck)-length(g_id)))
      
      for(j in 1:length(id_truck)){
        
        queue = cycle[first_in[j],4] - cycle[id_truck[j],3]
        
        if(queue <= 0){
          queue = 0
          q[cr[j]] = 0
        }
        
        extra = delay + queue
        
        temp_penta[id_truck[j],2:(ncol(temp_penta)-2)] = 
          temp_penta[id_truck[j],2:(ncol(temp_penta)-2)] + extra
        
        cycle[id_truck[j],2:ncol(cycle)] = cycle[id_truck[j],2:ncol(cycle)] + extra
        
        q[cr[j]] = q[cr[j]] + queue
        
        #     queue = cycle[cr[j],2] - cycle[id_truck[j],1]
        # 
        # if(queue <= 0){
        #   queue = 0
        #   q = c(0,0)
        # }
        # 
        # temp_penta[id_truck[j],2:(ncol(temp_penta)-2)] = temp_penta[id_truck[j],2:(ncol(temp_penta)-2)] + queue
        # # cycle[id_truck[j],2:ncol()] = cycle[id_truck[j],2:ncol(cycle)] + extra
        # 
        # q[cr[j]] = q[cr[j]] + queue
      }
      
      queue_time[i] = list(q)
      crane_prop[i] = list(crane)
    }
    
    # queue_penta = queue_time
    # crane_penta = crane_prop
  }
  
  out = list(queue_penta = queue_time, crane_penta = crane_prop, temp=temp_penta)
  
  return(out)
  
}

desv = c(0,1,2,3)
outa = outb = NULL
for(dly in 1:length(desv)){
  
  b = queue_system(vehicle_type = npenta, crane_type = ngpenta,
                     delay=desv[dly], type="B")
  
  a = queue_system(vehicle_type =  nbi, crane_type =  ngbi, 
                     delay = desv[dly], type="A")
  
  outb[dly] = list(b)
  outa[dly] = list(a)
  
}

# unloading ####################################################################
temp_a=sapply(outa,tail,1)
temp_b=sapply(outb,tail,1)

out_unload = NULL
for(sim in 1:length(outa)){
  # sim=1
  temp_aux = rbind(temp_a[[sim]], temp_b[[sim]])
  
  unload = queue_system(vehicle_type = (npenta+nbi), crane_type = ngd, type = "unload",
                temp = temp_aux)
  
  out_unload[sim] = list(unload)
  
}

# summarizing
for(i in 1:4){
  a = matrix(unlist(outa[[i]][1]), ncol=3, byrow = T)
  
  b = matrix(unlist(outb[[i]][1]), ncol=3, byrow = T) 
  
  unload = matrix(unlist(out_unload[[i]][1]), ncol=3, byrow = T) 

  
  desvio = switch(i,
         0,
         1,
         2,
         3)
  
  h_a = data.frame(outa[[i]][3])
  media_h_a = mean(h_a[,12])
  max_h_a = max(h_a[,12])
  min_h_a = min(h_a[,12])
  queue_mean_a = sum(a)/21
  queue_tot_a = sum(a)

  h_b = data.frame(outb[[i]][3])
  media_h_b = mean(h_b[,12])
  max_h_b = max(h_b[,12])
  min_h_b = min(h_b[,12])
  queue_mean_b = sum(b)/18
  queue_tot_b = sum(b)

  h = data.frame(out_unload[[i]][3])
  media_h = mean(h[,12])
  max_h = max(h[,12])
  min_h = min(h[,12])
  queue_mean = sum(unload)/18
  queue_tot = sum(unload)

  summary_a = cbind(2,desvio,media_h_a,max_h_a,min_h_a,queue_mean_a,queue_tot_a)
  summary_b = cbind(1,desvio,media_h_b,max_h_b,min_h_b,queue_mean_b,queue_tot_b)
  summary = cbind(3,desvio,media_h,max_h,min_h,queue_mean,queue_tot)
    
  if(i == 1){
    sum_a = summary_a
    sum_b = summary_b
    unload_out = summary
  }else{
    sum_a = rbind(sum_a,summary_a)
    sum_b = rbind(sum_b,summary_b)
    unload_out = rbind(unload_out, summary)
  }

}

out_fuzzy = as.data.frame(rbind(sum_a, sum_b, unload_out))
out_fuzzy$V1=ifelse(out_fuzzy$V1==1,"A",ifelse(out_fuzzy$V1==2,"B","Unload"))
names(out_fuzzy)=c("tipo","desvio","media hora util","max hora util","min hora util","media fila","total fila")


# running simulador de fila for the deterministic model
source("simulador de fila.R")

for(i in 1:length(final_file)){
  
  pp = final_file[[i]]
  
  aux = as.data.frame(matrix(unlist(pp), nrow=2))
  
  if(i == 1){
    out_lp = aux
  }else{
    out_lp = rbind(out_lp,aux)
  }

}

names(out_lp) = names(as.data.frame(final_file[[1]]))
out_lp = out_lp[,-15]
rm(list=setdiff(ls(), c("out_unload","outa","outb","final_file","out_fuzzy","out_lp")))

library(openxlsx)
getwd()

wb <- createWorkbook()

addWorksheet(wb=wb, sheetName = "fuzzy resumo")
writeData(wb, x = out_fuzzy, sheet = "fuzzy resumo")

addWorksheet(wb=wb, sheetName = "lp resumo")
writeData(wb, x = out_lp, sheet = "lp resumo")

saveWorkbook(wb, "resultados_fila.xlsx", overwrite = TRUE)


# # pentatrem (test) - vehicle B #################################################
# {
#   temp_penta = matrix(unlist(lapply(1:npenta, function(j) tempo1[[j]])), ncol = 15, byrow = T)
#   temp_penta = as.data.frame(temp_penta)
#   names(temp_penta) = c("saida","chegada_carga","fim_carga","chegada_desc","fim_desc",
#                         "chegada_carga","fim_carga","chegada_desc","fim_desc",
#                         "chegada_carga","fim_carga","chegada_desc","fim_desc",
#                         "tipo","ciclo")
#   temp_penta=temp_penta[,-1]
#   grua = queue_time = crane_prop = NULL
#   
#   # initial condition
#   q = rep(0, 6)
#   crane = rep(0,6)
#   delay=0
#   for(i in 1:temp_penta$ciclo[1]){
#     
#     if(i > 1){
#       # decision at cycle i
#       
#       if(sum(q) == 0){
#         g = as.data.frame(matrix(unlist(queue_time),ncol=ngpenta, byrow = T))
#         q = colMeans(g)
#       }
#       
#       p = sapply(1:ngpenta, function (y) crane_decisionMaker(qt = q[y], pcrane = crane[y]))
#       p[which(is.na(p))] = min(p[!is.na(p)])
#     }else{
#       p = c(0.3,0.4,0.5,0.2,0.1,0.2)
#     }
#     grua[i] = list(p)
#     
#     # min is better - min delay = max allocation
#     grua_allocation = 1-grua[[i]]/sum(unique(grua[[i]]))
#     aux=grua_allocation/sum(grua_allocation)
#     
#     # allocating proportion
#     g_id = ceiling(npenta*aux)
#     
#     # checking number of wheelers per crane
#     if(sum(g_id) > npenta){
#       df = sum(g_id) - npenta
#       id_order = order(g_id,decreasing = T)[df]
#       g_id[id_order] = g_id[id_order] - df
#     }
#     
#     # sum(g_id)
#     
#     # proportion of truck per crane
#     crane = round(g_id/sum(g_id),2)
#     
#     # potential trucks in the queue
#     cycle = temp_penta[,(i+3*(i-1)):(i+3*i)]
#     first_in = order(cycle[,3])
#     # trucks in queue
#     id_truck = first_in[(ngpenta+1):length(first_in)]
#     
#     # updating first in trucks when there is delay - random cycles
#     # if(cycle_id_delay == i){
#     #   joint[,(i+3*(i-1)):(i+3*i)] = joint[,(i+3*(i-1)):(i+3*i)] + delay
#     #   cycle = joint[,(i+3*(i-1)):(i+3*i)]
#     # }
#     
#     # potential crane with truck in queue
#     cr = rep(which(g_id>1),(length(id_truck)-length(g_id)))
#     
#     for(j in 1:length(id_truck)){
#       
#       queue = cycle[first_in[j],4] - cycle[id_truck[j],3]
#       
#       if(queue <= 0){
#         queue = 0
#         q[cr[j]] = 0
#       }
#       
#       extra = delay + queue
#       
#       temp_penta[id_truck[j],3:(ncol(temp_penta)-2)] = 
#         temp_penta[id_truck[j],3:(ncol(temp_penta)-2)] + extra
#       
#       cycle[id_truck[j],3:ncol(cycle)] = cycle[id_truck[j],3:ncol(cycle)] + extra
#       
#       q[cr[j]] = q[cr[j]] + queue
#       
#       #     queue = cycle[cr[j],2] - cycle[id_truck[j],1]
#       # 
#       # if(queue <= 0){
#       #   queue = 0
#       #   q = c(0,0)
#       # }
#       # 
#       # temp_penta[id_truck[j],2:(ncol(temp_penta)-2)] = temp_penta[id_truck[j],2:(ncol(temp_penta)-2)] + queue
#       # # cycle[id_truck[j],2:ncol()] = cycle[id_truck[j],2:ncol(cycle)] + extra
#       # 
#       # q[cr[j]] = q[cr[j]] + queue
#     }
#     
#     queue_time[i] = list(q)
#     crane_prop[i] = list(crane)
#   }
#   
#   queue_penta = queue_time
#   crane_penta = crane_prop
# }
# 
# # bitrem (test) - vehicle A ####################################################
# {
#   temp_bi = matrix(unlist(lapply((npenta+1):(npenta+nbi), function(j) tempo1[[j]])), ncol = 15, byrow = T)
#   temp_bi = as.data.frame(temp_bi)
#   names(temp_bi) = c("saida","chegada_carga","fim_carga","chegada_desc","fim_desc",
#                      "chegada_carga","fim_carga","chegada_desc","fim_desc",
#                      "chegada_carga","fim_carga","chegada_desc","fim_desc",
#                      "tipo","ciclo")
#   temp_bi=temp_bi[,-1]
#   grua = queue_time = crane_prop = NULL
#   
#   # initial condition
#   q = rep(0,ngbi)
#   crane = rep(0,ngbi)
#   # delay=0
#   for(i in 1:temp_bi$ciclo[1]){
#     
#     # decision at cycle i
#     p = sapply(1:ngbi, function (y) crane_decisionMaker(qt = q[y], pcrane = crane[y]))
#     grua[i] = list(p)
#     
#     # min is better - min delay = max allocation
#     grua_allocation = 1-grua[[i]]/sum(grua[[i]])
#     
#     # allocating proportion
#     g_id = ceiling(nbi*grua_allocation)
#     
#     # checking number of wheelers per crane
#     if(sum(g_id) > nbi){
#       df = sum(g_id) - nbi
#       id_order = order(g_id,decreasing = T)[df]
#       g_id[id_order] = g_id[id_order] - df
#     }
#     
#     # proportion of truck per crane
#     crane = round(g_id/sum(g_id),2)
#     
#     # truck in queue
#     id_truck = nbi:1
#     id_truck = id_truck[1:(nbi-ngbi)]
#     
#     # crane with truck in queue
#     cr = which(g_id>1)
#     
#     # read info from simulation
#     cycle = temp_bi[,(i+3*(i-1)):(i+3*i)]
#     
#     # updating times per wheeler
#     for(j in 1:length(id_truck)){
#       
#       queue = cycle[first_in[j],4] - cycle[id_truck[j],3]
#       
#       if(queue <= 0){
#         queue = 0
#         q = rep(0,ngbi)
#       }
#       
#       temp_bi[id_truck[j],3:(ncol(temp_bi)-2)] = 
#         temp_bi[id_truck[j],3:(ncol(temp_bi)-2)] + queue
#       
#       cycle[id_truck[j],3:ncol(cycle)] = cycle[id_truck[j],3:ncol(cycle)] + queue
#       
#       q[cr[j]] = q[cr[j]] + queue
#       
#       
#       # queue = cycle[cr[j],2] - cycle[id_truck[j],1]
#       # 
#       # extra = round(queue + delay,2)
#       # 
#       # if(extra == 0){
#       #   q = c(0,0)
#       # }
#       # 
#       # temp_bi[id_truck[j],2:(ncol(temp_bi)-2)] = temp_bi[id_truck[j],2:(ncol(temp_bi)-2)] + extra
#       # # cycle[id_truck[j],2:ncol()] = cycle[id_truck[j],2:ncol(cycle)] + extra
#       # 
#       # q[cr[j]] = q[cr[j]] + extra
#     }
#     
#     queue_time[i] = list(q)
#     crane_prop[i] = list(crane)
#   }
#   
#   queue_bi = queue_time
#   crane_bi = crane_prop
# }
# 
# # unloading script - test #######################################
# {
#   
#   grua = queue_time = crane_prop = NULL
#   
#   delay = 0
#   
#   # initial condition
#   # trucks in unloading
#   
#   temp_b=sapply(outa,tail,1)
#   temp_p=sapply(outb,tail,1)
#   
#   joint = rbind(temp_penta[[1]], temp_bi[[1]])
#   
#   # delay in the entire day (all cycles)
#   # joint[,-c(13,14)] = joint[,-c(13,14)] + delay
#   
#   q = rep(0,ngd)
#   crane = rep(0,ngd)
#   
#   # setting random delay
#   # cycle_id_delay = rdunif(n = 1, b = 1, a = 3)
#   
#   for(i in 1:max(temp_bi$ciclo, temp_penta$ciclo)){
#     
#     # decision at cycle i
#     p = sapply(1:ngd, function (y) crane_decisionMaker(qt = q[y], pcrane = crane[y]))
#     grua[i] = list(p)
#     
#     # min is better - min delay = max allocation
#     grua_allocation = 1-grua[[i]]/sum(grua[[i]])
#     aux = grua_allocation/sum(grua_allocation)
#     # allocating proportion
#     g_id = ceiling(nrow(joint)*aux)
#     
#     # checking number of wheelers per crane
#     if(sum(g_id) > ngd){
#       df = sum(g_id) - nrow(joint)
#       id_order = order(g_id,decreasing = T)[df]
#       g_id[id_order] = g_id[id_order] - df
#     }
#     
#     # proportion of truck per crane
#     crane = round(g_id/sum(g_id),2)
#     
#     # potential trucks in the queue
#     cycle = joint[,(i+3*(i-1)):(i+3*i)]
#     first_in = order(cycle[,3])
#     id_truck = first_in[(ngd+1):length(first_in)]
#     
#     # updating first in trucks when there is delay - random cycles
#     # if(cycle_id_delay == i){
#     #   joint[,(i+3*(i-1)):(i+3*i)] = joint[,(i+3*(i-1)):(i+3*i)] + delay
#     #   cycle = joint[,(i+3*(i-1)):(i+3*i)]
#     # }
#     
#     # potential crane with truck in queue
#     cr = rep(which(g_id>1),(length(id_truck)-length(g_id)))
#     
#     # updating times per wheeler
#     for(j in 1:length(id_truck)){
#       
#       queue = cycle[first_in[j],4] - cycle[id_truck[j],3]
#       
#       if(queue <= 0){
#         queue = 0
#         q = rep(0,ngd)
#       }
#       
#       joint[id_truck[j],3:(ncol(joint)-2)] = joint[id_truck[j],3:(ncol(joint)-2)] + queue
#       cycle[id_truck[j],3:ncol(cycle)] = cycle[id_truck[j],3:ncol(cycle)] + queue
#       
#       q[cr[j]] = q[cr[j]] + queue
#     }
#     
#     queue_time[i] = list(q)
#     crane_prop[i] = list(crane)
#   }
#   
#   queue_desc = queue_time
#   crane_desc = crane_prop
#   
# }
# 
# 





