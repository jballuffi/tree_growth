#function that collects R2s from list of model
#use with lapply on list of models

collectR2 <- function(model) {
  #collect R2s
  rsqOut <- data.table(r.squaredGLMM(model))
  rsqOut<- round(rsqOut, 2)
  #return each datatable binded together by row
  return(data.table(rsqOut))
}
