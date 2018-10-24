#Generate color palette adaptively
ColorPaletteGenerate <- function(ColorBase,n, Scenario = NULL){
  if (is.null(Scenario)){
  rgbs <- col2rgb(brewer.pal(9,ColorBase)[2:5])
  newRange <- t(apply(rgbs-rgbs[,4],1,range)) * n/9 + rgbs[,4]
  RGBv <- round(apply(newRange, 1, function(x) seq(min(x), max(x), length.out = n)))
      if (class(RGBv)!='numeric'){
        ReplineColors <-paste0('rgb(', apply(RGBv, 1, function(x) paste(x, collapse = ',')),')')} else{
        ReplineColors <- paste0('rgb(',paste(RGBv, collapse = ',') ,')')}
  } else if (Scenario=='Adverse') {
    rgbs <- col2rgb(brewer.pal(9,ColorBase)[5:8])
    newRange <- t(apply(rgbs-rgbs[,4],1,range)) * n/9 + rgbs[,4]
    RGBv <- round(apply(newRange, 1, function(x) seq(min(x), max(x), length.out = n)))
    if (class(RGBv)!='numeric'){
      # RGBv[,1] <-RGBv[,1] - 20;RGBv[,2] <-RGBv[,2] + 20;RGBv[,3] <-RGBv[,3] + 20
      ReplineColors <-paste0('rgb(', apply(RGBv, 1, function(x) paste(x, collapse = ',')),')')} else{
        # RGBv <- RGBv + c(-20,20,20)
        ReplineColors <- paste0('rgb(',paste(RGBv, collapse = ',') ,')')} 
  } else if (Scenario=='Baseline') {
    rgbs <- col2rgb(brewer.pal(9,ColorBase)[1:4])
    newRange <- t(apply(rgbs-rgbs[,4],1,range)) * n/9 + rgbs[,4]
    RGBv <- round(apply(newRange, 1, function(x) seq(min(x), max(x), length.out = n)))
    if (class(RGBv)!='numeric'){
      # RGBv[,1] <-RGBv[,1] - 20;RGBv[,2] <-RGBv[,2] + 20;RGBv[,3] <-RGBv[,3] + 20
      ReplineColors <-paste0('rgb(', apply(RGBv, 1, function(x) paste(x, collapse = ',')),')')} else{
        # RGBv <- RGBv + c(-20,20,20)
        ReplineColors <- paste0('rgb(',paste(RGBv, collapse = ',') ,')')} 
  }
}