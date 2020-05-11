quicksofa_simple <- function(
  ID = NULL,
  BPSysMin = NULL,
  RespRateMin = NULL,
  RespRateMax = NULL,
  Confusion = NULL,
  GCS = NULL
){
  
  # Quick and dirty quickSofa function without progress data structure
  
  # ID <- qs$input$patstuid
  # BPSysMin <- qs$input$sysbp.min_d0
  # RespRateMin <- qs$input$afrq.min_d0
  # RespRateMax <- qs$input$afrq.max_d0
  # Confusion <- qs$input$conf
  # GCS <- qs$input$gcs_d0
  
  dat <- data.table(
    id = ID,
    bp = BPSysMin,
    rrmin = RespRateMin,
    rrmax = RespRateMax,
    conf = Confusion,
    gcs = GCS
  )
  
  bp1 <- dat$bp
  myFilt <- is.na(bp1)
  bp1[myFilt] <- dat$bp[myFilt]
  bloodpressurePoint <- as.numeric(bp1 <= 100)
  hrp <- pmax(dat$rrmin, dat$rrmax, na.rm = T)
  myFilt <- is.na(hrp)
  dummy <- pmax(dat$rrmin, dat$rrmax, na.rm = T)
  hrp[myFilt] <- dummy[myFilt]
  highRespRatePoint <- as.numeric(hrp >= 22)
  verwirrt <- dat$conf
  myFilt <- is.na(verwirrt)
  verwirrt[myFilt] <- dat$conf[myFilt]
  verwirrt[is.na(verwirrt)] <- 0
  gcsPoint <- as.numeric(dat$gcs < 15 | verwirrt)
  dummy <- cbind(bloodpressurePoint, highRespRatePoint, gcsPoint)
  res <- apply(dummy, 1, function(x) sum(x, na.rm = T))
  res = data.table(qSOFA = res)
  res$PATSTUID = dat$id
  res <- data.table::setcolorder(res, neworder = c("PATSTUID", 
                                                   "qSOFA"))
  return(res$qSOFA)
}