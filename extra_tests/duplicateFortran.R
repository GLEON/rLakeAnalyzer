# Duplicate the output of the Fortran program to make it easy to compare.

duplicateFortran = function(dataname="97130187") {
  hyphenspacer = "------------------------------------------------------\n"
  equalsspacer = "======================================================\n"

  reporter = function(file,filename,parm,thres,by_s_m,nimax,smz,sms) {
    cat("\n",file=file)
    cat(sprintf("FILE NAME=  %-20s\n",filename),file=file)
    p = paste(parm,"PROFILE,")
    cat(sprintf("RESULT FOR %-22s ERR NORM=%12.5f\n",p,thres),file=file)
    cat(sprintf("MLD=%12.2f          MAX SEGMENTS=%3.0f\n",by_s_m,nimax),file=file)
    cat(hyphenspacer,file=file)
    # p = paste(spaces,parm,sep = "")
    cat(sprintf("NUMBER     DEPTH    %-20s\n",parm),file=file)
    cat(hyphenspacer,file=file)
    for(i in (1:(nimax+1))) {
      cat(sprintf("%4.0f%12.4f%12.4f\n",i,smz[i],sms[i]),file=file)
    }
    cat(equalsspacer,file=file)
  }


  # dataname = "97130187"
  filename =paste(dataname,".t11",sep="")
  # load("t11.rda")   # Same data as Fortran test
  t11 = as.data.frame( scan(file=filename,what=list(depth=0,temper=0,salinity=0,oxygen.sat=0,oxygen=0,density=0)))
  thres=.005; z0=2.5; zmax=140

  ofile <- file(paste(dataname,"-R.txt",sep=""), "w")
  # by_s_m = function(thres,z0,zmax,z,sigma)
  results = by_s_m( thres=thres, z0=z0,zmax=zmax, z=t11$depth, sigma=t11$density)
  # results = list(nimax=nimax,smz=smz,sms=sms,by_s_m=ss)
  reporter(ofile,filename,"DENSITY",thres,results$by_s_m,results$nimax,results$smz,results$sms)

  results = by_s_m( thres=thres, z0=z0,zmax=zmax, z=t11$depth, sigma=t11$temper)
  reporter(ofile,filename,"TEMPERATURE",thres,results$by_s_m,results$nimax,results$smz,results$sms)

  results = by_s_m( thres=thres, z0=z0,zmax=zmax, z=t11$depth, sigma=t11$salinity)
  reporter(ofile,filename,"SALINITY",thres,results$by_s_m,results$nimax,results$smz,results$sms)

  nimax = 5  # Number of segments fixed.

  results = by_s_m3( nr=nimax, z0=z0,zmax=zmax, z=t11$depth, sigma=t11$density)
  # results = list(nimax=nimax,smz=smz,sms=sms,by_s_m=ss)
  reporter(ofile,filename,"DENSITY",results$eps,results$by_s_m,nimax,results$smz,results$sms)

  results = by_s_m3( nr=nimax, z0=z0,zmax=zmax, z=t11$depth, sigma=t11$temper)
  reporter(ofile,filename,"TEMPERATURE",results$eps,results$by_s_m,nimax,results$smz,results$sms)

  results = by_s_m3( nr=nimax, z0=z0,zmax=zmax, z=t11$depth, sigma=t11$salinity)
  reporter(ofile,filename,"SALINITY",results$eps,results$by_s_m,nimax,results$smz,results$sms)

  close(ofile)
}
