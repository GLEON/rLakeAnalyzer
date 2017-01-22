# Duplicate the output of the Fortran program to make it easy to compare.

duplicateFortran = function() {
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
    for(i in (1:(results$nimax+1))) {
      cat(sprintf("%4.0f%12.4f%12.4f\n",i,smz[i],sms[i]),file=file)
    }
    cat(equalsspacer,file=file)
  }

  load("data/t11.rda")   # Same data as Fortran test

  thres=.005; z0=2.5; zmax=140


  ofile <- file("DuplicatedReport.txt", "w")
  # by_s_m = function(thres,z0,zmax,z,sigma)
  results = by_s_m( thres=thres, z0=z0,zmax=zmax, z=t11$depth, sigma=t11$density)
  # results = list(nimax=nimax,smz=smz,sms=sms,by_s_m=ss)
  reporter(ofile,"97130187.t11","DENSITY",thres,results$by_s_m,results$nimax,results$smz,results$sms)

  results = by_s_m( thres=thres, z0=z0,zmax=zmax, z=t11$depth, sigma=t11$temper)
  reporter(ofile,"97130187.t11","TEMPERATURE",thres,results$by_s_m,results$nimax,results$smz,results$sms)

  results = by_s_m( thres=thres, z0=z0,zmax=zmax, z=t11$depth, sigma=t11$salinity)
  reporter(ofile,"97130187.t11","SALINITY",thres,results$by_s_m,results$nimax,results$smz,results$sms)

  close(ofile)

  # write(2,*)
  # write(2,200)name
  # write(2,2011)thres
  # write(2,2020)res1,nimax
  # write(2,2041)
  # write(2,2031)
  # write(2,2041)
  # do i=1,nimax+1
  # write(2,2030)i,smz(i),sms(i)
  # end do
  # write (2,2040)
  # 200   format('FILE NAME=  ',a20)
  # 2011  format('RESULT FOR DENSITY PROFILE,       ERR NORM=',f12.5)
  # 2012  format('RESULT FOR TEMPERATURE PROFILE,   ERR NORM=',f12.5)
  # 2013  format('RESULT FOR SALINITY PROFILE,      ERR NORM=',f12.5)
  # 2020  format('MLD=',F12.2,10x,'MAX SEGMENTS=',i3)
  # 2031  format('NUMBER     DEPTH     DENSITY')
  # 2032  format('NUMBER     DEPTH   TEMPERATURE')
  # 2033  format('NUMBER     DEPTH    SALINITY')
  # 3011  format('RESULT FOR DENSITY PROFILE,       ',i3,' SEGMENTS')
  # 3012  format('RESULT FOR TEMPERATURE PROFILE,   ',i3,' SEGMENTS')
  # 3013  format('RESULT FOR SALINITY PROFILE,      ',i3,' SEGMENTS')
  # 3020  format('MLD=',F12.2,10x,'ERROR NORM=',f12.5)
  #
  # 2030  format(i4,2f12.4)
  # 2040  format('======================================================')
  # 2041  format('------------------------------------------------------')


# test_that("Run, compare temperature results with Fortran.", {
#   # by_s_m = function(thres,z0,zmax,z,sigma)
#   results = by_s_m( thres=.005, z0=2.5,zmax=140, z=t11$depth, sigma=t11$temper)
#   # results = list(nimax=nimax,smz=smz,sms=sms,by_s_m=ss)
#
#   expect_equal( results$nimax, TemperatureResults$nimax )
#   expect_equal( results$by_s_m, TemperatureResults$result )
#
#   expect_lt( error(results$smz,TemperatureResults$smz), maxerror )
#   expect_lt( error(results$sms,TemperatureResults$sms), maxerror )
# })
#
# test_that("Run, compare salinity results with Fortran.", {
#   # by_s_m = function(thres,z0,zmax,z,sigma)
#   results = by_s_m( thres=.005, z0=2.5,zmax=140, z=t11$depth, sigma=t11$salinity)
#   # results = list(nimax=nimax,smz=smz,sms=sms,by_s_m=ss)
#
#   expect_equal( results$nimax, SalinityResults$nimax )
#   expect_equal( results$by_s_m, SalinityResults$result )
#
#   expect_lt( error(results$smz,SalinityResults$smz), maxerror )
#   expect_lt( error(results$sms,SalinityResults$sms), maxerror )
# })
}
