check_injections<-function(u){
# just make true if it wasn't a ksv test
if (u$assay == "gain"){return(TRUE)}
# calc expected F0
F0hat <- ((u$O2_coefs$Ksv * pp::O2(37)) + 1) * u$O2_coefs$target
# calc percent error from expected F0
F0per00dif <- 100 * ((u$summary$F0 - F0hat) / F0hat)
# counts the number of wells that were greater than 10% from expected
inj <- sum(abs(F0per00dif >= 10))
# test to make sure 5% or less of wells were likely injection errors
(100 * (inj / length(F0per00dif))) <= 5
}
