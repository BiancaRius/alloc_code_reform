program test2
    use establishment
    use types
    use FPC
    
    
    implicit none
    

    real(r_8) :: est_pls2
    real(r_8) :: FPC_total_gc2
    real(r_8) :: FPC_ind2
    real(r_8) :: FPC_pls2
    
    !!!!!!! ALIVE PLSs
    ! real(r_8) :: npls_alive2 = 1.0D0


    !C that enters in the logic (kgC/m2) for each PLS
    real(r_8) :: cleaf_pls_in  = 1.0D0
    real(r_8) :: csap_pls_in   = 1.0D0
    real(r_8) :: cheart_pls_in = 10.0D0
    real(r_8) :: croot_pls_in  = 1.0D0



    !density that enters in the logic (ind/m2) for each PLS
    real(r_8) :: dens_pls_in = 10. 
    
    !C that is an output from the logic (kgC/m2) for each PLS
    real(r_8) :: cleaf_pls_out  
    real(r_8) :: csap_pls_out   
    real(r_8) :: cheart_pls_out 
    real(r_8) :: croot_pls_out  

    !density that is an output from the logic (ind/m2) for each PLS
    real(r_8) :: dens_pls_out
    
   

        call gc_occupation(dens_pls_in,cleaf_pls_in,csap_pls_in, cheart_pls_in,croot_pls_in,&
           cleaf_pls_out, csap_pls_out, cheart_pls_out, croot_pls_out, dens_pls_out, FPC_total_gc2, FPC_ind2, FPC_pls2)
        
        !UPDATE VARIABLES
           !cleaf_pls_in = cleaf_pls_out
         

end program test2