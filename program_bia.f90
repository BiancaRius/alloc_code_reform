program program2
    use constants
    use types
    use allocation
    
    implicit none
    integer(i_4) :: i
    real(r_8) :: leaf_in  = 1.5 !kgC/m2 initial C leaf input 
    real(r_8) :: root_in  = 1.2 !kgC/m2 initial C root input
    real(r_8) :: sap_in = 1000.
    real(r_8) :: heart_in = 500.
    real(r_8) :: storage_in = 50.
    real(r_8) :: bminc_in = 3.5!0.0004 !carbon (NPP) available to be allocated
                                !basically NPPt - NPPt-1. NPP accumulated in the year/month/day
                                !gc/ind/time_step

    real(r_8) :: dens_in = 10. !ind/m2 initial density of individuals 

    real(r_8) :: leaf_out
    real(r_8) :: root_out
    real(r_8) :: sap_out
    real(r_8) :: heart_out
    real(r_8) :: storage_out



    ! sap_in   = 87.!0.05*wood_in
    ! heart_in = 324.!0.95*wood_in

    do i = 1, 20
      print*, ' '
      print*,'STEP', i
      call alloc(leaf_in, root_in, sap_in, heart_in, storage_in, bminc_in,dens_in,&
          leaf_out, root_out, sap_out, heart_out, storage_out)

          !update variables
        leaf_in = leaf_out
        root_in = root_out
        sap_in = sap_out
        storage_in = storage_out
      
      
        ! ! print*, 'bminc ======', bminc_in/dens_in, i
        PRINT*, '____________________________________'
        print*,'leaf updt===========', leaf_in,  i
        print*,'root updt===========', root_in,  i
        print*,'sap  updt===========', sap_in,  i
        print*,'storage  updt===========', storage_in,  i

        PRINT*, '____________________________________'

    enddo
        
       

end program program2
