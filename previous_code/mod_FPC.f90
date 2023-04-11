!Este módulo calcula as variáveis para indivíduos médios de cada PLS
!Designa-se também a calcular o FPC (foliar projective cover) através
!da estruturação alométrica dos PLSs. Além disso, calcula a mortalidade por espaço,
!ou seja, considerando uma área disponível de 1ha, os PLSs lenhosos só podem ocupar
!95% desta área. Caso ultrapassem esse montante, haverá penalização (redução na densidade 
!de indivíduos) que impactará os compartimentos de carbono. Caso não ultrapassem, será 
!possível que novos indivíduos se estabeleçam. Tais indivíduos são chamados de "sapling",
!apresentam alometria própria, mas não há cohort, de modo que ao incorporá-los na população do 
!PLSs, deve haver um ajustamento no balanço de carbono, o "shrink", que representa uma reestruturação
!dos indivíduos médios.
!A grande maioria dos cálculos aqui é feita para o indivíduo médio, portanto, a maioria das variáveis são
!divididas pela densidade (indivíduos/m²). Pools são usados em gC.
!Este código é baseado principalmente no modelo LPJ (Sitch et al 2003 e Smith et al 2001) e no código
!do LPJ-MLFire

module FPC


    use types
    use params
    use establishment
    

implicit none

private

public :: gc_occupation

contains

    subroutine gc_occupation(dens_pls_old, cleaf_pls_old, csap_pls_old, cheart_pls_old, croot_pls_old,&
        cleaf_pls_new, csap_pls_new, cheart_pls_new, croot_pls_new, dens_pls_new, FPC_total_gc, FPC_ind, FPC_pls)
        
        !VARIABLE INPUTS
        real(r_8), intent(in) :: dens_pls_old  !density (ind/m2) previous the mortality
        
        real(r_8), intent(in) :: cleaf_pls_old !carbon in leaf (kgC/m2) previous the mortality
        real(r_8), intent(in) :: csap_pls_old !carbon in sapwood (kgC/m2) previous the mortality
        real(r_8), intent(in) :: cheart_pls_old !carbon in heartwood (kgC/m2) previous the mortality
        real(r_8), intent(in) :: croot_pls_old !carbon in root (kgC/m2) previous the mortality


        !VARIABLES OUTPUTS 
        real(r_8), intent(out) :: FPC_total_gc !(m2) total FPC in a grid cell considering all PLS
        real(r_8), intent(out) :: FPC_ind !(m2)avg individual FPC
        real(r_8), intent(out) :: FPC_pls !(m2) FPC for each PLS

        real(r_8), intent(out) :: cleaf_pls_new !carbon in leaf (kgC/m2) after mortality
        real(r_8), intent(out) :: csap_pls_new !carbon in sapwood (kgC/m2) after the mortality
        real(r_8), intent(out) :: cheart_pls_new !carbon in heartwood (kgC/m2) after the mortality
        real(r_8), intent(out) :: croot_pls_new !carbon in root (kgC/m2) after the mortality

        real(r_8), intent(out) :: dens_pls_new
        
        !INTERNAL VARIABLES
        !!!Structuring variables
        !!!!!!!!!!!!!!!!Must come from allometry, provisorialy will have a specified value
        real(r_8) :: diam_pls
        real(r_8) :: crown_area_pls
        real(r_8) :: lai_pls
        real(r_8) :: height_pls
        real(r_8) :: npls_alive
        real(r_8) :: est_pls

        !gc occupation variables
        real(r_8) :: exc_area_gc
        
        real(r_8) :: nind_kill_FPC !number of avg ind. that will die due to (ind/m2) excedent area occupation
        real(r_8) :: carbon_increment !carbon increment from a time step to the next (gC)
        real(r_8) :: cleaf_total_pls  !(gC) total C in leaf for each pls considering the density
        real(r_8) :: csap_total_pls   !(gC) total C in sapwood for each pls considering the density
        real(r_8) :: cheart_total_pls !(gC) total C in heartwood for each pls considering the density
        real(r_8) :: croot_total_pls  !(gC) total C in roots for each pls considering the density

        real(r_8) :: spec_leaf
        real(r_8) :: wood_density
        
        real(r_8) :: mort_perc

        real(r_8) :: res_time_leaf
        real(r_8) :: res_time_sap 
        real(r_8) :: res_time_heart 
        real(r_8) :: res_time_root

        !(gC) Carbon on plant compartments after establishment and shrink process
        real(r_8) :: cleaf_est 
        real(r_8) :: csap_est 
        real(r_8) :: cheart_est 
        real(r_8) :: croot_est 

        real(r_8) :: dens_est !density after establishment and shrink process

        !(gC) Carbon on plant compartments after applying mortality percentage 
        real(r_8) :: cleaf_mort 
        real(r_8) :: csap_mort 
        real(r_8) :: cheart_mort 
        real(r_8) :: croot_mort 

        real(r_8) :: dens_mort !density after applying mortality percentage


        !(gC) Carbon on plant after loosing C by tissue turnover
        real(r_8) :: cleaf_turn 
        real(r_8) :: csap_turn 
        real(r_8) :: cheart_turn 
        real(r_8) :: croot_turn

       

        !initializing variables
        FPC_total_gc = 0.0D0
        FPC_ind = 0.0D0
        FPC_pls = 0.0D0
        diam_pls = 0.0D0
        crown_area_pls = 0.0D0
        lai_pls = 0.0D0
        height_pls = 0.0D0
        cleaf_total_pls = 0.0D0
        csap_total_pls = 0.0D0
        cheart_total_pls = 0.0D0
        croot_total_pls = 0.0D0

        nind_kill_FPC = 0.0D0 !used when the total gc is not gt then gc_max

        cleaf_est = 0.0D0
        csap_est = 0.0D0
        cheart_est = 0.0D0
        croot_est = 0.0D0
        dens_est = 0.0D0

        cleaf_mort = 0.0D0
        csap_mort = 0.0D0
        cheart_mort = 0.0D0
        croot_mort = 0.0D0
        dens_mort = 0.0D0

        cleaf_turn = 0.0D0
        csap_turn = 0.0D0
        cheart_turn = 0.0D0
        croot_turn = 0.0D0

    !!!!!POSSIBLE INPUTS
        diam_pls = 100.
        crown_area_pls = 100. 
        lai_pls = 100.
        height_pls = 100.
        carbon_increment = 10.
        
        spec_leaf = 10.
        wood_density = 10. !CHECK! PRECISA SER TRANSFORMADO

        res_time_leaf = 2.
        res_time_sap = 5.
        res_time_heart = 1.
        res_time_root = 2.

    !__________________________________
    !Calculating total C for compartments considering density (gC)
    ! (*1000) transforms from kgC to gC
        
        cleaf_total_pls  = (cleaf_pls_old * 1000.) / dens_pls_old
        csap_total_pls   = (csap_pls_old * 1000.) / dens_pls_old
        cheart_total_pls = (cheart_pls_old * 1000.) / dens_pls_old
        croot_total_pls  = (croot_pls_old * 1000.) / dens_pls_old

        print*,'cleaf old', cleaf_total_pls

    !__________________________________
    !Calculating Foliage Projective Cover of average individual(FPC_ind) and Fractional
    !Projective cover for PLS (FPC_pls2)
        FPC_ind = (1 - (exp (-0.5 * lai_pls)))
    
        FPC_pls = (crown_area_pls * dens_pls_old) * FPC_ind 
        
!!!!!!!!!!!!!!!!!!calculo FPC_total_gc é feito a partir do acúmulo de todos os PLS.
        !!!!!!!!!!por enquanto trabalhando com 1

        FPC_total_gc = 1000.

        npls_alive = 10.

    !Verify if total FPC is gt or lt 95% of grid cell area
        if (FPC_total_gc.gt.gc_area_95) then
            print*, 'gt'

            call exc_area(dens_pls_old, FPC_total_gc, FPC_pls, exc_area_gc, nind_kill_FPC)
            ! print*, exc_area_gc

            cleaf_est  = cleaf_total_pls
            csap_est   = csap_total_pls
            cheart_est = cheart_total_pls
            croot_est  = croot_total_pls
            dens_est   = dens_pls_old


        else
            print*, 'lt'

            call establish(npls_alive,FPC_total_gc,est_pls)

            call shrink(cleaf_total_pls, csap_total_pls, cheart_total_pls, croot_total_pls,&
                est_pls, dens_pls_old,& 
                cleaf_est, csap_est, cheart_est, croot_est, dens_est)            
        
        endif
    
        !calculates mortality due to gc occupation, greff, and wood density
        !!CHECCCCKKK if you need to use dens old or dens est/ cleaf total or cleaf est
        call mortality(dens_pls_old, nind_kill_FPC, carbon_increment, cleaf_total_pls, spec_leaf,&
            wood_density, mort_perc)

        !Changing carbon pools due to mort_perc
        cleaf_mort  = cleaf_est * mort_perc
        csap_mort   = csap_est * mort_perc
        cheart_mort = cheart_est * mort_perc
        croot_mort  = croot_est * mort_perc

        !Changing density due to mort_perc
        dens_mort = dens_est * mort_perc

        !lost of C due to turnover
           
        cleaf_turn = cleaf_mort - (cleaf_mort / res_time_leaf)

        print*, cleaf_turn

        csap_turn = csap_mort - (csap_mort /res_time_sap)

        !!!!the dead sapwood is added to heartwood
        cheart_turn = (cheart_mort - (cheart_mort / res_time_heart)) + (csap_mort / res_time_sap)

        croot_turn = croot_est - (croot_turn / res_time_root)

        !turn into kgC/m2

        cleaf_pls_new = cleaf_turn * dens_mort

        csap_pls_new = csap_turn * dens_mort

        cheart_pls_new = cheart_turn * dens_mort

        croot_pls_new = croot_turn * dens_mort

        dens_pls_new = dens_mort

        print*, 'cleaf new', cleaf_pls_new, cleaf_turn, dens_mort


    end subroutine gc_occupation

    subroutine exc_area(dens_pls_old, FPC_total_gc, FPC_pls, exc_area_gc, nind_kill_FPC)
        !VARIABLES INPUTS
        real(r_8), intent(in) :: FPC_total_gc !(m2) total FPC in a grid cell considering all PLSs
        real(r_8), intent(in) :: FPC_pls !(m2) FPC for each PLS
        real(r_8), intent(in) :: dens_pls_old


        !VARIABLES OUTPUTS
        real(r_8), intent(out) :: exc_area_gc !(m2) excendent in area for a gc
        real(r_8), intent(out) :: nind_kill_FPC !number of avg ind. that will die due to (ind/m2) excedent area occupation

        !INTERNAL VARIABLES
        real(r_8) :: FPC_dec_pls
        
        !initializing variables
        exc_area_gc = 0.0D0
        FPC_dec_pls = 0.0D0
        nind_kill_FPC = 0.0D0

        ! Excedent area           
        exc_area_gc = FPC_total_gc - gc_area_95

        !FPC decay to fit the 95% of gc occupation
        FPC_dec_pls = min(FPC_pls, exc_area_gc * (FPC_pls/FPC_total_gc))
        
        !kill ind due to exc are
        nind_kill_FPC = (dens_pls_old * FPC_dec_pls) / FPC_pls

    end subroutine exc_area

    subroutine mortality(dens_pls_old, nind_kill_FPC,carbon_increment,cleaf_total_pls, spec_leaf,&
        wood_density, mort_perc)
        !calculates mortality (due to gc occupation, greff, and wood density) and
        !########################carbon loss due to residence time
        

        !VARIABLES INPUTS
        real(r_8), intent(in) :: nind_kill_FPC !number of avg ind. (ind/m2 that will die due to excedent area occupation
        real(r_8), intent(in) :: carbon_increment !carbon increment from a time step to the next (gC)
        real(r_8), intent(in) :: cleaf_total_pls !Cleaf before mortality
        real(r_8), intent(in) :: spec_leaf !specific leaf area (m2/gC)
        real(r_8), intent(in) :: wood_density !gC/m3 - wood density
        real(r_8), intent(in) :: dens_pls_old

        !VARIABLES OUTPUTS
        real(r_8), intent(out) :: mort_perc !ind that will REMAIN in % due to mortality. This number is used to multiply carbon pools (it is what remains)


        !INTERNAL VARIABLES
        ! real(r_8) :: nind_kill_FPC
        real(r_8) :: greff_pls !growth efficiency in m2/gC for each PLS
        real(r_8) :: mort_wd   !mortality by wood density (from Sakschewski et al 2015)
        real(r_8) :: mort_greff !mortality by growth efficiency
        real(r_8) :: nind_kill_greff !number of avg ind. (ind/m2) that will die due to growthn efficiency
        real(r_8) :: nind_kill_total !total number of avg ind that will die (ind/m2)

        !CHECK: minimum density mortality

        !initializing variables
        greff_pls       = 0.0D0
        mort_wd         = 0.0D0
        mort_greff      = 0.0D0
        nind_kill_greff = 0.0D0
        nind_kill_total = 0.0D0
        mort_perc       = 0.0D0

        greff_pls = carbon_increment / cleaf_total_pls * spec_leaf 

        mort_wd = exp( -2.66 + (0.255 / wood_density)) 

        mort_greff = mort_wd / (1 + k_mort2 * greff_pls)
                    
        nind_kill_greff = dens_pls_old * mort_greff
      

        !summing nind_kill
        nind_kill_total = nind_kill_FPC + nind_kill_greff

        mort_perc = (dens_pls_old - nind_kill_total) / dens_pls_old 

        print*, 'mort perc', mort_perc        

    end subroutine mortality
    
end module FPC



