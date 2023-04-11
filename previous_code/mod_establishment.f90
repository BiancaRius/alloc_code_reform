!-------------------------------------------------------------------------------------------------
    !Module defining functions related to the establishment of average individuals in
    !a PLS if the area of occupation of all PLSs is lower than 95% of the considered area
    !It informs both the new density of individuals and the "shrink" process (see section 
    !4.6.1.Establishment in Smith et al. 2001)
    !Most of the equations are based on LPJ population mode (as described in Smith et al 2001) 
    !and can be found in Sitch et al., 2003 and Smith et al., 2001.
    !In this mode, each PLS is represented by a population of average individuals.
    !All individuals of a PLS present the same features and the population is defined by
    !the density of individuals per m2.
!--------------------------------------------------------------------------------------------------  

module establishment
    use types
    use params


implicit none

private

public :: establish, shrink, sapling_allometry


contains

    subroutine establish(npls_alive, FPC_total_gc, est_pls) !establishment of sapling per PLS
   
        !parameters (types.f90):
        !est_max
        !p1
        
        !VARIABLES [INPUT] - vem do módulo de FPC
        real(r_8), intent(in) :: FPC_total_gc !(m2) total FPC in a grid cell considering all PLS
        real(r_8), intent(in) :: npls_alive   !number of alive PLSs
        
        !VARIABLES [OUTPUT] - 
        real(r_8), intent(out) :: est_pls !(number of individuals/m2/year) establishment rate - PLS especific 

        !Local Variables
        real(r_8) :: FPC_total_perc !percentage of gc_area that is not occupied by trees
        real(r_8) :: est            !(number of individuals/m2/year) overall establishment rate - not PLS specific
     
        ! !initializing variables
        FPC_total_perc = 0.0D0
        est            = 0.0D0
        est_pls        = 0.0D0
        


        !The overall establishment rate for trees 
        !(est) is proportional to the fractional ground area not covered by trees (occupied/gc size 95%)
        FPC_total_perc = FPC_total_gc/gc_area_95
        
        est = est_max * (1. - exp(p1 * (1-FPC_total_perc)) / npls_alive) !no need to declare est_max 

        est_pls = max(est * (1. - FPC_total_perc),0.)

          
    end subroutine establish

    subroutine sapling_allometry(cleaf_sapl, csap_sapl, cheart_sapl,croot_sapl)

        !VARIABLES [INPUT] - no input

        !VARIABLES [OUTPUT]  
        !Carbon content on plant compartments in saplings (gC)
        real(r_8), intent(out) :: cleaf_sapl  !leaves
        real(r_8), intent(out) :: csap_sapl   !sapwood
        real(r_8), intent(out) :: cheart_sapl !heartwood
        real(r_8), intent(out) :: croot_sapl  !fine roots

        !interal variables
        real(r_8) :: diam_sapl   !sapling diameter
        real(r_8) :: height_sapl !sapling height
        
        ! !initializing variables
        cleaf_sapl   = 0.0D0
        csap_sapl    = 0.0D0
        cheart_sapl  = 0.0D0
        croot_sapl   = 0.0D0
        diam_sapl    = 0.0D0
        height_sapl  = 0.0D0


        !Equations - from LPJmlfire
        cleaf_sapl = (lai_sapl * k_allom1_sapl * x_sapl ** reinickerp_sapl * (4. * sla_sapl / pi / klatosa_sapl) **& 
                     (reinickerp_sapl * 0.5) / sla_sapl) ** (1. - 1. / reinickerp_sapl)

        diam_sapl = x_sapl * (4. * cleaf_sapl * sla_sapl / pi / klatosa_sapl) ** 0.5

        height_sapl = k_allom2_sapl * diam_sapl ** k_allom3_sapl

        csap_sapl = dwood_sapl * height_sapl * cleaf_sapl * sla_sapl / klatosa_sapl

        cheart_sapl = (x_sapl - 1.) * csap_sapl

        croot_sapl = (1. / lmtorm_sapl) * cleaf_sapl
             

    end subroutine sapling_allometry
    
    subroutine shrink(cleaf_old, csap_old, cheart_old, croot_old,&
                      est_pls, dens_pls_old,&
                      cleaf_est, csap_est, cheart_est, croot_est, dens_est) !calculates new densities after the application of shrink process 
                                                                             !(shrink of avg individual for carbon balance -- need to be applied because 
                                                                             !of the saplings establishment)
    
        
        
        !VARIABLES [INPUT] 

        real(r_8), intent(in) :: est_pls !(number of individuals/m2/year) establishment rate - PLS especific (from establish subroutine)

        real(r_8), intent(in) :: dens_pls_old !PLS density (number of individuals/m2) previous the establishment of saplings

        !Carbon content on plant compartments before saplings establishment (gC)
        real(r_8), intent(in) :: csap_old   !sapwood
        real(r_8), intent(in) :: cleaf_old  !leaves
        real(r_8), intent(in) :: cheart_old !heartwood
        real(r_8), intent(in) :: croot_old  !fine roots
        
       
        !VARIABLES [OUTPUT]  

        !Carbon content on plant compartments after saplings establishment (gC) and after shrink process
        real(r_8), intent(out) :: csap_est   !sapwood
        real(r_8), intent(out) :: cleaf_est  !leaves
        real(r_8), intent(out) :: cheart_est !heartwood
        real(r_8), intent(out) :: croot_est  !fine roots
        ! real(r_8), intent(out) :: cwood_est  !total wood

        real(r_8), intent(out) :: dens_est !PLS density (number of individuals/m2) after the establishment of saplings

        !Carbon content on plant compartments in saplings (gC) - calculated on sapling_allometry subroutine
        real(r_8) :: csap_sapl   !sapwood
        real(r_8) :: cleaf_sapl  !leaves
        real(r_8) :: cheart_sapl !heartwood
        real(r_8) :: croot_sapl  !fine roots

        real(r_8) :: cwood_est
        
     
        ! !initializing variables
        dens_est   = 0.0D0
        csap_est   = 0.0D0
        cleaf_est  = 0.0D0
        cheart_est = 0.0D0
        croot_est  = 0.0D0
        cwood_est  = 0.0D0
         
        
        dens_est = dens_pls_old + est_pls

        call sapling_allometry(cleaf_sapl, csap_sapl, cheart_sapl,croot_sapl)

        !Equations from Sitch et al., 2003
        csap_est   = ((csap_old * dens_pls_old) + (csap_sapl * est_pls)) / dens_est 

        cleaf_est  = ((cleaf_old * dens_pls_old) + (cleaf_sapl * est_pls)) / dens_est
      
        cheart_est = ((cheart_old * dens_pls_old) + (cheart_sapl * est_pls)) / dens_est

        croot_est  = ((croot_old * dens_pls_old) + (croot_sapl * est_pls)) / dens_est

        cwood_est  = cheart_est + csap_est

        ! print*, csap_new


            
    end subroutine shrink


end module establishment
