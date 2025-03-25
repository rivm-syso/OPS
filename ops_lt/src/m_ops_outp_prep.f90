!-------------------------------------------------------------------------------------------------------------------------------
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.
!

!-------------------------------------------------------------------------------------------------------------------------------
! DESCRIPTION        : Prepare output process (print/plot)
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_outp_prep

implicit none

contains

SUBROUTINE ops_outp_prep(nrrcp, icm, nsubsec, conc_cf, f_subsec_rcp, csec, drydep, wetdep, cpri, cpri_class_sum, cnox, totdep, csubsec, scale_con, scale_sec,     &
                      &  scale_subsec, scale_dep, class_output)
use m_ops_scalefac
use m_commonconst_lt

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: nrrcp                      ! 
INTEGER,   INTENT(IN)                            :: icm                        ! 
INTEGER,   INTENT(IN)                            :: nsubsec                    ! number of sub-secondary species                       
REAL,      INTENT(IN)                            :: conc_cf                    ! correction factor NOx
REAL,      INTENT(IN)                            :: f_subsec_rcp(nrrcp,nsubsec)! fractions for sub-secondary species, HNO3/NO3_total, NO3_C/NO3_total, NO3_F/NO3_total [-]
REAL,      INTENT(IN)                            :: csec(nrrcp)                ! 
REAL,      INTENT(IN)                            :: drydep(nrrcp)              ! 
REAL,      INTENT(IN)                            :: wetdep(nrrcp)              ! 
LOGICAL,   INTENT(IN)                            :: class_output               ! indicator whether results for receptors will be stored per wind sector/distance/particle/stability class

! SUBROUTINE ARGUMENTS - I/O
REAL,      INTENT(INOUT)                         :: cpri(nrrcp)                ! 
REAL,   POINTER, INTENT(INOUT)                   :: cpri_class_sum(:,:,:,:)    ! concentration of primary component per class, sum over part. classes [ug/m3] 
                                                                               ! only allocated if class_output=.true.
REAL,      INTENT(INOUT)                         :: cnox(nrrcp)                ! NOx concentration, per receptor

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: totdep(nrrcp)              ! 
REAL,      INTENT(OUT)                           :: csubsec(nrrcp,nsubsec)     ! concentration of sub-secondary species [ug/m3]
REAL,      INTENT(OUT)                           :: scale_con                  ! 
REAL,      INTENT(OUT)                           :: scale_sec                  ! 
REAL,      INTENT(OUT)                           :: scale_subsec(nsubsec)      ! scaling factor for sub-secondary species
REAL,      INTENT(OUT)                           :: scale_dep                  ! 

! LOCAL VARIABLES
INTEGER                                          :: isubsec                    ! index of sub-secondary species

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_outp_prep')

!-------------------------------------------------------------------------------------------------------------------------------
!
! Calculate totdep = total deposition = dry deposition + wet deposition
totdep = drydep + wetdep

IF (icm == icm_NOx) THEN
   ! Correct cpri = NOx concentration (to account for HNO2 and PAN contributions to NO2)
   !    Note: NO2 by the vdHout formula is based on measurements and is not corrected
   cpri = cpri * conc_cf 
   
   IF (class_output) THEN
     cpri_class_sum = cpri_class_sum * conc_cf
   ENDIF
   
   cnox = cnox * conc_cf
 
   ! Calculate concentration of sub-secondary species 
   do isubsec = 1,nsubsec
      csubsec(:,isubsec) = f_subsec_rcp(:,isubsec)*csec
   enddo
ENDIF
!
! Scaling factors for concentration and deposition fields
!
CALL ops_scalefac(nrrcp, nsubsec, cpri, csec, drydep, wetdep, scale_con, scale_sec, scale_dep, csubsec, scale_subsec)

RETURN

END SUBROUTINE ops_outp_prep

end module m_ops_outp_prep
