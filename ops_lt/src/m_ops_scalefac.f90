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
! DESCRIPTION        : Compute scaling factors for printing of concentrations and depositions. A scaling factor is the ratio between the 
!                      computed concentration (or deposition) and the value to be printed. All ratio's are based on an input source strength
!                      in g/s and an output in ug/m3 for concentrations and mol/ha/y for depositions.
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_scalefac

implicit none

contains

SUBROUTINE ops_scalefac(nrrcp, nsubsec, cpri, csec, drydep, wetdep, scale_con, scale_sec, scale_dep, csubsec, scale_subsec)

use m_commonconst_lt                                                              ! EPS_DELTA only

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER        (ROUTINENAAM = 'ops_scalefac')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: nrrcp                      ! number of receptor points
INTEGER,   INTENT(IN)                            :: nsubsec                    ! number sub-secondary species
REAL,      INTENT(IN)                            :: cpri(nrrcp)                ! array van primaire concentraties
REAL,      INTENT(IN)                            :: csec(nrrcp)                ! array van secundaire concentraties
REAL,      INTENT(IN)                            :: drydep(nrrcp)              ! array van droge depositie
REAL,      INTENT(IN)                            :: wetdep(nrrcp)              ! array van natte depositie
REAL,      INTENT(IN), OPTIONAL                  :: csubsec(nrrcp,nsubsec)     ! concentration of sub-secondary substance [ug/m3]

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: scale_con                  ! schaal vergr. concentratie
REAL,      INTENT(OUT)                           :: scale_sec                  ! schaal vergr. secundaire concentratie
REAL,      INTENT(OUT)                           :: scale_dep                  ! schaal vergr. droge depositie
REAL,      INTENT(OUT), OPTIONAL                 :: scale_subsec(nsubsec)      ! scaling factor for sub-secondary species

! LOCAL VARIABLES
INTEGER                                          :: i                          ! teller over schaalfactoren
INTEGER                                          :: isubsec                    ! index of sub-secondary species
REAL                                             :: cmax                       ! grootst voorkomende primaire concentratie
REAL                                             :: csmax                      ! grootst voorkomende secundaire concentratie
REAL                                             :: csubsecmax(nsubsec)        ! maximal value csubsec
REAL                                             :: ddepmax                    ! grootst voorkomende droge depositie
REAL                                             :: depntmax                   ! grootst voorkomende natte depositie
REAL                                             :: s                          ! schaalfactor
REAL                                             :: tc                         ! teller aantal te grote prim. conc.
REAL                                             :: td                         ! teller aantal te grote droge dep.
REAL                                             :: tn                         ! teller aantal te grote natte dep.
REAL                                             :: ts                         ! teller aantal te grote sec. conc.
REAL                                             :: tsubsec(nsubsec)                ! number of sub-secondary species with too large concentrations
REAL                                             :: scale_dry                  ! schaal vergr. concentratie
REAL                                             :: scale_wet                  ! schaal vergr. concentratie

!-------------------------------------------------------------------------------------------------------------------------------
!
! Compute maxima of primary and secondary concentrations and dry and wet depositions
!
cmax     = MAXVAL(cpri(:))
csmax    = MAXVAL(csec(:))
IF (PRESENT(csubsec)) csubsecmax = MAXVAL(csubsec,1)
ddepmax  = MAXVAL(drydep(:))
depntmax = MAXVAL(wetdep(:))


!
! Initialise scaling factors for primary and secondary concentrations and dry and wet depositions
!
scale_con = 1.0e-10
scale_sec = 1.0e-10
IF (PRESENT(csubsec)) scale_subsec = 1.0e-10
scale_dry = 1.0e-10
scale_wet = 1.0e-10
! 
! Loop over exponent in scaling factor (scaling factors range from 1e-10 to 1e30);
! Set scaling factor s, such that for a parameter x with maximum xmax: s*xmax < 2000 (or (2000/xmax) > s)
!
DO i = -10, 30
   s = 10**(FLOAT(i))
   ! Using nested if-statement as short-circuit evaluation is apparently not
   ! supported by all compilers.
   if (cmax .GT. (0. + EPS_DELTA)) then
      if ((2000./cmax) .GT. (s + EPS_DELTA)) then
         scale_con = s
      endif
   endif
   IF (csmax .GT. (0. + EPS_DELTA)) then
      if ((2000./csmax) .GT. (s + EPS_DELTA)) THEN
         scale_sec = s
      endif
   ENDIF
  IF (PRESENT(csubsec)) THEN
    do isubsec = 1,nsubsec
       IF (csubsecmax(isubsec) .GT. (0. + EPS_DELTA) .AND. (2000./csubsecmax(isubsec)) .GT. (s + EPS_DELTA)) THEN
          scale_subsec(isubsec) = s
       ENDIF
    enddo 
  ENDIF
  IF (ddepmax .GT. (0. + EPS_DELTA) .AND. (2000./ddepmax) .GT. (s + EPS_DELTA)) THEN
     scale_dry = s
  ENDIF
  IF (depntmax .GT. (0. + EPS_DELTA) .AND. (2000./depntmax) .GT. (s + EPS_DELTA)) THEN
     scale_wet = s
  ENDIF
ENDDO
!
! Count number of values > 999
!
tc = COUNT(cpri(:)*scale_con .GT. 999.+EPS_DELTA)
ts = COUNT(csec(:)*scale_sec .GT. 999.+EPS_DELTA)
IF (PRESENT(csubsec)) then
   do isubsec = 1,nsubsec
      tsubsec(isubsec) = COUNT(csubsec(:,isubsec)*scale_subsec(isubsec) .GT. 999.+EPS_DELTA)
   enddo
ENDIF
td = COUNT(drydep(:)*scale_dry .GT. 999.+EPS_DELTA)
tn = COUNT(wetdep(:)*scale_wet .GT. 999.+EPS_DELTA)
!
! If more than 5% of values are > 999, divide scaling factor by 10
!
IF (tc .GT. (0.05*nrrcp + EPS_DELTA)) THEN
  scale_con = scale_con/10.
ENDIF
IF (ts .GT. (0.05*nrrcp + EPS_DELTA)) THEN
  scale_sec = scale_sec/10.
ENDIF
IF (PRESENT(csubsec)) THEN
   do isubsec = 1,nsubsec
     IF (tsubsec(isubsec) .GT. (0.05*nrrcp + EPS_DELTA)) THEN
        scale_subsec(isubsec) = scale_subsec(isubsec)/10.
     ENDIF
  enddo
ENDIF
IF (td .GT. (0.05*nrrcp + EPS_DELTA)) THEN
  scale_dry = scale_dry/10.
ENDIF
IF (tn .GT. (0.05*nrrcp + EPS_DELTA)) THEN
  scale_wet = scale_wet/10.
ENDIF
scale_dep = AMIN1(scale_dry, scale_wet)

RETURN
END SUBROUTINE ops_scalefac

end module m_ops_scalefac
