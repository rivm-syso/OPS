module m_ops_tdo_proc

implicit none

TYPE Tdo_proc
   ! Options to switch on / off certain processes; for testing purposes only !
   logical :: chem         ! do chemistry
   logical :: depl_drydep  ! do depletion (i.e. loss over trajectory) caused by dry deposition; dry deposition at receptor is still possible
   logical :: depl_wetdep  ! do depletion (i.e. loss over trajectory) caused by wet deposition; wet deposition at receptor is still possible
   logical :: grad_drydep  ! do gradient due to dry deposition (at receptor). Note: there is no gradient due to wet deposition.
END TYPE
end module m_ops_tdo_proc

