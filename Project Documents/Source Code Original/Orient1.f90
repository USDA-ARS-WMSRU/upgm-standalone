SUBROUTINE orient1(rh,rw,rs,rd,impl_rh,impl_rw,impl_rs,impl_rd,tilld,rflag)
!
IMPLICIT NONE
!
! Subroutine arguments
!
REAL :: impl_rd,impl_rh,impl_rs,impl_rw,rd,rh,rs,rw,tilld
INTEGER :: rflag
!
!     + + + purpose + + +
!
!     this subroutine performs an oriented roughness calculation
!     after a tillage operation.  actually it performs a check of the
!     ridge flag (rflag) and does the coresponding manipulation
!     of the ridge parameters.  the three valid values of the
!     ridge flag are:
!     0 - operation has no effect if a ridge currently exists.
!     1 - set all oriented roughness parameters to the implement values.
!     2 - modification depends on the current ridge height,
!         specified tillage depth, and ridging characteristics
!         of the tillage implement.
!         if the tillage depth is great enough to remove the ridges,
!         ridge values are set according to the implement values.
!         if the tillage depth is too shallow to remove the current
!         ridges alone, then the two following situations occur:
!         a) if the difference between the original ridge height and
!         specified tillage depth is less than the implement specified
!         ridging height, the ridge values are set according to the
!         implement values.
!         b) if not, then the current ridge remains but at a reduced
!         height dependent upon the implement tillage depth.
!
!     + + + keywords + + +
!     oriented roughness (or), tillage (primary/secondary)
!
!     + + + argument declarations + + +
!
!     + + + argument definitions + + +
!
!     + + + accessed common block variable definitions + + +
!
!     + + + parameters + + +
!
!     + + + local variables + + +
!
!
!     + + + local variable definitions + + +
!
!     rh      - current ridge height (mm)
!     rw      - current ridge width (mm)
!     rs      - current ridge spacing (mm)
!     rd      - current ridge direction (clockwise from true north)
!     impl_rh - implement ridge height (mm)
!     impl_rw - implement ridge width (mm)
!     impl_rs - implement ridge spacing (mm)
!     impl_rd - implement ridge direction (clockwise from true north)
!     tilld  - implement tillage depth (mm)
!     rflag  - flag (0-3) telling what need to be done
 
!     + + + end specifications + + +
!
!  perform the calculation of the oriented or after a tillage
!     operation.
!
SELECT CASE (rflag)
 
CASE (0)         !typical of a row cultivator in a ridged field
  IF (rh.LT.0.1) THEN           !if ridges don't exist, create'em
     rh = impl_rh
     rw = impl_rw
     rs = impl_rs
     rd = impl_rd
  END IF
 
CASE (1)         !always set ridge values to those specified for tool
  rh = impl_rh
  rw = impl_rw
  rs = impl_rs
  rd = impl_rd
 
CASE (2)         !adjust ridge height based upon tillage depth
  IF (tilld.GE.(rh/2.0)) THEN
            !tillage depth is deep enough
     rh = impl_rh
     rw = impl_rw
     rs = impl_rs
     rd = impl_rd
  ELSE IF (impl_rh.GE.(2.0*(rh/2.0-tilld))) THEN
!		   !tillage implement ridging great enough
     rh = impl_rh
     rw = impl_rw
     rs = impl_rs
     rd = impl_rd
  ELSE
               !tdepth too shallow to completely remove original ridges
     rh = 2.0*(rh/2.0-tilld)
  END IF
 
CASE DEFAULT
  PRINT *,'the ridge flag (for oriented roughness)'
  PRINT *,' was not set correctly'
 
END SELECT
!
END SUBROUTINE orient1
