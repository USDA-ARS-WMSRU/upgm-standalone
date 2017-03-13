FUNCTION valbydepth(layrsn,bszlyd,lay_val,ai_flag,depthtop,depthbot)
!
IMPLICIT NONE
!
INCLUDE 'p1unconv.inc'
!
! Function arguments
!
INTEGER :: ai_flag,layrsn
REAL :: depthbot,depthtop
REAL,DIMENSION(layrsn) :: bszlyd,lay_val
REAL :: valbydepth
!
! Local variables
!
REAL,DIMENSION(layrsn) :: dmlayr
INTEGER :: indexbot,indextop,lay
!
!     + + + purpose + + +
!     find the average of any soil property based on layer thickness
!     containing the soil property
 
!     + + + key words + + +
!     averaging, interpolation
 
!     + + + common blocks + + +
 
!     + + + local common blocks + + +
 
!     + + + argument declarations + + +
 
!     + + + argument definitions + + +
!     layrsn  - number of soil layers used in the simulation
!     bszlyd  - distance from surface to bottom of layer (mm)
!     lay_val - layer based array of values to be averaged or interpolated
!     ai_flag - flag indicating averaging scheme used
!           0 - entire layer assumed to have same value
!           1 - value assumed valid at center of layer, with continuous
!               transition to next layer center
!     depthtop - depth in the soil of the top of the segment to be averaged
!     depthbot - depth in the soil of the bottom of the segment to be
!                averaged or interpolated
 
!     + + + parameters + + +
 
!     + + + local variables + + +
!
!     dmlayr declared here but not used in subroutine  jcaii  8/08/08
!
 
!     + + + local definitions + + +
!     lay    - soil layer index
 
!     + + + functions called + + +
 
!     + + + subroutines called + + +
 
!     + + + data initializations + + +
 
!     + + + end specifications + + +
 
indextop = 0
indexbot = 0
 
dmlayr(1) = 0.5*bszlyd(1)*mmtom
 
lay = 1
IF (depthtop.LE.bszlyd(lay)) indextop = lay
IF (depthbot.LE.bszlyd(lay)) indexbot = lay
 
DO lay = 2,layrsn
          ! find center of the layers
  dmlayr(lay) = 0.5*mmtom*(bszlyd(lay-1)+bszlyd(lay))
 
          ! check if top depth contained in layer
  IF ((depthtop.LE.bszlyd(lay)).AND.(depthtop.GT.bszlyd(lay-1))) indextop = lay
          ! check if bottom depth contained in layer
  IF ((depthbot.LE.bszlyd(lay)).AND.(depthbot.GT.bszlyd(lay-1))) indexbot = lay
END DO
 
      ! check for bottom depth past soil bottom
IF (depthbot.GT.bszlyd(layrsn)) indexbot = layrsn
 
IF (ai_flag.EQ.0) THEN
  IF (indextop.EQ.indexbot) THEN
            ! entirely in the same layer, use layer value
     valbydepth = lay_val(indextop)
  ELSE
            ! crosses one or more layer boundaries
            ! this is a layer thickness weighted average
            ! first section, layer containing depthtop
     valbydepth = lay_val(indextop)*(bszlyd(indextop)-depthtop)
     DO lay = indextop + 1,indexbot - 1
              ! add in layers contained in interval
        valbydepth = valbydepth + lay_val(lay)*(bszlyd(lay)-bszlyd(lay-1))
     END DO
            ! last section, layer containing depthbot
     valbydepth = valbydepth + lay_val(indexbot)*(depthbot-bszlyd(indexbot-1))
            ! divide by total depth to get average
     valbydepth = valbydepth/(depthbot-depthtop)
  END IF
ELSE IF (ai_flag.EQ.1) THEN
  valbydepth = 0.0
  WRITE (*,*) 'valbydepth: ai_flag method 1 not yet implemented'
  STOP
END IF
! 
END FUNCTION valbydepth
