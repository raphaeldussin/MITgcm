#include "GCHEM_OPTIONS.h"
C--  File gchem_compute_schmidt.F: Routines to compute schmidt number
C for gas exchange
C--   Contents
C--   o GCHEM_COMPUTE_SCHMIDT

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: GCHEM_COMPUTE_SCHMIDT

C !INTERFACE:
      SUBROUTINE GCHEM_COMPUTE_SCHMIDT(
     I                     p0, p1, p2, p3,
     O                     schmidt,
     I                     iMin, iMax, jMin, jMax, k, bi, bj,
     I                     myTime, myIter, myThid )

C     !DESCRIPTION:
C     Compute the schmidt number of a particular given the polynomial
C     coeficients p0 to p3. Sc = p0 + p1 * T + p2 * T*T + p3 * T*T*T

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "GCHEM.h"

C     !INPUT/OUTPUT PARAMETERS:
C     p0, p1, p2, p3 :: coeficients for polynomial
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     k         :: Current vertical level index
C     bi,bj     :: Current tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL     p0, p1, p2, p3
      _RL     schmidt(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER k, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

C     local variables
      INTEGER i, j

C only compute at the surface layer
      k=1

C calculate SCHMIDT NO. for Gas
        DO j=jMin,jMax
         DO i=iMin,iMax
           IF ( maskC(i,j,k,bi,bj).NE.zeroRS ) THEN
             schmidt(i,j) =
     &            p0
     &          + p1 * theta(i,j,k,bi,bj)
     &          + p2 * theta(i,j,k,bi,bj)*theta(i,j,k,bi,bj)
     &          + p3 * theta(i,j,k,bi,bj)*theta(i,j,k,bi,bj)
     &               * theta(i,j,k,bi,bj)
           ENDIF
         ENDDO
        ENDDO

      RETURN
      END
