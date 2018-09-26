#include "GCHEM_OPTIONS.h"

CBOP
C !ROUTINE: GCHEM_READPARMS

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_READPARMS( myThid )

C !DESCRIPTION:
C     Initialize GCHEM parameters, read in data.gchem

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GCHEM.h"

C !INPUT PARAMETERS: ===================================================
C  myThid         :: thread number
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_GCHEM

C !LOCAL VARIABLES: ====================================================
C  tIter0         :: retired parameter
C  iUnit          :: unit number for I/O
C  msgBuf         :: message buffer
      INTEGER tIter0
      INTEGER iUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

C- Sub-package on/off flags: not fully implemented, requires
C  to test the flag before any corresponding pkg S/R call

      NAMELIST /GCHEM_PARM01/
     &                   useCFC,
     &                   useDIC,
     &                   useBLING,
     &                   useSPOIL,
     &                   useDARWIN,
     &           fileName1, fileName2, fileName3,
     &           fileName4, fileName5,
     &           gchem_int1, gchem_int2, gchem_int3,
     &           gchem_int4, gchem_int5, nsubtime,
     &           gchem_rl1, gchem_rl2, gchem_rl3,
     &           gchem_rl4, gchem_rl5,
     &           gchem_ForcingPeriod, gchem_ForcingCycle,
     &           tIter0

C- Schmidt number/Solubility/... coeficients
      NAMELIST /GCHEM_PARM02/
     &           sc_cfc11_p0, sc_cfc11_p1, sc_cfc11_p2, sc_cfc11_p3,
     &           sc_cfc12_p0, sc_cfc12_p1, sc_cfc12_p2, sc_cfc12_p3
C     &           sc_co2_p0, sc_co2_p1, sc_co2_p2, sc_co2_p3,
C     &           sc_o2_p0, sc_o2_p1, sc_o2_p2, sc_o2_p3

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.useGCHEM ) THEN
C-    pkg GCHEM is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.gchem is found
         CALL PACKAGES_UNUSED_MSG( 'useGCHEM', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

C- Set defaults values for parameters in GCHEM.h
       useCFC = .FALSE.
       useDIC = .FALSE.
       useBLING  = .FALSE.
       useSPOIL  = .FALSE.
       useDARWIN = .FALSE.
       tIter0 = UNSET_I
       fileName1=' '
       fileName2=' '
       fileName3=' '
       fileName4=' '
       fileName5=' '
       nsubtime = 1
       gchem_int1=0
       gchem_int2=0
       gchem_int3=0
       gchem_int4=0
       gchem_int5=0
       gchem_rl1=0. _d 0
       gchem_rl2=0. _d 0
       gchem_rl3=0. _d 0
       gchem_rl4=0. _d 0
       gchem_rl5=0. _d 0
C  default periodic forcing to same as for physics
       gchem_ForcingPeriod=externForcingPeriod
       gchem_ForcingCycle=externForcingCycle

C Schmidt number coeficients
       sc_cfc11_p0 = 3501.8 _d 0
       sc_cfc11_p1 = -210.31 _d 0
       sc_cfc11_p2 =    6.1851 _d 0
       sc_cfc11_p3 =   -0.075139 _d 0

       sc_cfc12_p0 = 3845.4 _d 0
       sc_cfc12_p1 = -228.95 _d 0
       sc_cfc12_p2 =    6.1908 _d 0
       sc_cfc12_p3 =   -0.067430 _d 0


C- Open and read the data.gchem file
      WRITE(msgBuf,'(A)') ' GCHEM_READPARMS: opening data.gchem'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      CALL OPEN_COPY_DATA_FILE(
     I                   'data.gchem', 'GCHEM_READPARMS',
     O                   iUnit,
     I                   myThid )
      
      READ(UNIT=iUnit,NML=GCHEM_PARM01)

      READ(UNIT=iUnit,NML=GCHEM_PARM02)

      WRITE(msgBuf,'(A)')
     &  ' GCHEM_READPARMS: finished reading data.gchem'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C- Close the open data file
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

C- Check for retired parameters:
      IF ( tIter0 .NE. UNSET_I ) THEN
c      nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)')
     &  'S/R GCHEM_READPARMS: Paramater "tIter0" is',
     &  ' no longer allowed in file "data.gchem"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)')
     &  'S/R GCHEM_READPARMS: "tIter0" has been moved to',
     &  ' PTRACERS_Iter0 in file "data.ptracers".'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R GCHEM_READPARMS'
      ENDIF

      _END_MASTER(myThid)

C  Everyone else must wait for the parameters to be loaded
      _BARRIER

#ifdef ALLOW_CFC
      IF ( useCFC ) THEN
        CALL CFC_READPARMS(myThid)
      ENDIF
#endif

#ifdef ALLOW_DIC
      IF ( useDIC ) THEN
        CALL DIC_READPARMS(myThid)
      ENDIF
#endif

#ifdef ALLOW_BLING
      IF ( useBLING ) THEN
        CALL BLING_READPARMS(myThid)
      ENDIF
#endif

#ifdef ALLOW_SPOIL
      IF ( useSPOIL ) THEN
        CALL SPOIL_READPARMS(myThid)
      ENDIF
#endif

#ifdef ALLOW_DARWIN
      IF ( useDARWIN ) THEN
        CALL DARWIN_READPARMS(myThid)
      ENDIF
#endif

C- Register GCHEM tracer indices
      CALL GCHEM_TR_REGISTER( myThid )

#endif /* ALLOW_GCHEM */

      RETURN
      END
