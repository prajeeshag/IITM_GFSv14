#include "./ESMFVersionDefine.h"
!-----------------------------------------------------------------------
!
      MODULE module_GSM_CAP
!
!-----------------------------------------------------------------------
!***  This module is GSM NUOPC CAP for gsm grid component
!-----------------------------------------------------------------------
!
!  2016-10-20  Jun Wang  - change module_ATM_GRID_COMP.F90 into gsm cap
!-----------------------------------------------------------------------
!
      USE ESMF

      use NUOPC
      use NUOPC_Model, only: &
        model_routine_SS            => SetServices, &
        model_label_DataInitialize  => label_DataInitialize, &
        model_label_CheckImport     => label_CheckImport, &
        model_label_Advance         => label_Advance
      use module_CPLFIELDS
!
      USE module_gfs_grid_comp      ,ONLY:  &
        gfs_initialize, gfs_run, gfs_finalize
!
      USE module_ERR_MSG,ONLY: ERR_MSG,MESSAGE_CHECK
!
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      PRIVATE
!
! private internal state to keep instance data
      type InternalStateStruct
        integer(ESMF_KIND_I4), pointer :: numPerRow(:)
        real(ESMF_KIND_R8), pointer :: lons(:,:), lats(:,:)
        integer, pointer :: rowinds(:), indList(:)
        integer :: dims(2)
        integer :: myrows
        integer :: wamtotalnodes, localnodes
        integer :: PetNo, PetCnt
      end type

      type InternalState
        type(InternalStateStruct), pointer :: wrap
      end type
!
      PUBLIC :: SetServices
!
!-----------------------------------------------------------------------
!
      character(len=160) :: nuopcMsg
      logical :: write_diagnostics = .true.
      logical :: profile_memory = .true.
      type(ESMF_Clock)                :: clock_gfs
      type(ESMF_State):: gfsImportState, gfsExportState


!-----------------------------------------------------------------------
!
      CONTAINS
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!
      SUBROUTINE SetServices(gcomp,rc_reg)
!
!-----------------------------------------------------------------------
!***  Register the Init, Run, and Finalize routines of 
!***  the GSM component.
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      TYPE(ESMF_GridComp)               :: gcomp       
      INTEGER            ,INTENT(OUT)   :: rc_reg
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------

      ! the NUOPC model component will register the generic methods
      call NUOPC_CompDerive(gcomp, model_routine_SS, rc=RC_REG)
      if (ESMF_LogFoundError(rcToCheck=RC_REG, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
        
      ! Provide InitializeP0 to switch from default IPDv00 to IPDv02
      call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
        InitializeP0, phase=0, rc=RC_REG)
      if (ESMF_LogFoundError(rcToCheck=RC_REG, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! IPDv02p1: advertise Fields
      call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
        phaseLabelList=(/"IPDv02p1"/), userRoutine=InitializeIPDv02p1, &
        rc=RC_REG)
      if (ESMF_LogFoundError(rcToCheck=RC_REG, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! IPDv02p3: realize connected Fields
      call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
        phaseLabelList=(/"IPDv02p3"/), userRoutine=InitializeIPDv02p3, &
        rc=RC_REG)
      if (ESMF_LogFoundError(rcToCheck=RC_REG, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! attach specializing method(s)
      call NUOPC_CompSpecialize(gcomp, &
        specLabel=model_label_DataInitialize, specRoutine=GSM_DATAINIT, &
        rc=RC_REG)
      if (ESMF_LogFoundError(rcToCheck=RC_REG, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call NUOPC_CompSpecialize(gcomp, &
        specLabel=model_label_Advance, specRoutine=GSM_ADVANCE, rc=RC_REG)
      if (ESMF_LogFoundError(rcToCheck=RC_REG, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_MethodRemove(gcomp, model_label_CheckImport, rc=rc_reg)
      if (ESMF_LogFoundError(rcToCheck=RC_REG, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call NUOPC_CompSpecialize(gcomp, &
        specLabel=model_label_CheckImport, specRoutine=GSM_CHECKIMPORT, rc=RC_REG)
      if (ESMF_LogFoundError(rcToCheck=RC_REG, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! Overwrite generic NUOPC_Model Finalize method
      CALL ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE, &
        GSM_CAP_FINALIZE, rc=RC_REG)
      if (ESMF_LogFoundError(rcToCheck=RC_REG, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
!      
!-----------------------------------------------------------------------
!
      END SUBROUTINE SetServices
!
!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
!
  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    character(len=10)                         :: value
    
    rc = ESMF_SUCCESS

    ! Switch to IPDv02 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv02"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_AttributeGet(gcomp, name="DumpFields", value=value, defaultValue="true", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    write_diagnostics=(trim(value)=="true")

    call ESMF_AttributeGet(gcomp, name="ProfileMemory", value=value, defaultValue="true", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    profile_memory=(trim(value)/="false")
    
  end subroutine

  !-----------------------------------------------------------------------

  subroutine InitializeIPDv02p1(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS

    ! advertise Fields

    ! importable fields:
    call NUOPC_Advertise(importState, StandardNames=ImportFieldsList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable fields:
    call NUOPC_Advertise(exportState, StandardNames=ExportFieldsList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------

  subroutine InitializeIPDv02p3(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    integer                         :: n
    type(ESMF_Grid)                 :: gridIn, gridOut
    type(ESMF_array)                :: array
    real(ESMF_KIND_R8)              :: medAtmCouplingIntervalSec
    type(ESMF_Clock)                :: gsmClock
    type(ESMF_TimeInterval)         :: gsmStep, earthStep
    type(ESMF_Time)                 :: currTime, stopTime
    TYPE(ESMF_Config)               :: CF


    rc = ESMF_SUCCESS
    

    ! call into the actual GSM initialize routine
!------------------------------------------------------------------------------
    ! check earth clock
    call ESMF_ClockPrint(clock, options="currTime", &
         preString="entering GSM_INITIALIZE with CLOCK_EARTH current: ", &
         unit=nuopcMsg)
    call ESMF_LogWrite(nuopcMsg, ESMF_LOGMSG_INFO)
    call ESMF_ClockPrint(clock, options="startTime", &
         preString="entering GSM_INITIALIZE with CLOCK_EARTH start:   ", &
         unit=nuopcMsg)
    call ESMF_LogWrite(nuopcMsg, ESMF_LOGMSG_INFO)
    call ESMF_ClockPrint(clock, options="stopTime", &
         preString="entering GSM_INITIALIZE with CLOCK_EARTH stop:    ", &
         unit=nuopcMsg)
    call ESMF_LogWrite(nuopcMsg, ESMF_LOGMSG_INFO)
    !
    ! Create GFS clock and state instances
    clock_gfs=ESMF_ClockCreate(clock, rc=RC)
    ESMF_ERR_RETURN(RC,RC)
    gfsImportState=ESMF_StateCreate(rc=rc)
    ESMF_ERR_RETURN(RC,RC)
    gfsExportState=ESMF_StateCreate(rc=rc)
    ESMF_ERR_RETURN(RC,RC)

    !
    ! set gsm configure file
    CF=ESMF_ConfigCreate(rc=RC)
    CALL ESMF_ConfigLoadFile(config=CF ,filename='atmos.configure' ,rc=RC)
    
!
    ! gsm grid component, import and export are created in earth grid component
    ! call gsm initialization
    CALL gfs_initialize(gcomp,                    &
                        gfsImportState,              &
                        gfsExportState,              &
                        clock_gfs,                &
                        rc_init=RC)

    !
    !Under NUOPC, the EARTH driver clock is a separate instance from the
    ! - GSM clock. However, the GSM clock may have been reset during GSM initialize
    ! - and therefore the EARTH driver clock must also be adjusted.
    ! - Affected: currTime, timeStep
      call ESMF_ClockGet(clock_gfs, currTime=currTime, &
        stopTime=stopTime, rc=RC)
      ESMF_ERR_RETURN(RC,RC)
      call ESMF_ClockGet(clock, timeStep=earthStep, rc=RC)
      ESMF_ERR_RETURN(RC,RC)

      if (earthStep>(stopTime-currTime)) earthStep=stopTime-currTime
      call ESMF_ClockSet(clock, currTime=currTime, &
        timeStep=earthStep, rc=RC)
      ESMF_ERR_RETURN(RC,RC)

      ! Set GSM component clock as copy of EARTH clock.
      call NUOPC_CompSetClock(gcomp, clock, rc=RC)
      ESMF_ERR_RETURN(RC,RC)

      ! Read in the GSM coupling interval
      call ESMF_ConfigGetAttribute(CF, medAtmCouplingIntervalSec, &
        label="atm_coupling_interval_sec:", default=-1.0_ESMF_KIND_R8, &
        rc=RC)
      ESMF_ERR_RETURN(RC,RC)

      if (medAtmCouplingIntervalSec>0._ESMF_KIND_R8) then
        ! The coupling time step was provided
        call ESMF_TimeIntervalSet(gsmStep, s_r8=medAtmCouplingIntervalSec, &
          rc=RC)
        ESMF_ERR_RETURN(RC,RC)
        call ESMF_GridCompGet(gcomp, clock=gsmClock, rc=RC)
        ESMF_ERR_RETURN(RC,RC)
        call ESMF_ClockSet(gsmClock, timestep=gsmStep, rc=RC)
        ESMF_ERR_RETURN(RC,RC)
      endif

    !
    ! use the regular Gaussian Grid that was setup during the NEMS/GSM init
    gridIn  = gauss2d ! for imported Fields
    gridOut = gauss2d ! for exported Fields

    ! conditionally realize or remove Fields from States ...

    do n = 1,nImportFields
      call realizeConnectedInternCplField(importState, &
        field=importFields(n), standardName=trim(importFieldsList(n)), &
        grid=gridIn, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo

    do n = 1,nExportFields
      call realizeConnectedInternCplField(exportState, &
        field=exportFields(n), standardName=trim(exportFieldsList(n)), &
        grid=gridOut, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo

  contains  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
    subroutine realizeConnectedInternCplField(state, field, standardName, grid, rc)
      type(ESMF_State)                :: state
      type(ESMF_Field)                :: field
      character(len=*)                :: standardName
      type(ESMF_Grid)                 :: grid
      integer, intent(out), optional  :: rc
      
      ! local variables
      character(len=80)               :: fieldName
      type(ESMF_ArraySpec)            :: arrayspec
      integer                         :: i

      if (present(rc)) rc = ESMF_SUCCESS
      
      fieldName = standardName  ! use standard name as field name

      !! Create fields using wam2dmesh if they are WAM fields 
      if (NUOPC_IsConnected(state, fieldName=fieldName)) then
         if (fieldName == "northward_wind_neutral" .or. &
             fieldName == "eastward_wind_neutral" .or. &
             fieldName == "upward_wind_neutral" .or. &
             fieldName == "temp_neutral" .or. &
             fieldName == "O_Density" .or. &
             fieldName == "O2_Density" .or. &
             fieldName == "N2_Density" .or. &
             fieldName == "height")  then
           call ESMF_ArraySpecSet(arrayspec,2,ESMF_TYPEKIND_R8, rc=rc)
           if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
           field = ESMF_FieldCreate(wam2dmesh, arrayspec, &
              ungriddedLBound=(/1/), ungriddedUBound=(/wamlevels/),&
              name=fieldName, rc=rc)
           if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
         else
           ! realize the connected Field pass back up for internal cpl fields
           field = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R8, name=fieldName, rc=rc)
           if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
             line=__LINE__, &
             file=__FILE__)) &
             return  ! bail out
         endif
         call NUOPC_Realize(state, field=field, rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
      else
        ! remove a not connected Field from State
        call ESMF_StateRemove(state, (/fieldName/), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
    end subroutine

  end subroutine
!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!

  subroutine GSM_DATAINIT(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_State)              :: exportState

    rc = ESMF_SUCCESS
    
    ! the GSM initializes export Fields that the MED initialize depends on

    ! query the Component for its exportState
    call ESMF_GridCompGet(gcomp, exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! -> set Updated Field Attribute to "true", indicating to the IPDv02p5
    ! generic code to set the timestamp for this Field
    
    call setAllFieldsUpdated(exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! -> set InitializeDataComplete Component Attribute to "true", indicating
    ! to the driver that this Component has fully initialized its data
    call NUOPC_CompAttributeSet(gcomp, &
      name="InitializeDataComplete", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
        
  contains  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
    subroutine setAllFieldsUpdated(state, rc)
      type(ESMF_State)                :: state
      integer, intent(out), optional  :: rc
      
      integer                         :: i, fieldCount
      character(len=80), allocatable  :: fieldNameList(:)
      type(ESMF_Field)                :: field
      type(ESMF_StateItem_Flag)       :: itemType
      real(ESMF_KIND_R8), pointer     :: fptr(:,:)

      if (present(rc)) rc = ESMF_SUCCESS
      
      call ESMF_StateGet(State, itemCount=fieldCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      allocate(fieldNameList(fieldCount))
      call ESMF_StateGet(State, itemNameList=fieldNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      
      do i=1, fieldCount
        call ESMF_StateGet(state, itemName=fieldNameList(i), &
          itemType=itemType, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        if (itemType /= ESMF_STATEITEM_NOTFOUND) then
          ! item exists -> set "Updated"
          call ESMF_StateGet(state, itemName=fieldNameList(i), field=field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
	  call ESMF_FieldGet(field, farrayPtr=fptr, rc=rc)
	  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          fptr=0.d0 ! zero out the entire field
          call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        endif
      enddo
    end subroutine
    
  end subroutine
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!
      SUBROUTINE GSM_ADVANCE(gcomp, rc)
!
!-----------------------------------------------------------------------
!***  Advance the GSM component.
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      type(ESMF_GridComp)   :: gcomp
      integer, intent(out)  :: rc
!
!---------------------
!***  Local Variables
!---------------------
!
      type(ESMF_Clock)              :: clock
      type(ESMF_Time)               :: stopTime, currTime
      type(ESMF_State)              :: importState, exportState
      type(ESMF_Field)              :: field
      type(ESMF_StateItem_Flag)     :: itemType
      
      !TODO: move the slice counter into an internal state to be instance safe
      integer, save                 :: slice=1
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
    if(profile_memory) call ESMF_VMLogMemInfo("Entering GSM GSM_ADVANCE ")
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!***  Use the internal Clock set by NUOPC layer for GSM but update stopTime
!-----------------------------------------------------------------------

      ! Component internal Clock gets updated per NUOPC rules
      call ESMF_GridCompGet(gcomp, clock=clock, rc=rc)
      ESMF_ERR_RETURN(rc,rc)
      
      ! The stopTime will be updated to be the next GSM-OCN coupling time
      call ESMF_ClockGet(clock, currTime=currTime, stopTime=stopTime, rc=rc)
      ESMF_ERR_RETURN(rc,rc)
      
      ! Set the GSM-OCN coupling time to be stopTime in Clock that GSM core uses
      call ESMF_ClockSet(clock_gfs, currTime=currTime, &
        stopTime=stopTime, rc=rc)
      ESMF_ERR_RETURN(rc,rc)

      call ESMF_ClockPrint(clock_gfs, options="currTime", &
        preString="entering GSM_ADVANCE with clock_gfs current: ", &
        unit=nuopcMsg)
      call ESMF_LogWrite(nuopcMsg, ESMF_LOGMSG_INFO)
      call ESMF_ClockPrint(clock_gfs, options="startTime", &
        preString="entering GSM_ADVANCE with clock_gfs start:   ", &
        unit=nuopcMsg)
      call ESMF_LogWrite(nuopcMsg, ESMF_LOGMSG_INFO)
      call ESMF_ClockPrint(clock_gfs, options="stopTime", &
        preString="entering GSM_ADVANCE with clock_gfs stop:    ", &
        unit=nuopcMsg)
      call ESMF_LogWrite(nuopcMsg, ESMF_LOGMSG_INFO)

      ! query the Component for its importState and exportState
      call ESMF_GridCompGet(gcomp, exportState=exportState, &
        importState=importState, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
!-----------------------------------------------------------------------
!***  Execute the Run step of the selected dynamic core.
!-----------------------------------------------------------------------

      if(profile_memory) call ESMF_VMLogMemInfo("Entering GSM GridCompRun ")
      CALL gfs_run(gcomp,                           &
                   gfsImportState,                     &
                   gfsExportState,                     &
                   clock_gfs,                       &
                   rc_run=rc)
      ESMF_ERR_RETURN(rc,rc)
      if(profile_memory) call ESMF_VMLogMemInfo("Leaving GSM GridCompRun ")

!-----------------------------------------------------------------------

      ! diagnostics     
      if(write_diagnostics) then
        ! for testing write all of the Fields in the importState to file
        call NUOPC_Write(importState, fileNamePrefix="field_atm_import_", &
          timeslice=slice, relaxedFlag=.true., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        ! for testing write all of the Fields in the exportState to file
        call NUOPC_Write(exportState, fileNamePrefix="field_atm_export_", &
          timeslice=slice, relaxedFlag=.true., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        ! advance the time slice counter
        slice = slice + 1
      endif

      call ESMF_ClockPrint(clock_gfs, options="currTime",             &
        preString="leaving  GSM_ADVANCE with clock_gfs current: ", &
        unit=nuopcMsg)
      call ESMF_LogWrite(nuopcMsg, ESMF_LOGMSG_INFO)
      call ESMF_ClockPrint(clock_gfs, options="startTime", &
        preString="leaving  GSM_ADVANCE with clock_gfs start:   ", &
        unit=nuopcMsg)
      call ESMF_LogWrite(nuopcMsg, ESMF_LOGMSG_INFO)
      call ESMF_ClockPrint(clock_gfs, options="stopTime", &
        preString="leaving  GSM_ADVANCE with clock_gfs stop:    ", &
        unit=nuopcMsg)
      call ESMF_LogWrite(nuopcMsg, ESMF_LOGMSG_INFO)
    if(profile_memory) call ESMF_VMLogMemInfo("Leaving GSM GSM_ADVANCE ")

!-----------------------------------------------------------------------

      END SUBROUTINE GSM_ADVANCE
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------

      SUBROUTINE GSM_CHECKIMPORT(gcomp, rc)
!
!-----------------------------------------------------------------------
!***  Check the import state fields
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      type(ESMF_GridComp)   :: gcomp
      integer, intent(out)  :: rc
!
!---------------------
!***  Local Variables
!---------------------
!
      integer :: n, nf
      type(ESMF_Clock)   :: clock
      type(ESMF_Time)    :: currTime, invalidTime
      type(ESMF_State)   :: importState
      logical            :: timeCheck1,timeCheck2
      type(ESMF_Field),pointer  :: fieldList(:)
      character(len=128) :: fldname

      ! query the Component for its clock
      call ESMF_GridCompGet(gcomp, clock=clock, &
         importState=importState, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! get the current time out of the clock
      call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! set up invalid time (by convention)
      call ESMF_TimeSet(invalidTime, yy=99999999, mm=01, dd=01, &
        h=00, m=00, s=00, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      nullify(fieldList)
      call NUOPC_GetStateMemberLists(importState, &
        fieldList=fieldList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      !--------------------------------
      ! set the importFieldsValid flag
      ! have to be VERY CAREFUL here, the order of fieldList does NOT
      ! match the order of importFieldsList.
      ! associated(fieldList) will be false if there are no fields
      !--------------------------------

      importFieldsValid(:) = .true.
      if (associated(fieldList)) then
      do n = 1,size(fieldList)
        call ESMF_FieldGet(fieldList(n), name=fldname, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        nf = QueryFieldList(ImportFieldsList,fldname)
        timeCheck1 = NUOPC_IsAtTime(fieldList(n), invalidTime, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        if (timeCheck1) then
          importFieldsValid(nf) = .false.
        else
          timeCheck2 = NUOPC_IsAtTime(fieldList(n), currTime, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          if (.not.timeCheck2) then
            !TODO: introduce and use INCOMPATIBILITY return codes!!!!
            call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
              msg="NUOPC INCOMPATIBILITY DETECTED: "//&
              "Import Field not at current time", &
              line=__LINE__, file=__FILE__, &
              rcToReturn=rc)
              return  ! bail out
          endif
        endif
        write(MESSAGE_CHECK,'(A,2i4,l3)') &
          "GSM_CHECKIMPORT "//trim(fldname),n,nf,importFieldsValid(nf)
        CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
      enddo
      endif

!-----------------------------------------------------------------------

      END SUBROUTINE GSM_CHECKIMPORT
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!

      SUBROUTINE GSM_CAP_FINALIZE(gcomp                             &
                             ,IMP_STATE                                 &
                             ,EXP_STATE                                 &
                             ,CLOCK_EARTH                               &
                             ,RC_FINALIZE)
!
!-----------------------------------------------------------------------
!***  CAP for Finalize the GSM component.
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      TYPE(ESMF_GridComp)               :: gcomp                   !<-- The GSM component
      TYPE(ESMF_State)                  :: IMP_STATE                       !<-- The GSM import state
      TYPE(ESMF_State)                  :: EXP_STATE                       !<-- The GSM import state
      TYPE(ESMF_Clock)                  :: CLOCK_EARTH                     !<-- The Clock of the EARTH component
      INTEGER            ,INTENT(OUT)   :: RC_FINALIZE                     !<-- Error return code
!
!---------------------
!***  Local Variables
!---------------------
!
      INTEGER :: RC
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Execute the Finalize step of the CORE component"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL gfs_finalize(gcomp,      &
                        gfsImportState,  &
                        gfsExportState,  &
                        clock_gfs,  &
                        rc_final=RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_FINALIZE)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!
      call ESMF_ClockDestroy(clock_gfs, rc=RC_FINALIZE)
      if (ESMF_LogFoundError(rcToCheck=RC_FINALIZE, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      IF(RC_FINALIZE==ESMF_SUCCESS)THEN
        WRITE(0,*)' GSM_FINALIZE succeeded'
      ELSE
        WRITE(0,*)' GSM_FINALIZE failed  RC_FINALIZE=',RC_FINALIZE
      ENDIF
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE GSM_CAP_FINALIZE
!
!-----------------------------------------------------------------------
!
      END MODULE module_GSM_CAP
!
