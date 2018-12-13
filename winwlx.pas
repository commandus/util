unit
  winwlx;
(*##*)
(*******************************************************************************
*                                                                             *
*   W  I  N  W  L  X                                                           *
*                                                                             *
*   Copyright © 2007 Andrei Ivanov. All rights reserved.                       *
*   WinLogon and GINA pascal wrapper                                          *
*     WLX == WinLogon eXtension                                                *
*                                                                             *
*     This file contains definitions, data types, and routine prototypes       *
*     necessary to produce a replacement Graphical Identification aNd         *
*     Authentication (GINA) DLL for Winlogon.                                  *
*     Richard Ward (RichardW) and Jim Kelly (JimK) May-1994                   *
*                                                                              *
*                                                                             *
*   Conditional defines:                                                       *
*                                                                             *
*   Revisions    : Mar 31 2007                                                 *
*   Last revision: Mar 31 2007                                                *
*   Lines        : 467                                                         *
*   History      :                                                            *
*   Printed      : ---                                                         *
*                                                                             *
********************************************************************************)
(*##*)

interface
uses
  Windows, Messages;

const
  WLX_VERSION_1_0 = $00010000;
  WLX_VERSION_1_1 = $00010001;
  WLX_VERSION_1_2 = $00010002;
  WLX_VERSION_1_3 = $00010003;
  WLX_VERSION_1_4 = $00010004; // ?!! not supported!  

  WLX_DIRECTORY_LENGTH = 127; // ?!!

{ Secure attention sequence types
  These values are passed to routines that have a dwSasType  parameter.
   ALL VALUES FROM 0 TO 127 ARE RESERVED FOR MICROSOFT DEFINITION.
   VALUES ABOVE 127 ARE RESERVED FOR CUSTOMER DEFINITION.
       CTRL_ALT_DEL - used to indicate that the standard ctrl-alt-del
           secure attention sequence has been entered.
       SCRNSVR_TIMEOUT - used to indicate that keyboard/mouse inactivity
           has lead to a screensaver activation.  It is up to the GINA
           DLL whether this constitutes a workstation locking event.
       SCRNSVR_ACTIVITY - used to indicate that keyboard or mouse
           activity occured while a secure screensaver was active.
}
  WLX_SAS_TYPE_TIMEOUT = 0;
  WLX_SAS_TYPE_CTRL_ALT_DEL = 1;
  WLX_SAS_TYPE_SCRNSVR_TIMEOUT = 2;
  WLX_SAS_TYPE_SCRNSVR_ACTIVITY = 3;
  WLX_SAS_TYPE_USER_LOGOFF = 4;
  WLX_SAS_TYPE_MAX_MSFT_VALUE = 127;

{ Upon successful logon, the GINA DLL may specify any of the following
  options to Winlogon (via the dwOptions parameter of the WlxLoggedOutSas()
  api). When set, these options specify:
      NO_PROFILE -     Winlogon must NOT load a profile for the logged
                       on user.  Either the GINA DLL will take care of
                       this activity, or the user does not need a profile.
      USE_GROUPPOLICY - Apply group policy templates to the user.
}
  WLX_LOGON_OPT_NO_PROFILE = $00000001;
  WLX_LOGON_OPT_USE_GROUPPOLICY = $00000002;

{
  GINA DLLs are expected to return account information to Winlogon
  following a successful logon.  This information allows Winlogon
  to support profile loading and supplemental network providers.
  To allow different sets of profile information to be returned
  by GINAs over time, the first DWORD of each profile structure
  is expected to contain a type-identifier.  The following constants
  are the defined profile type identifiers.
  Standard profile is V2_0
}
  WLX_PROFILE_TYPE_V1_0 = 1;
  WLX_PROFILE_TYPE_V2_0 = 2;

{
  WlxLoggedOnSas() and WlxWkstaLockedSas() return an action
  value to Winlogon directing Winlogon to either remain unchanged
  or to perform some action (such as force-log the user off).
  These are the values that may be returned.  Note, however, that
  not all of the values may be returned by both of these api.  See
  the description of each api to see which values are expected from
  each.

  LOGON              - User has logged on
  NONE               - Don't change the state of the window station.
  LOCK_WKSTA         - Lock the workstation, wait for next SAS.
  LOGOFF             - Log the user off of the workstation.
  SHUTDOWN           - Log the user off and shutdown the machine.
  PWD_CHANGED        - Indicates that the user changed their password.  Notify network providers.
  TASKLIST           - Invoke the task list.
  UNLOCK_WKSTA       - Unlock the workstation.
  FORCE_LOGOFF       - Forcibly log the user off.
  SHUTDOWN_POWER_OFF - Turn off machine after shutting down.
  SHUTDOWN_REBOOT    - Reboot machine after shutting down.
  SHUTDOWN_SLEEP     - Put the machine to sleep
  SHUTDOWN_SLEEP2    - Put the machine to sleep and disable wakeup events
  SHUTDOWN_HIBERNATE - Hibernate the machine
}
  WLX_SAS_ACTION_LOGON = 1;
  WLX_SAS_ACTION_NONE = 2;
  WLX_SAS_ACTION_LOCK_WKSTA = 3;
  WLX_SAS_ACTION_LOGOFF = 4;
  WLX_SAS_ACTION_SHUTDOWN = 5;
  WLX_SAS_ACTION_PWD_CHANGED = 6;
  WLX_SAS_ACTION_TASKLIST = 7;
  WLX_SAS_ACTION_UNLOCK_WKSTA = 8;
  WLX_SAS_ACTION_FORCE_LOGOFF = 9;
  WLX_SAS_ACTION_SHUTDOWN_POWER_OFF = 10;
  WLX_SAS_ACTION_SHUTDOWN_REBOOT = 11;
  WLX_SAS_ACTION_SHUTDOWN_SLEEP = 12;
  WLX_SAS_ACTION_SHUTDOWN_SLEEP2 = 13;
  WLX_SAS_ACTION_SHUTDOWN_HIBERNATE = 14;

  // Window Messages. The WM_SAS is defined as follows
  // The wParam parameter has the SAS Type (above)
  WLX_WM_SAS = WM_USER + 601;

  // Dialog return values. These may be returned by dialogs started by a GINA dll.
  WLX_DLG_SAS = 101;
  // Input (keys, etc) timed out
  WLX_DLG_INPUT_TIMEOUT = 102;
  // Screen Saver activated
  WLX_DLG_SCREEN_SAVER_TIMEOUT = 103;
  // User logged off
  WLX_DLG_USER_LOGOFF = 104;

  // Name present
  WLX_DESKTOP_NAME = $00000001;
  // Handle present
  WLX_DESKTOP_HANDLE = $00000002;

  WLX_CREATE_INSTANCE_ONLY = $00000001;
  WLX_CREATE_USER = $00000002;

type
  LUID = record
  case Integer of
    0: (
      LowPart: DWORD;
      HighPart: LongInt;
      );
    1: (Int: Int64);
  end;
  PLUID = ^LUID;

  LARGE_INTEGER = record
  case Integer of
    0: (
      LowPart: DWORD;
      HighPart: LongInt;
      );
    1: (Int: Int64);
  end;
  PLARGE_INTEGER = ^LARGE_INTEGER;

  SIZE_T = LongWord; // ?!!
  {
  The WLX_PROFILE_* structure is returned from a GINA DLL
  following authentication.  This information is used by Winlogon
  to support supplemental Network Providers and to load the
  newly logged-on user's profile.

  Winlogon is responsible for freeing both the profile structure
  and the fields within the structure that are marked as separately
  deallocatable.

  This field identifies the type of profile being returned by a
  GINA DLL.  Profile types are defined with the prefix
  WLX_PROFILE_TYPE_xxx.  It allows Winlogon to typecast the
  structure so the remainder of the structure may be referenced.

  pathname of profile to load for user.
  The buffer pointed to by this field must be separately allocated.
  Winlogon will free the buffer when it is no longer needed.
  }

type
  TWLX_PROFILE_V1_0 = record
    dwType: DWORD;
    pszProfile: PWideChar;
  end;
  PWLX_PROFILE_V1_0 = ^TWLX_PROFILE_V1_0;

  {
  This field identifies the type of profile being returned by a
  GINA DLL.  Profile types are defined with the prefix
  WLX_PROFILE_TYPE_xxx.  It allows Winlogon to typecast the
  structure so the remainder of the structure may be referenced.
  pathname of profile to load for user.
  This parameter can be NULL.  If so, the user has a local
  profile only.
  The buffer pointed to by this field must be separately allocated.
  Winlogon will free the buffer when it is no longer needed.
  pathname of policy to load for user.
  This parameter can be NULL which prevents network wide policy
  from being applied.
  The buffer pointed to by this field must be separately allocated.
  Winlogon will free the buffer when it is no longer needed.
  pathname of network default user profile
  This parameter can be NULL, which causes the Default User
  profile on the local machine to be used.
  The buffer pointed to by this field must be separately allocated.
  Winlogon will free the buffer when it is no longer needed.
  name of the server which validated the user account
  This is used to enumerate globals groups the user belongs
  to for policy support.  This parameter can be NULL.
  The buffer pointed to by this field must be separately allocated.
  Winlogon will free the buffer when it is no longer needed.
  pointer to a series of null terminated environment variables
  envname=environment variable value
   - or -
  envname=%OtherVar%\more text
  Each environment variable is NULL terminated with the last
  environment variable double NULL terminated.  These variables
  are set into the user's initial environment.  The environment
  variable value can contain other environment variables wrapped
  in "%" signs. This parameter can be NULL.
  The buffer pointed to by this field must be separately allocated.
  Winlogon will free the buffer when it is no longer needed.
}

  TWLX_PROFILE_V2_0 = record
    dwType: DWORD;
    Profile: PWideChar;
    Policy: PWideChar;
    NetworkDefaultUserProfile: PWideChar;
    ServerName: PWideChar;
    Environment: PWideChar;
  end;
  PWLX_PROFILE_V2_0 = ^TWLX_PROFILE_V2_0;

  TQUOTA_LIMITS = record
    PagedPoolLimit,
      NonPagedPoolLimit,
      MinimumWorkingSetSize,
      MaximumWorkingSetSize,
      PagefileLimit: SIZE_T;
    TimeLimit: LARGE_INTEGER;
  end;

  TWLX_CLIENT_CREDENTIALS_INFO_1_0 = record
    dwType: DWORD;
    UserName,
    Domain,
    Password: PWideChar;
    PromptForPassword: Boolean;
  end;
  PWLX_CLIENT_CREDENTIALS_INFO_1_0 = ^TWLX_CLIENT_CREDENTIALS_INFO_1_0;

  TWLX_CLIENT_CREDENTIALS_INFO_2_0 = record
    dwType: DWORD;
    UserName,
    Domain,
    Password: PWideChar;
    PromptForPassword,
    DisconnectOnLogonFailure: Boolean;
  end;
  PWLX_CLIENT_CREDENTIALS_INFO_2_0 = ^TWLX_CLIENT_CREDENTIALS_INFO_2_0;

  TWLX_CONSOLESWITCH_CREDENTIALS_INFO_1_0 = record
    dwType: DWORD;
    UserToken: THANDLE;
    LogonId: LUID;
    Quotas: TQUOTA_LIMITS;
    UserName,
    Domain: PWideChar;
    LogonTime: LARGE_INTEGER;
    SmartCardLogon: Bool;
    ProfileLength: Cardinal;
    MessageType: DWORD;
    LogonCount: Byte;
    BadPasswordCount: Byte;
    ProfileLogonTime,
    LogoffTime,
    KickOffTime,
    PasswordLastSet,
    PasswordCanChange,
    PasswordMustChange: LARGE_INTEGER;
    LogonScript,
    HomeDirectory,
    FullName,
    ProfilePath,
    HomeDirectoryDrive,
    LogonServer: PWideChar;
    UserFlags,
    PrivateDataLen: Cardinal;
    PrivateData: Pointer;
  end;
  PWLX_CONSOLESWITCH_CREDENTIALS_INFO_1_0 = ^TWLX_CONSOLESWITCH_CREDENTIALS_INFO_1_0;

  TWLX_TERMINAL_SERVICES_DATA = record
    ProfilePatharray: array [0..WLX_DIRECTORY_LENGTH] of WideChar;
    HomeDir: array [0..WLX_DIRECTORY_LENGTH] of WideChar;
    HomeDirDrive: array [0..3] of WideChar;
  end;
  PWLX_TERMINAL_SERVICES_DATA = ^TWLX_TERMINAL_SERVICES_DATA;

  {
  The WLX_NPR_NOTIFICATION_INFO structure is returned
  from a GINA DLL following successful authentication.
  This information is used by Winlogon to provide
  identification and authentication information already
  collected to network providers.  Winlogon is
  responsible for freeing both the main structure and all
  string and other buffers pointed to from within the
  structure.
  The name of the account logged onto (e.g. REDMOND\Joe).
  The string pointed to by this field must be separately
  allocated and will be separately deallocated by Winlogon.
  The string pointed to by this field must be separately
  allocated and will be separately deallocated by Winlogon.
  Cleartext password of the user account.  If the OldPassword
  field is non-null, then this field contains the new password
  in a password change operation.  The string pointed to by
  this field must be separately allocated and will be seperately
  deallocated by Winlogon.
  Cleartext old password of the user account whose password
  has just been changed.  The Password field contains the new
  password.  The string pointed to by this field must be
  separately allocated and will be separately deallocated by
  Winlogon.
  }
  TWLX_MPR_NOTIFY_INFO = record
    UserName,
    Domain,
    Password,
    OldPassword: PWideChar;
  end;
  PWLX_MPR_NOTIFY_INFO = ^TWLX_MPR_NOTIFY_INFO;

  TWLX_DESKTOP = record
    Size: DWORD;
    Flags: DWORD;
    hDesktop: HDESK;
    pszDesktopName: PWideChar;
  end;
  PWLX_DESKTOP = ^TWLX_DESKTOP;

  TDLGPROC = function (Wnd: HWnd; Msg: UINT; WParam: WPARAM; LParam: LPARAM): UINT; stdcall;
  TDLGTEMPLATE = record
    style,
    dwExtendedStyle: DWORD;
    cdit: Word;
    x,
    y,
    cx,
    cy: SmallInt;
  end;
  PDLGTEMPLATE = ^TDLGTEMPLATE;

  PWLX_USE_CTRL_ALT_DEL = procedure (AWlx: THandle); stdcall;
  PWLX_SET_CONTEXT_POINTER = procedure (AWlx: THandle; AWlxContext: Pointer); stdcall;
  PWLX_SAS_NOTIFY = procedure (AWlx: THandle; ASASType: DWORD); stdcall;
  PWLX_SET_TIMEOUT = function (AWlx: THandle; ATimeOut: DWORD): Bool; stdcall;
  PWLX_ASSIGN_SHELL_PROTECTION = function (AWlx: THandle; AToken, AProcess, AThread: THandle): Integer; stdcall;
  PWLX_MESSAGE_BOX = function (AWlx: THandle; AOwner: HWND; AText, ATitle: PWideChar; AStyle: UINT): Integer; stdcall;
  PWLX_DIALOG_BOX = function (AWlx: THandle; AInst: THandle; ATemplate: PWideChar; AOwner: HWND; ADlgProc: TDLGPROC): Integer; stdcall;
  PWLX_DIALOG_BOX_PARAM = function (AWlx: THandle; AInst: THandle; ATemplate: PWideChar; AOwner: HWND; ADlgProc: TDLGPROC; AInitParam: LongInt): Integer; stdcall;
  PWLX_DIALOG_BOX_INDIRECT = function (AWlx: THandle; AInst: THandle; ATemplate: PDLGTEMPLATE; AOwner: HWND; ADlgProc: TDLGPROC): Integer; stdcall;
  PWLX_DIALOG_BOX_INDIRECT_PARAM = function (AWlx: THandle; AInst: THandle; ATemplate: PDLGTEMPLATE; AOwner: HWND; ADlgProc: TDLGPROC; AInitParam: LongInt): Integer; stdcall;
  PWLX_SWITCH_DESKTOP_TO_USER = function (AWlx: THandle): Integer; stdcall;
  PWLX_SWITCH_DESKTOP_TO_WINLOGON = function (AWlx: THandle): Integer; stdcall;
  PWLX_CHANGE_PASSWORD_NOTIFY = function (AWlx: THandle; AMprInfo: PWLX_MPR_NOTIFY_INFO; AChangeInfo: DWORD): Integer; stdcall;
  PWLX_GET_SOURCE_DESKTOP = function (AWlx: THandle; var ADesktop: PWlx_Desktop): Integer; stdcall;
  PWLX_SET_RETURN_DESKTOP = function (AWlx: THandle; ADesktop: PWlx_Desktop): Integer; stdcall;
  PWLX_CREATE_USER_DESKTOP = function (AWlx, AToken: THandle; AFlags: DWORD; ADesktopName: PWideChar;
    var ADesktop: PWLX_DESKTOP): Bool; stdcall;
  PWLX_CHANGE_PASSWORD_NOTIFY_EX = function (AWlx: THandle; AMprInfo: PWLX_MPR_NOTIFY_INFO; AChangeInfo: DWORD;
    AProviderName: PWideChar; AReserved: Pointer): Integer; stdcall;
  PWLX_CLOSE_USER_DESKTOP = function (AWlx: THandle; ADesktop: PWlx_Desktop; AToken: THandle): BOOL; stdcall;
  PWLX_SET_OPTION = function (AWlx: THandle; AOption: DWORD; ANewValue: Pointer; var AOldValue: Pointer): Bool; stdcall;
  PWLX_GET_OPTION = function (AWlx: THandle; AOption: DWORD; var AValue: Pointer): Bool; stdcall;
  PWLX_WIN31_MIGRATE = procedure (AWlx: THandle); stdcall;
  PWLX_QUERY_CLIENT_CREDENTIALS = function (ACred: PWLX_CLIENT_CREDENTIALS_INFO_1_0): Bool; stdcall;
  PWLX_QUERY_IC_CREDENTIALS = function (ACred: PWLX_CLIENT_CREDENTIALS_INFO_1_0): Bool; stdcall;
  PWLX_DISCONNECT = function : Bool; stdcall;
  PWLX_QUERY_TERMINAL_SERVICES_DATA = function(AWlx: THandle; ATSData: PWLX_TERMINAL_SERVICES_DATA; AUserName, ADomain: PWideChar): DWORD; stdcall;
  PWLX_QUERY_CONSOLE_SWITCH_CREDENTIALS = function(ACred: PWLX_CONSOLESWITCH_CREDENTIALS_INFO_1_0): Bool; stdcall;
  PWLX_QUERY_TS_LOGON_CREDENTIALS = function (ACred: PWLX_CLIENT_CREDENTIALS_INFO_2_0): Bool; stdcall;

  TWLX_DISPATCH_VERSION_1_0 = record
    WlxUseCtrlAltDel : PWLX_USE_CTRL_ALT_DEL;
    WlxSetContextPointer : PWLX_SET_CONTEXT_POINTER;
    WlxSasNotify : PWLX_SAS_NOTIFY;
    WlxSetTimeout : PWLX_SET_TIMEOUT;
    WlxAssignShellProtection : PWLX_ASSIGN_SHELL_PROTECTION;
    WlxMessageBox : PWLX_MESSAGE_BOX;
    WlxDialogBox : PWLX_DIALOG_BOX;
    WlxDialogBoxParam : PWLX_DIALOG_BOX_PARAM;
    WlxDialogBoxIndirect : PWLX_DIALOG_BOX_INDIRECT;
    WlxDialogBoxIndirectParam : PWLX_DIALOG_BOX_INDIRECT_PARAM;
    WlxSwitchDesktopToUser : PWLX_SWITCH_DESKTOP_TO_USER;
    WlxSwitchDesktopToWinlogon : PWLX_SWITCH_DESKTOP_TO_WINLOGON;
    WlxChangePasswordNotify : PWLX_CHANGE_PASSWORD_NOTIFY;
  end;
  PWLX_DISPATCH_VERSION_1_0 = ^PWLX_DISPATCH_VERSION_1_0;

  TWLX_DISPATCH_VERSION_1_1 = record
    WlxUseCtrlAltDel : PWLX_USE_CTRL_ALT_DEL;
    WlxSetContextPointer : PWLX_SET_CONTEXT_POINTER;
    WlxSasNotify : PWLX_SAS_NOTIFY;
    WlxSetTimeout : PWLX_SET_TIMEOUT;
    WlxAssignShellProtection : PWLX_ASSIGN_SHELL_PROTECTION;
    WlxMessageBox : PWLX_MESSAGE_BOX;
    WlxDialogBox : PWLX_DIALOG_BOX;
    WlxDialogBoxParam : PWLX_DIALOG_BOX_PARAM;
    WlxDialogBoxIndirect : PWLX_DIALOG_BOX_INDIRECT;
    WlxDialogBoxIndirectParam : PWLX_DIALOG_BOX_INDIRECT_PARAM;
    WlxSwitchDesktopToUser : PWLX_SWITCH_DESKTOP_TO_USER;
    WlxSwitchDesktopToWinlogon : PWLX_SWITCH_DESKTOP_TO_WINLOGON;
    WlxChangePasswordNotify : PWLX_CHANGE_PASSWORD_NOTIFY;
    // 1.1
    WlxGetSourceDesktop : PWLX_GET_SOURCE_DESKTOP;
    WlxSetReturnDesktop : PWLX_SET_RETURN_DESKTOP;
    WlxCreateUserDesktop : PWLX_CREATE_USER_DESKTOP;
    WlxChangePasswordNotifyEx : PWLX_CHANGE_PASSWORD_NOTIFY_EX;
  end;
  PWLX_DISPATCH_VERSION_1_1 = ^TWLX_DISPATCH_VERSION_1_1;

  TWLX_DISPATCH_VERSION_1_2 = record
    WlxUseCtrlAltDel: PWLX_USE_CTRL_ALT_DEL;
    WlxSetContextPointer: PWLX_SET_CONTEXT_POINTER;
    WlxSasNotify: PWLX_SAS_NOTIFY;
    WlxSetTimeout: PWLX_SET_TIMEOUT;
    WlxAssignShellProtection: PWLX_ASSIGN_SHELL_PROTECTION;
    WlxMessageBox: PWLX_MESSAGE_BOX;
    WlxDialogBox: PWLX_DIALOG_BOX;
    WlxDialogBoxParam: PWLX_DIALOG_BOX_PARAM;
    WlxDialogBoxIndirect: PWLX_DIALOG_BOX_INDIRECT;
    WlxDialogBoxIndirectParam: PWLX_DIALOG_BOX_INDIRECT_PARAM;
    WlxSwitchDesktopToUser: PWLX_SWITCH_DESKTOP_TO_USER;
    WlxSwitchDesktopToWinlogon: PWLX_SWITCH_DESKTOP_TO_WINLOGON;
    WlxChangePasswordNotify: PWLX_CHANGE_PASSWORD_NOTIFY ;
    // 1.1
    WlxGetSourceDesktop: PWLX_GET_SOURCE_DESKTOP ;
    WlxSetReturnDesktop: PWLX_SET_RETURN_DESKTOP ;
    WlxCreateUserDesktop: PWLX_CREATE_USER_DESKTOP ;
    WlxChangePasswordNotifyEx: PWLX_CHANGE_PASSWORD_NOTIFY_EX ;
    // 1.2
    WlxCloseUserDesktop: PWLX_CLOSE_USER_DESKTOP ;
  end;
  PWLX_DISPATCH_VERSION_1_2 = ^TWLX_DISPATCH_VERSION_1_2;

  TWLX_DISPATCH_VERSION_1_3 = record
    WlxUseCtrlAltDel: PWLX_USE_CTRL_ALT_DEL;
    WlxSetContextPointer: PWLX_SET_CONTEXT_POINTER;
    WlxSasNotify: PWLX_SAS_NOTIFY;
    WlxSetTimeout: PWLX_SET_TIMEOUT;
    WlxAssignShellProtection: PWLX_ASSIGN_SHELL_PROTECTION;
    WlxMessageBox: PWLX_MESSAGE_BOX;
    WlxDialogBox: PWLX_DIALOG_BOX;
    WlxDialogBoxParam: PWLX_DIALOG_BOX_PARAM;
    WlxDialogBoxIndirect: PWLX_DIALOG_BOX_INDIRECT;
    WlxDialogBoxIndirectParam: PWLX_DIALOG_BOX_INDIRECT_PARAM;
    WlxSwitchDesktopToUser: PWLX_SWITCH_DESKTOP_TO_USER;
    WlxSwitchDesktopToWinlogon: PWLX_SWITCH_DESKTOP_TO_WINLOGON;
    WlxChangePasswordNotify: PWLX_CHANGE_PASSWORD_NOTIFY ;
    // 1.1
    WlxGetSourceDesktop: PWLX_GET_SOURCE_DESKTOP ;
    WlxSetReturnDesktop: PWLX_SET_RETURN_DESKTOP ;
    WlxCreateUserDesktop: PWLX_CREATE_USER_DESKTOP ;
    WlxChangePasswordNotifyEx: PWLX_CHANGE_PASSWORD_NOTIFY_EX ;
    // 1.2
    WlxCloseUserDesktop: PWLX_CLOSE_USER_DESKTOP ;
    // 1.3
    WlxSetOption: PWLX_SET_OPTION ;
    WlxGetOption: PWLX_GET_OPTION ;
    WlxWin31Migrate: PWLX_WIN31_MIGRATE ;
    WlxQueryClientCredentials: PWLX_QUERY_CLIENT_CREDENTIALS ;
    WlxQueryInetConnectorCredentials: PWLX_QUERY_IC_CREDENTIALS ;
    WlxDisconnect: PWLX_DISCONNECT ;
    WlxQueryTerminalServicesData: PWLX_QUERY_TERMINAL_SERVICES_DATA ;
  end;
  PWLX_DISPATCH_VERSION_1_3 = ^TWLX_DISPATCH_VERSION_1_3;

  TWLX_DISPATCH_VERSION_1_4 = record
    WlxUseCtrlAltDel: PWLX_USE_CTRL_ALT_DEL;
    WlxSetContextPointer: PWLX_SET_CONTEXT_POINTER;
    WlxSasNotify: PWLX_SAS_NOTIFY;
    WlxSetTimeout: PWLX_SET_TIMEOUT;
    WlxAssignShellProtection: PWLX_ASSIGN_SHELL_PROTECTION;
    WlxMessageBox: PWLX_MESSAGE_BOX ;
    WlxDialogBox: PWLX_DIALOG_BOX;
    WlxDialogBoxParam: PWLX_DIALOG_BOX_PARAM;
    WlxDialogBoxIndirect: PWLX_DIALOG_BOX_INDIRECT;
    WlxDialogBoxIndirectParam: PWLX_DIALOG_BOX_INDIRECT_PARAM;
    WlxSwitchDesktopToUser: PWLX_SWITCH_DESKTOP_TO_USER;
    WlxSwitchDesktopToWinlogon: PWLX_SWITCH_DESKTOP_TO_WINLOGON;
    WlxChangePasswordNotify: PWLX_CHANGE_PASSWORD_NOTIFY;
    // 1.1
    WlxGetSourceDesktop: PWLX_GET_SOURCE_DESKTOP;
    WlxSetReturnDesktop: PWLX_SET_RETURN_DESKTOP;
    WlxCreateUserDesktop: PWLX_CREATE_USER_DESKTOP;
    WlxChangePasswordNotifyEx: PWLX_CHANGE_PASSWORD_NOTIFY_EX;
    // 1.2
    WlxCloseUserDesktop: PWLX_CLOSE_USER_DESKTOP;
    // 1.3
    WlxSetOption: PWLX_SET_OPTION;
    WlxGetOption: PWLX_GET_OPTION;
    WlxWin31Migrate: PWLX_WIN31_MIGRATE;
    WlxQueryClientCredentials: PWLX_QUERY_CLIENT_CREDENTIALS;
    WlxQueryInetConnectorCredentials: PWLX_QUERY_IC_CREDENTIALS;
    WlxDisconnect: PWLX_DISCONNECT;
    WlxQueryTerminalServicesData: PWLX_QUERY_TERMINAL_SERVICES_DATA;
    // 1.4
    WlxQueryConsoleSwitchCredentials: PWLX_QUERY_CONSOLE_SWITCH_CREDENTIALS;
    WlxQueryTsLogonCredentials: PWLX_QUERY_TS_LOGON_CREDENTIALS;
  end;
  PWLX_DISPATCH_VERSION_1_4 = ^TWLX_DISPATCH_VERSION_1_4;

  TTOKEN_TYPE = (TokenPrimary = 1, TokenImpersonation);
  TSECURITY_IMPERSONATION_LEVEL = (SecurityAnonymous, SecurityIdentification,
    SecurityImpersonation, SecurityDelegation);

  TTOKEN_STATISTICS = record
    TokenId,
    AuthenticationId: LUID;
    ExpirationTime: UInt64;
    TokenType: TTOKEN_TYPE;
    ImpersonationLevel: TSECURITY_IMPERSONATION_LEVEL;
    DynamicCharged,
    DynamicAvailable,
    GroupCount,
    PrivilegeCount: DWORD;
    ModifiedId: LUID;
  end;
  PTOKEN_STATISTICS = ^TTOKEN_STATISTICS;


  TGINA_CONTEXT = record
    Wlx: THandle;
    Station: PWideChar;
    WlxFuncs: PWLX_DISPATCH_VERSION_1_3;
    DllInstance: THandle;
    UserToken: THandle;
  end;
  PGINA_CONTEXT = ^TGINA_CONTEXT;

{ WlxActivateUserShell
  Winlogon calls this function following a successful logon to request that
  the GINA activate the user's shell program.

  Parameters
  pWlxContext       Pointer to the GINA context associated with this window station. This is the context value that the GINA returns when Winlogon calls WlxInitialize for this station.
  pszDesktopName    name of desktop where the shell will start. Pass this string to the CreateProcess or CreateProcessAsUser function through the lpDesktop member of the STARTUPINFO structure.
  pszMprLogonScript any script names returned from the network provider DLLs. Network provider DLLs can return scripts to be executed during logon; however, the GINA may ignore them.
  pEnvironment      initial environment variables for the process. Winlogon creates a copy of the environment and hands it off to the GINA. The GINA can modify this environment before using it to initialize the user's shell. The GINA should call VirtualFree to free the memory allocated for pEnvironment.
  When FALSE is returned, Winlogon cancels the logon in process.
  Before calling WlxActivateUserShell, Winlogon sets:
    - current desktop is the Winlogon
    - desktop is locked
}

  TWlxActivateUserShell = function (AWlxContext: Pointer;
    ADesktopName, AMprLogonScript: PWideChar; AEnvironment: Pointer): Bool; stdcall;

{ WlxDisplayLockedNotice
  Winlogon calls this function when the workstation is placed in the locked state.
  WlxDisplayLockedNotice allows the GINA to display information about the lock,
  such as who locked the workstation and when it was locked.

  Parameters
  pWlxContext       Pointer to the GINA context associated with this window station. This is the context value that the GINA returns when Winlogon calls WlxInitialize for this station.

  To display lock information, the GINA must display a dialog box that will be interrupted
  by a WLX_WM_SAS message.
  Before calling WlxDisplayLockedNotice, Winlogon sets:
    - current desktop is the Winlogon
    - desktop is locked
}

  TWlxDisplayLockedNotice = procedure (AWlxContext: Pointer); stdcall;

{ WlxDisplaySASNotice
  Winlogon calls this function when the workstation is placed in the locked state.
  Parameters
  pWlxContext       Pointer to the GINA context associated with this window station. This is the context value that the GINA returns when Winlogon calls WlxInitialize for this station.

  Before calling WlxDisplaySASNotice, Winlogon sets:
    - current desktop is the Winlogon
    - desktop is locked
}

  TWlxDisplaySASNotice = procedure (AWlxContext: Pointer); stdcall;

{ WlxDisplayStatusMessage
  Winlogon calls this function when the GINA DLL should display a message.

  Parameters
  pWlxContext       Pointer to the GINA context associated with this window station. The GINA returns this context value when Winlogon calls WlxInitialize for this station.
  hDesktop          Handle to the desktop where the status message should be displayed.
  dwOptions         Specifies display options for the status dialog box. The following options are valid:
                      STATUSMSG_OPTION_NOANIMATION, STATUSMSG_OPTION_SETFOREGROUND
  pTitle            Pointer to a null-terminated wide character string that specifies the title of the message to be displayed.
  pMessage          Pointer to a null-terminated wide character string that specifies the message to be displayed.
  Returns TRUE if the message was displayed
}

  TWlxDisplayStatusMessage = function (AWlxContext: Pointer;
    ADesktop: HDESK; AOptions: DWORD; ATitle, AMessage: PWideChar): Bool; stdcall;

{ WlxGetConsoleSwitchCredentials
  Winlogon calls this function to read the currently logged on user's credentials
  to transparently transfer them to a target session.

  Parameters
  pWlxContext       Pointer to a GINA-specific context.
  pInfo             Pointer to a WLX_CONSOLESWITCH_CREDENTIALS_INFO_V1_0 to return GINA relevant information.
}
  TWlxGetConsoleSwitchCredentials = function (AWlxContext: Pointer;
    AInfo: PWLX_CONSOLESWITCH_CREDENTIALS_INFO_1_0): Bool; stdcall;

{ WlxGetStatusMessage
  Winlogon calls this function to get the status message being displayed by the GINA DLL.

  Parameters
  pWlxContext       Pointer to the GINA context associated with this window station. The GINA returns this context value when Winlogon calls WlxInitialize for this station.
  pdwOptions        Pointer to a DWORD that will hold the display options for the current status message.
  pMessage          Returns the current status message text.
  dwBufferSize      Size of the pMessage buffer.
}

  TWlxGetStatusMessage = function (AWlxContext: Pointer; AOptions: PDWORD;
    AMessage: PWideChar; ABufferSize: Cardinal): Bool; stdcall;

{ WlxInitialize
  Winlogon calls this function once for each window station present on the computer.
  OS supports one window station per workstation.
  The context returned by this function will be passed back to the GINA in all subsequent calls.

  Parameters
  lpWinsta            Pointer to the name of the window station being initialized.
  hWlx                Handle to Winlogon. The GINA must supply this handle in all calls to Winlogon support functions that involve this window station.
  pvReserved          Reserved.
  pWinlogonFunctions  Pointer to a Winlogon support function dispatch table. The contents of the table depends on the GINA DLL version returned by the WlxNegotiate call. This table does not change, allowing the GINA DLL to reference the table without copying it. If the GINA DLL needs to make a copy of the table, it should call WlxGetOption and supply WLX_OPTION_DISPATCH_TABLE_SIZE for the Option parameter.
                      WLX_DISPATCH_VERSION_1_0 WLX_DISPATCH_VERSION_1_1 WLX_DISPATCH_VERSION_1_2 WLX_DISPATCH_VERSION_1_3
  pWlxContext         Pointer to a pointer to VOID which will contain the address of the GINA context for this window station. This context is passed in all subsequent calls to the GINA from Winlogin. The GINA DLL manages any memory used by the context. The context pointer can be changed later by calling WlxSetOption with WLX_OPTION_CONTEXT_POINTER.

  WlxInitialize is called once for each window station present on the computer.
  Windows 2000 and later support a single window station called Winsta0. Additional physical window stations may be supported in future releases.
  Before calling WlxInitialize, Winlogon sets
    - current desktop is the Winlogon desktop.
    - desktop is locked.
}

  TWlxInitialize = function (AWinsta: PWideChar; AWlx: THandle; AReserved: Pointer;
    AWinlogonFunctions: Pointer; var AWlxContext: Pointer): Bool; stdcall;

{ WlxIsLockOk
  Winlogon calls this function before attempting to lock the workstation.
  Parameters
  pWlxContext         Pointer to the GINA context associated with this window station. The GINA supplies this context when Winlogon calls WlxInitialize.
}

  TWlxIsLockOk = function (AWlxContext: Pointer): Bool; stdcall;

{ WlxIsLogoffOk
  Winlogon calls this function when the user initiates a logoff operation.
  WlxIsLogoffOk can return FALSE to prevent the user from logging off the workstation.

  Parameters
  pWlxContext         Pointer to the GINA context associated with this window station. The GINA returns this context value when Winlogon calls WlxInitialize for this station.

}

  TWlxIsLogoffOk = function (AWlxContext: Pointer): Bool; stdcall;

{ WlxLoggedOnSAS
  Winlogon calls this function when it receives an SAS event while the user is
  logged on and the workstation is not locked.

  Parameters
  pWlxContext         Pointer to the GINA context associated with this window station. The GINA returns this context value when Winlogon calls WlxInitialize for this station.
  dwSasType           Specifies the type of SAS that occurred. Values from 0 to WLX_SAS_TYPE_MAX_MSFT_VALUE are reserved to define standard Microsoft SAS types. GINA developers can define additional SAS types using values greater than WLX_SAS_TYPE_MAX_MSFT_VALUE.
    WLX_SAS_TYPE_CTRL_ALT_DEL Indicates a user has typed the standard CTRL+ALT+DEL secure attention sequence (SAS).
    WLX_SAS_TYPE_SC_INSERT Indicates that a smart card has been inserted into a compatible device.
    WLX_SAS_TYPE_SC_REMOVE Indicates that a smart card has been removed from a compatible device.
    WLX_SAS_TYPE_TIMEOUT Indicates that no user input was received within the specified time-out period.
  pReserved           Reserved.
  Return Values
    WLX_SAS_ACTION_NONE Returns to the default desktop.
    WLX_SAS_ACTION_LOCK_WKSTA Locks the workstation and waits for the next SAS.
    WLX_SAS_ACTION_LOGOFF Logs the user off the workstation.
    WLX_SAS_ACTION_SHUTDOWN Logs the user off and shuts down the computer.
    WLX_SAS_ACTION_SHUTDOWN_REBOOT Logs the user off, shuts down the computer, and then reboots the computer.
    WLX_SAS_ACTION_SHUTDOWN_POWER_OFF If hardware allows, logs the user off, shuts down the computer, and then turns off the computer.
    WLX_SAS_ACTION_PWD_CHANGE Notifies network providers that the user changed their password. Obsolete GINA DLLs should call WlxChangePasswordNotify whenever a password is changed.
    WLX_SAS_ACTION_TASKLIST Invokes the task list.
    WLX_SAS_ACTION_FORCE_LOGOFF Forcibly logs off the user.
    WLX_SAS_ACTION_SHUTDOWN_SLEEP Puts the computer in suspend mode.
    WLX_SAS_ACTION_SHUTDOWN_SLEEP2 Shuts down the system into an ACPI power-down state. If the machine is not an ACPI machine, this option will have no effect.
    WLX_SAS_ACTION_SHUTDOWN_HIBERNATE Shuts down the system into the hibernate mode. If the system was not configured for hibernation, this option will have no effect.

  Remarks
  Winlogon calls WlxLoggedOnSAS when the logged-on user wants to shut down, log out, or lock the workstation. The GINA DLL can lock the workstation by returning WLX_SAS_LOCK_WKSTA. When this value is returned, Winlogon locks the workstation and calls WlxWkstaLockedSAS the next time it receives an SAS.

  Before calling WlxLoggedOnSAS, Winlogon sets the desktop and workstation states as follows.Sets the… So that…
  Desktop state The current desktop is the Winlogon desktop.
  Workstation state The desktop is locked.
}

  TWlxLoggedOnSAS = function (AWlxContext: Pointer; ASasType: DWORD; AReserved: Pointer): Integer; stdcall;

{ WlxLoggedOutSAS
  Winlogon calls this function when it receives a SAS event while no user is logged on.

  Parameters
  pWlxContext         Pointer to the GINA context associated with this window station. The GINA returns this context value when Winlogon calls WlxInitialize for this station.
  dwSasType           Specifies the type of SAS that occurred. Values from zero to WLX_SAS_TYPE_MAX_MSFT_VALUE are reserved to define standard Microsoft SAS types. GINA developers can define additional SAS types using values greater than WLX_SAS_TYPE_MAX_MSFT_VALUE.
    WLX_SAS_TYPE_CTRL_ALT_DEL Indicates that a user has typed the standard CTRL+ALT+DEL secure attention sequence (SAS).
    WLX_SAS_TYPE_SC_INSERT Indicates that a smart card has been inserted into a compatible device.
    WLX_SAS_TYPE_SC_REMOVE Indicates that a smart card has been removed from a compatible device.
    WLX_SAS_TYPE_TIMEOUT Indicates that no user input was received within the specified time-out period.

  pAuthenticationId   Specifies the authentication identifier associated with the current logon session. You can get this value by calling GetTokenInformation to obtain a TOKEN_STATISTICS structure for the token returned by the LogonUser function.
  pLogonSid           On input, this parameter points to a security identifier (SID) that is unique to the current logon session. Winlogon uses this SID to change the protection on the window station and application desktop so that the new logged-on user can access them.
    On output, Winlogon provides an SID. You can also get the SID by using the GetTokenInformation function to retrieve a TOKEN_GROUPS structure for the token returned by the LogonUser function. To do this, search the array returned in the TOKEN_GROUPS structure for the group with the SE_GROUP_LOGON_ID attribute.
  pdwOptions          Pointer to a DWORD that contains the set of logon options. The following option is defined.Value Meaning
  WLX_LOGON_OPT_NO_PROFILE Indicates that Winlogon must not load a profile for the logged-on user. Either the GINA DLL will take care of this activity, or the user does not need a profile.
  phToken             Points to a handle variable. When the logon operation succeeds, set this handle to a token that represents the logged-on user. Use the LogonUser function to get this token, then when the user logs off, Winlogon closes this handle and calls the WlxLogoff function.
    If you need this handle during your WlxLogoff, make a duplicate of the handle before returning it to Winlogon.
  pNprNotifyInfo      Points to an WLX_MPR_NOTIFY_INFO structure that contains domain, user name, and password information for the user. Winlogon will use this information to provide identification and authentication information to network providers.
    The GINA is not required to return password information. Any NULL fields within the structure will be ignored by Winlogon. Use LocalAlloc to allocate each string; Winlogon will free them when they are no longer needed.
    The GINA should provide domain, user, and password values for complete Session Directory functionality. If the password is not provided, Session Directory will require the user to input the password twice before the user is connected to the server.
  pProfile            On return from a successful authentication, the pProfile parameter points to either a WLX_PROFILE_V1_0 or a WLX_PROFILE_V2_0 structure. The first DWORD in the structure indicates which structure it is. Winlogon uses this structure to load the logged-on user's profile, and frees the memory associated with the structure when it no longer needs it.
  Return Values
  If the function fails, it returns zero. If the function succeeds, the return value is:
    WLX_SAS_ACTION_LOGON Indicates a user has logged on.
    WLX_SAS_ACTION_NONE Indicates the logged attempt was unsuccessful or canceled.
    WLX_SAS_ACTION_SHUTDOWN Indicates the user requested that the system be shut down.

  Before calling WlxLoggedOutSAS, Winlogon sets
    - current desktop is the Winlogon desktop.
    - desktop is locked.

  Do not activate the user shell program in WlxLoggedOutSAS.
  The user shell program should always be activated in WlxActivateUserShell.
}
  TWlxLoggedOutSAS = function (AWlxContext: Pointer; ASasType: DWORD; AAuthenticationId: PLUID;
    ALogonSid: PSIDIdentifierAuthority; AOptions: PDWORD; AToken: PHANDLE;
    AMprNotifyInfo: PWLX_MPR_NOTIFY_INFO; var AProfile: Pointer): Integer; stdcall;

{ WlxLogoff
  Winlogon calls this function to notify the GINA of a logoff operation on this workstation,
  allowing the GINA to perform any logoff operations that may be required.
  Parameters
  pWlxContext         Pointer to the GINA context associated with this window station. The GINA returns this context value when Winlogon calls WlxInitialize for this station.
  Before calling WlxLogoff, Winlogon sets:
    - current desktop is the Winlogon desktop
    - desktop is locked
}
  TWlxLogoff = procedure (AWlxContext: Pointer); stdcall;

{ WlxNegotiate
  This is the first call made by Winlogon to the GINA DLL.
  WlxNegotiate allows the GINA to verify that it supports the installed version of Winlogon.

  Parameters
  dwWinLogonVersion   Specifies which version of Winlogon will be communicating with the GINA.
  pdwDllVersion       Indicates which version of Winlogon the GINA supports. This version information is also used by Winlogon to determine which dispatch table is passed to the GINA in subsequent calls to WlxInitialize. This version cannot be greater than the version specified by dwWinLogonVersion.
  Return TRUE if the Winlogon version specified by dwWinLogonVersion is greater than or equal to the version returned in pdwDllVersion.
    When TRUE is returned, Winlogon will continue to initialize.
  Return FALSE if dwWinLogonVersion is less than pdwDllVersion.
    When FALSE is returned, Winlogon will terminate and the system will not boot.

  Before calling WlxLogoff, Winlogon sets:
    - current desktop is the Winlogon desktop
    - desktop is locked
}
  TWlxNegotiate = function (AWinLogonVersion: DWORD; ADllVersion: PDWORD): Bool; stdcall;

{ WlxNetworkProviderLoad
  Winlogon calls this function to collect valid authentication and identification information.

  Parameters
  pWlxContext         Pointer to the GINA context associated with this window station. The GINA returns this context value when Winlogon calls WlxInitialize for this station.
  pNprNotifyInfo      Points to an WLX_MPR_NOTIFY_INFO structure that contains domain, user name, and password information for the user. Winlogon will use this information to provide identification and authentication information to network providers.
  The GINA is not required to return password information.
  Any NULL fields within the structure will be ignored by Winlogon.
  Use LocalAlloc to allocate each string; Winlogon will free them when they are no longer needed.
}
  TWlxNetworkProviderLoad = function (AWlxContext: Pointer; AMprNotifyInfo: PWLX_MPR_NOTIFY_INFO): Bool; stdcall;

{ WlxRemoveStatusMessage
  Winlogon calls this function to tell the GINA DLL to stop displaying the status message.

  Parameters
  pWlxContext         Pointer to the GINA context associated with this window station.
  The GINA returns this context value when Winlogon calls WlxInitialize for this station.
}
  TWlxRemoveStatusMessage = function (AWlxContext: Pointer): Bool; stdcall;

{ WlxScreenSaverNotify
  Winlogon calls this function immediately before a screen saver is activated,
  allowing the GINA to interact with the screen saver program.

  Parameters
  pWlxContext         Pointer to the GINA context associated with this window station. The GINA returns this context value when Winlogon calls WlxInitialize for this station.
  pSecure             Pointer to a Boolean. On input, specifies whether the current screen saver is secure.
    On output, indicates whether the workstation should be locked.
  Return TRUE if the screen saver should be activated.
  If your GINA DLL does not export this function, Winlogon uses the default behavior (skipped).

  Before calling WlxScreenSaverNotify, Winlogon sets:
    - current desktop is the Winlogon desktop
    - desktop is locked

}
  TWlxScreenSaverNotify = function (AWlxContext: Pointer; ASecure: PBool): Bool; stdcall;

{ WlxShutdown
  Winlogon calls this function just before shutting down, allowing the GINA to
  perform any shutdown tasks, such as ejecting a smart card from a reader.

  Parameters
  pWlxContext         Pointer to the GINA context associated with this window station. The GINA returns this context value when Winlogon calls WlxInitialize for this station.
  ShutdownType        Specifies the type of shutdown. One of the following values is specified: Value Meaning
    WLX_SAS_ACTION_SHUTDOWN Logs the user off and shuts down the computer.
    WLX_SAS_ACTION_SHUTDOWN_REBOOT Shuts down and then reboots the computer.
    WLX_SAS_ACTION_SHUTDOWN_POWER_OFF Shuts down and turns off the computer, if the hardware allows.

  Winlogon calls WlxShutdown after the user has logged off and
  WlxLogoff function has been called.

  Before calling WlxShutdown, Winlogon sets:
    - current desktop is the Winlogon desktop
    - desktop is locked
}

  TWlxShutdown = procedure (AWlxContext: Pointer; AShutdownType: DWORD); stdcall;

{ WlxStartApplication
  Winlogon calls this function when system needs application started in user's context.
  This can occur for reasons:
    - Explorer has terminated unexpectedly and needs to be restarted,
    - Extended task manager needs to run.
  GINA can override this behavior, if appropriate, through this function.
  If WlxStartApplication function is not exported by the GINA, Winlogon will execute the process.

  Parameters
  pWlxContext         Pointer to the GINA context associated with this window station. The GINA returns this context value when Winlogon calls WlxInitialize for this station.
  pszDesktopName      Specifies the name of the desktop on which to start the application. Pass this string to the CreateProcess or CreateProcessAsUser functions through the lpDesktop member of the STARTUPINFO structure.
  pEnvironment        Specifies the initial environment for the process. Winlogon creates this environment and hands it off to the GINA. The GINA can modify this environment before using it to initialize the user's shell. The GINA is responsible for calling the VirtualFree function to free the memory allocated for pEnvironment.
  pszCmdLine          Program to execute.

  Before calling WlxStartApplication, Winlogon sets:
    - current desktop is the Winlogon desktop
    - desktop is locked
}
  TWlxStartApplication = function (AWlxContext: Pointer; ADesktopName: PWideChar;
    AEnvironment: Pointer; ACmdLine: PWideChar): Bool; stdcall;

{ WlxWkstaLockedSAS
  Winlogon calls this function when it receives a SAS and the workstation is locked.
  GINA should return value indicating workstation is to remain locked,
  workstation is to be unlocked, or the logged-on user is to be logged off
  (which leaves the workstation locked until the logoff is completed).

  Parameters
  pWlxContext         Pointer to the GINA context associated with this window station. The GINA returns this context value when Winlogon calls WlxInitialize for this station.
  dwSasType           Specifies the type of SAS that occurred. Values from zero to WLX_SAS_TYPE_MAX_MSFT_VALUE are reserved for standard Microsoft SAS types. GINA developers can use values greater than WLX_SAS_TYPE_MAX_MSFT_VALUE to define additional SAS types. The following SAS types are predefined. Value Meaning
    WLX_SAS_TYPE_CTRL_ALT_DEL Indicates a user has typed the standard CTRL+ALT+DEL secure attention sequence (SAS).
    WLX_SAS_TYPE_SC_INSERT Indicates that a smart card has been inserted into a compatible device.
    WLX_SAS_TYPE_SC_REMOVE Indicates that a smart card has been removed from a compatible device.
    WLX_SAS_TYPE_TIMEOUT Indicates that no user input was received within the specified time-out period.

  Return Values
    WLX_SAS_ACTION_NONE Tells Winlogon to keep the workstation locked.
    WLX_SAS_ACTION_FORCE_LOGOFF Tells Winlogon to forcibly log the user off.
    WLX_SAS_ACTION_LOGOFF Tells Winlogon to log the current user off.
    WLX_SAS_ACTION_UNLOCK_WKSTA Tells Winlogon to unlock the workstation.

  Before calling WlxWkstaLockedSAS, Winlogon sets:
    - current desktop is the Winlogon desktop
    - desktop is locked
}

  TWlxWkstaLockedSAS = function (AWlxContext: Pointer; ASasType: DWORD): Integer; stdcall;

  TGINAFuncsTable = record
    ActivateUserShell: TWlxActivateUserShell;      // Activates the user shell program.
    DisplayLockedNotice: TWlxDisplayLockedNotice;  // Allows the GINA DLL to display lock information.
    DisplaySASNotice: TWlxDisplaySASNotice;        // Winlogon calls this function when no user is logged on.
    DisplayStatusMessage: TWlxDisplayStatusMessage;// Winlogon calls this function with a status message to display.
    GetConsoleSwitchCredentials: TWlxGetConsoleSwitchCredentials;  // Winlogon calls this function to read the currently logged on user's credentials to transparently transfer them to a target session.
    GetStatusMessage: TWlxGetStatusMessage;        // Winlogon calls this function to get the current status message.
    Initialize: TWlxInitialize;                    // Initializes the GINA DLL for a specific window station.
    IsLockOk: TWlxIsLockOk;                        // Verifies that workstation lock is okay.
    IsLogoffOk: TWlxIsLogoffOk;                    // Verifies that logoff is okay.
    LoggedOnSAS: TWlxLoggedOnSAS;                  // Winlogon calls this function when it receives a secure attention sequence (SAS) event while the user is logged on and the workstation is not locked.
    LoggedOutSAS: TWlxLoggedOutSAS;                // Winlogon calls this function when it receives an SAS event while no user is logged on.
    Logoff: TWlxLogoff;                            // Notifies the GINA DLL that a logoff operation was requested.
    Negotiate: TWlxNegotiate;                      // Indicates whether the current version of Winlogon can be used with the GINA DLL.
    NetworkProviderLoad: TWlxNetworkProviderLoad;  // Winlogon calls this function after it loads a network provider to collect valid authentication and identification information.
    RemoveStatusMessage: TWlxRemoveStatusMessage;  // Winlogon calls this function to tell the GINA DLL to stop displaying the status message.
    ScreenSaverNotify: TWlxScreenSaverNotify;      // Allows the GINA to interact with the screen saver operation.
    Shutdown: TWlxShutdown;                        // Winlogon calls this function just before shutting down, allowing the GINA to perform any shutdown tasks, such as ejecting a smart card from a reader.
    StartApplication: TWlxStartApplication;        // Winlogon calls this function when the system needs an application started in the user's context.
    WkstaLockedSAS: TWlxWkstaLockedSAS;            // Winlogon calls this function when it receives an SAS while the workstation is locked.
  end;
{   GINA exported functions:
    1 WlxActivateUserShell
    3 WlxDisplayLockedNotice
    4 WlxDisplaySASNotice
    5 WlxDisplayStatusMessage
    6 WlxGetConsoleSwitchCredentials
    7 WlxGetStatusMessage
    8 WlxInitialize
    9 WlxIsLockOk
   $A WlxIsLogoffOk
   $B WlxLoggedOnSAS
   $C WlxLoggedOutSAS
   $D WlxLogoff
   $E WlxNegotiate
   $F WlxNetworkProviderLoad
  $11 WlxRemoveStatusMessage
  $12 WlxScreenSaverNotify
  $13 WlxShutdown
  $14 WlxStartApplication
  $15 WlxWkstaLockedSAS

   No description in MSDN about exported MSGina.dll functions
    0 ShellShutdownDialog
    2 WlxDisconnectNotify
  $10 WlxReconnectNotify
}

implementation

end.