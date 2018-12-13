unit
  winscard;
(*##*)
(*******************************************************************************
*                                                                             *
*   W  I  N  S  C  A  R  D                                                     *
*                                                                             *
*   Copyright © 2007 Andrei Ivanov. All rights reserved.                       *
*   Windows Smart Card API pascal wrapper                                     *
*   Conditional defines:                                                       *
*                                                                             *
*   Revisions    : Apr 02 2007                                                 *
*   Last revision: Apr 02 2007                                                *
*   Lines        : 144                                                         *
*   History      :                                                            *
*   Printed      : ---                                                         *
*                                                                             *
********************************************************************************)
(*##*)

interface
uses
  Windows;

const
  SCARD_S_SUCCESS                 = 0; // No error was encountered.
  SCARD_E_BAD_SEEK                = 1; // An error occurred in setting the smart card file object pointer.
  SCARD_E_CANCELLED               = 1; // The action was canceled by an SCardCancel request.
  SCARD_E_CANT_DISPOSE            = 1; // The system could not dispose of the media in the requested manner.
  SCARD_E_CARD_UNSUPPORTED        = 1; // The smart card does not meet minimal requirements for support.
  SCARD_E_CERTIFICATE_UNAVAILABLE = 1; // The requested certificate could not be obtained.
  SCARD_E_COMM_DATA_LOST          = 1; // A communications error with the smart card has been detected.
  SCARD_E_DIR_NOT_FOUND           = 1; // The specified directory does not exist in the smart card.
  SCARD_E_DUPLICATE_READER        = 1; // The reader driver did not produce a unique reader name.
  SCARD_E_FILE_NOT_FOUND          = 1; // The specified file does not exist in the smart card.
  SCARD_E_ICC_CREATEORDER         = 1; // The requested order of object creation is not supported.
  SCARD_E_ICC_INSTALLATION        = 1; // No primary provider can be found for the smart card.
  SCARD_E_INSUFFICIENT_BUFFER     = 1; // The data buffer for returned data is too small for the returned data.
  SCARD_E_INVALID_ATR             = 1; // An ATR string obtained from the registry is not a valid ATR string.
  SCARD_E_INVALID_CHV             = 1; // The supplied PIN is incorrect.
  SCARD_E_INVALID_HANDLE          = 1; // The supplied handle was invalid.
  SCARD_E_INVALID_PARAMETER       = 1; // One or more of the supplied parameters could not be properly interpreted.
  SCARD_E_INVALID_TARGET          = 1; // Registry startup information is missing or invalid.
  SCARD_E_INVALID_VALUE           = 1; // One or more of the supplied parameter values could not be properly interpreted.
  SCARD_E_NO_ACCESS               = 1; // Access is denied to the file.
  SCARD_E_NO_DIR                  = 1; // The supplied path does not represent a smart card directory.
  SCARD_E_NO_FILE                 = 1; // The supplied path does not represent a smart card file.
  SCARD_E_NO_MEMORY               = 1; // Not enough memory available to complete this command.
  SCARD_E_NO_READERS_AVAILABLE    = 1; // No smart card reader is available.
  SCARD_E_NO_SERVICE              = 1; // The smart card resource manager is not running.
  SCARD_E_NO_SMARTCARD            = 1; // The operation requires a smart card, but no smart card is currently in the device.
  SCARD_E_NO_SUCH_CERTIFICATE     = 1; // The requested certificate does not exist.
  SCARD_E_NOT_READY               = 1; // The reader or card is not ready to accept commands.
  SCARD_E_NOT_TRANSACTED          = 1; // An attempt was made to end a non-existent transaction.
  SCARD_E_PCI_TOO_SMALL           = 1; // The PCI receive buffer was too small.
  SCARD_E_PROTO_MISMATCH          = 1; // The requested protocols are incompatible with the protocol currently in use with the card.
  SCARD_E_READER_UNAVAILABLE      = 1; // The specified reader is not currently available for use.
  SCARD_E_READER_UNSUPPORTED      = 1; // The reader driver does not meet minimal requirements for support.
  SCARD_E_SERVICE_STOPPED         = 1; // The smart card resource manager has shut down.
  SCARD_E_SHARING_VIOLATION       = 1; // The smart card cannot be accessed because of other outstanding connections.
  SCARD_E_SYSTEM_CANCELLED        = 1; // The action was canceled by the system, presumably to log off or shut down.
  SCARD_E_TIMEOUT                 = 1; // The user-specified timeout value has expired.
  SCARD_E_UNEXPECTED              = 1; // An unexpected card error has occurred.
  SCARD_E_UNKNOWN_CARD            = 1; // The specified smart card name is not recognized.
  SCARD_E_UNKNOWN_READER          = 1; // The specified reader name is not recognized.
  SCARD_E_UNKNOWN_RES_MNG         = 1; // An unrecognized error code was returned from a layered component.
  SCARD_E_UNSUPPORTED_FEATURE     = 1; // This smart card does not support the requested feature.
  SCARD_E_WRITE_TOO_MANY          = 1; // An attempt was made to write more data than would fit in the target object.
  SCARD_F_COMM_ERROR              = 1; // An internal communications error has been detected.
  SCARD_F_INTERNAL_ERROR          = 1; // An internal consistency check failed.
  SCARD_F_UNKNOWN_ERROR           = 1; // An internal error has been detected, but the source is unknown.
  SCARD_F_WAITED_TOO_LONG         = 1; // An internal consistency timer has expired.
  SCARD_P_SHUTDOWN                = 1; // The operation has been aborted to allow the server application to exit.
  SCARD_W_CANCELLED_BY_USER       = 1; // The action was canceled by the user.
  SCARD_W_CHV_BLOCKED             = 1; // The card cannot be accessed because the maximum number of PIN entry attempts has been reached.
  SCARD_W_EOF                     = 1; // The end of the smart card file has been reached.
  SCARD_W_REMOVED_CARD            = 1; // The smart card has been removed, so that further communication is not possible.
  SCARD_W_RESET_CARD              = 1; // The smart card has been reset, so any shared state information is invalid.
  SCARD_W_SECURITY_VIOLATION      = 1; // Access was denied because of a security violation.
  SCARD_W_UNPOWERED_CARD          = 1; // Power has been removed from the smart card, so that further communication is not possible.
  SCARD_W_UNRESPONSIVE_CARD       = 1; // The smart card is not responding to a reset.
  SCARD_W_UNSUPPORTED_CARD        = 1; // The reader cannot communicate with the card, due to ATR string configuration conflicts.
  SCARD_W_WRONG_CHV               = 1; // The card cannot be accessed because the wrong PIN was presented.

type
  TSCARDCONTEXT = THandle;
  TSCARDHANDLE = THandle;
  
  PGUIDs = ^TGUIDs;
  TGUIDs = array [0..0] of TGUID; // Array of identifiers (GUIDs) that identify the interfaces

  TSCARD_READERSTATEA = record
    Reader: PANSIChar;
    UserData: Pointer;
    CurrentState: DWORD;
    EventState: DWORD;
    AtrCount: DWORD;
    Atr: array[0..35] of Char;
  end;
  TSCARD_READERSTATEW = record
    Reader: PWideChar;
    UserData: Pointer;
    CurrentState: DWORD;
    EventState: DWORD;
    AtrCount: DWORD;
    Atr: array[0..35] of Char;
  end;
  PSCARD_READERSTATEA = ^TSCARD_READERSTATEA;
  TSCARD_READERSTATEAs = array[0..0] of TSCARD_READERSTATEA;
  PSCARD_READERSTATEW = ^TSCARD_READERSTATEW;
  TSCARD_READERSTATEWs = array[0..0] of TSCARD_READERSTATEW;

  TSCARD_IO_REQUEST = record
    Protocol,
    PciLength: DWORD;
  end;
  PSCARD_IO_REQUEST = ^TSCARD_IO_REQUEST;

  TSCARD_ATRMASK = record
    AtrCount: DWORD;
    Atr: array [0..35] of Byte;
    AMask: array [0..35] of Byte;
  end;
  TSCARD_ATRMASKs = array[0..0] of TSCARD_ATRMASK;
  PSCARD_ATRMASKs = ^TSCARD_ATRMASKs;
  
// ------------------- Smart Card Database Management Functions ----------------

{ SCardAddReaderToGroup ANSI 6 3 Unicode 7 4
  adds a reader to a reader group.

  Parameters
  hContext              Handle identifies resource manager context- set by a previous call to SCardEstablishContext. Not NULL!
  szReaderName          Friendly name of the reader that you are adding.
  szGroupName           Friendly name of the group to which you are adding the reader.
  if return an error code, see Smart Card Return Values.
  SCardAddReaderToGroup automatically creates reader group if it does not exist.
  For more information on other database management functions, see Smart Card Database Management Functions.

  Example Code
  The following example demonstrates how to add a smart card reader to a group. The example assumes that lReturn is an existing variable of type LONG, that hContext is a valid handle received from a previous call to the SCardEstablishContext function, and that "MyReader" and "MyReaderGroup" are known by the system through previous calls to the SCardIntroduceReader and SCardIntroduceReaderGroup functions, respectively.
}
  TSCardAddReaderToGroupA = function (AhContext: TSCARDCONTEXT; AReaderName,
    AGroupName: PANSIChar): LongInt; stdcall;
  TSCardAddReaderToGroupW = function (AhContext: TSCARDCONTEXT; AReaderName,
    AGroupName: PWideChar): LongInt; stdcall;

{ SCardForgetCardType ANSI 16 0D Unicode 17 0E
  removes an introduced smart card from the smart card subsystem.

  Parameters
  hContext     handle identifies resource manager context is set by a previous call to SCardEstablishContext, cannot be NULL.
  szCardName   Friendly name of the card to be removed from the smart card database.
  Success SCARD_S_SUCCESS.
  Failure An error code. For more information, see Smart Card Return Values.
}
  TSCardForgetCardTypeA = function (AhContext: TSCARDCONTEXT; ACardName: PANSIChar): LongInt; stdcall;
  TSCardForgetCardTypeW = function (AhContext: TSCARDCONTEXT; ACardName: PWideChar): LongInt; stdcall;

{ ANSI 18 0F Unicode 21 12 SCardForgetReader
  removes a previously introduced reader from control by the smart card subsystem.
  It is removed from the smart card database, including from any reader group

  Parameters
  hContext                handle identifies resource manager context is set by a previous call to SCardEstablishContext, cannot be NULL.
  szReaderName            Friendly name of the reader to be removed from the smart card database.
  Success SCARD_S_SUCCESS.
  Failure An error code. For more information, see Smart Card Return Values.

  If reader is last member of reader group, reader group is automatically removed.
}
  TSCardForgetReaderA = function(AhContext: TSCARDCONTEXT; AReaderName: PANSIChar): LongInt; stdcall;
  TSCardForgetReaderW = function(AhContext: TSCARDCONTEXT; AReaderName: PWideChar): LongInt; stdcall;

{ SCardForgetReaderGroup ANSI 19 10 Unicode 20 11
  removes smart card reader group from subsystem.
  Although this function automatically clears all readers from the group,
  it does not affect the existence of the individual readers in the database.

  Parameters
  hContext             Handle that identifies the resource manager context. The resource manager context is set by a previous call to SCardEstablishContext. This parameter cannot be NULL.
  szGroupName          Friendly name of the reader group to be removed. System-defined reader groups cannot be removed from the database.
  Success SCARD_S_SUCCESS.
  Failure An error code.
}
  TSCardForgetReaderGroupA = function (AhContext: TSCARDCONTEXT; AGroupName: PANSIChar): LongInt; stdcall;
  TSCardForgetReaderGroupW = function (AhContext: TSCARDCONTEXT; AGroupName: PANSIChar): LongInt; stdcall;

{ SCardIntroduceCardType ANSI 30 1B Unicode 31 1C
  introduces a smart card to the smart card subsystem (for the active user) by adding
  it to the smart card database.

  Parameters
  hContext             handle identifies resource manager context is set by a previous call to SCardEstablishContext, cannot be NULL.
  szCardName           Name by which the user can recognize the card.
  pguidPrimaryProvider Pointer to the identifier (GUID) for the smart card's primary service provider.
  rgguidInterfaces     Array GUIDs that identify the interfaces supported by smart card.
  dwInterfaceCount     Number of identifiers in the rgguidInterfaces array.
  pbAtr                ATR string that can be used for matching purposes when querying the smart card database (for more information, see SCardListCards). The length of this string is determined by normal ATR parsing.
  pbAtrMask            Optional bitmask to use when comparing the ATRs of smart cards to the ATR supplied in pbAtr. If this value is non-NULL, it must point to a string of bytes the same length as the ATR string supplied in pbAtr. When a given ATR string 'A' is compared to the ATR supplied in pbAtr, it matches if and only if A & M =pbAtr, where M is the supplied mask, and & represents bitwise logical AND.
  cbAtrLen             Length of the ATR and optional ATR Mask. If this value is zero, then the length of the ATR is determined by normal ATR parsing. This value cannot be 0 (zero) if a pbAtr value is supplied.
  Success SCARD_S_SUCCESS.
  Failure An error code. For more information, see Smart Card Return Values.
}
  TSCardIntroduceCardTypeA = function (AhContext: TSCARDCONTEXT;
    ACardName: PANSIChar; APrimaryProvider: PGUID; const AInterfaces: TGUIDs;
      InterfaceCount: DWORD; AAtr, AAtrMask: PBYTE; AAtrLen: DWORD): LongInt; stdcall;

  TSCardIntroduceCardTypeW = function (AhContext: TSCARDCONTEXT;
    ACardName: PWideChar; APrimaryProvider: PGUID; const AInterfaces: TGUIDs;
      InterfaceCount: DWORD; AAtr, AAtrMask: PBYTE; AAtrLen: DWORD): LongInt; stdcall;

{ SCardIntroduceReader ANSI 32 1D Unicode 35 20
  introduces a new name for an existing smart card reader.
  Smart card readers are automatically introduced to the system; a smart card reader vendor's setup program can also introduce a smart card reader to the system.

  Parameters
  hContext       Handle that identifies the resource manager context. The resource manager context is set by a previous call to SCardEstablishContext. This parameter cannot be NULL.
  szReaderName   Friendly name to be assigned to the reader.
  szDeviceName   System name of the smart card reader, for example, "MyReader 01".
  All readers installed on the system are automatically introduced by their system name. Typically, SCardIntroduceReader is called only to change the name of an existing reader.
}

  TSCardIntroduceReaderA = function (AhContext: TSCARDCONTEXT; AReaderName,
    ADeviceName: PANSIChar): LongInt; stdcall;
  TSCardIntroduceReaderW = function (AhContext: TSCARDCONTEXT; AReaderName,
    ADeviceName: PWideChar): LongInt; stdcall;

{ SCardIntroduceReaderGroup ANSI 33 1E Unicode 34 1F
  introduces reader group to smart card subsystem.
  However, the reader group is not created until the group is specified when adding
  a reader to the smart card database.

  Parameters
  hContext      handle identifies resource manager context is set by a previous call to SCardEstablishContext, cannot be NULL.
  szGroupName   friendly name to be assigned to the new reader group.
  Success SCARD_S_SUCCESS.
  Failure An error code.
  for PC/SC specification compatibility.
  Reader groups are not stored until a reader is added to the group.
}
  TSCardIntroduceReaderGroupA = function (AhContext: TSCARDCONTEXT; AGroupName: PANSIChar): LongInt; stdcall;
  TSCardIntroduceReaderGroupW = function (AhContext: TSCARDCONTEXT; AGroupName: PWideChar): LongInt; stdcall;
{ SCardRemoveReaderFromGroup ANSI 52 33 Unicode 53 34
  removes a reader from an existing reader group. This function has no affect on the reader.

  Parameters
  hContext       handle identifies resource manager context is set by a previous call to SCardEstablishContext, cannot be NULL.
  szReaderName   Friendly name of the reader to be removed.
  szGroupName    Friendly name of the group from which the reader should be removed.
  When the last reader is removed from a group, the group is automatically forgotten.
}
  TSCardRemoveReaderFromGroupA = function (AhContext: TSCARDCONTEXT; AReaderName: PANSIChar): LongInt; stdcall;
  TSCardRemoveReaderFromGroupW = function (AhContext: TSCARDCONTEXT; AReaderName: PWideChar): LongInt; stdcall;

// -------------------- Smart Card Database Query Functions  -------------------

{ SCardGetProviderId ANSI 26 17 Unicode 27 18
  returns GUID of the primary service provider for a given card.
  caller supplies name of a smart card (previously introduced to the system) and
  receives the registered identifier of the primary service provider GUID, if one exists.

  Parameters
  hContext        Handle that identifies the resource manager context for the query. The resource manager context can be set by a previous call to SCardEstablishContext. This parameter cannot be NULL.
  szCard          Name of the card defined to the system.
  pguidProviderId [out] Identifier (GUID) of the primary service provider. This provider may be activated using COM, and will supply access to other services in the card.
}
   TSCardGetProviderIdA = function (AhContext: TSCARDCONTEXT; ACard: PANSIChar;
     AProviderId: PGUID): LongInt; stdcall;
   TSCardGetProviderIdW = function (AhContext: TSCARDCONTEXT; ACard: PWideChar;
     AProviderId: PGUID): LongInt; stdcall;

{ SCardListCards ANSI 37 22 Unicode 38 23
  searches smart card database and provides a list of named cards introduced to system by the user.
  caller specifies ATR string, a set of interface identifiers (GUIDs), or both.
  If both an ATR string and an identifier array are supplied, the cards returned will match the ATR string supplied and support the interfaces specified.

  Parameters
  hContext            Handle that identifies the resource manager context for the query. The resource manager context can be set by a previous call to SCardEstablishContext. This parameter cannot be NULL.
  pbAtr               Address of an ATR string to compare to known cards, or NULL if no ATR matching is to be performed.
  rgguidInterfaces    Array of identifiers (GUIDs), or NULL if no interface matching is to be
                      performed. When an array is supplied, a card name will be returned only
                      if all the specified identifiers are supported by the card.
  cguidInterfaceCount Number of entries in the rgguidInterfaces array. If rgguidInterfaces
                      is NULL, then this value is ignored.
  mszCards            Multi-string that lists the smart cards found. If this value is NULL,
                      SCardListCards ignores the buffer length supplied in pcchCards,
                      returning the length of the buffer that would have been returned if this
                      parameter had not been NULL to pcchCards and a success code.
  pcchCards           Length of the mszCards buffer in characters. Receives the actual length
                      of the multi-string structure, including all trailing null characters.
                      If the buffer length is specified as SCARD_AUTOALLOCATE, then mszCards
                      is converted to a pointer to a byte pointer, and receives the address
                      of a block of memory containing the multi-string structure. This block
                      of memory must be deallocated with SCardFreeMemory.
  To return all smart cards introduced to the subsystem, set pbAtr and rgguidInterfaces to NULL.
}
  TSCardListCardsA = function (AhContext: TSCARDCONTEXT; AAtr: PByte;
    var AInterfaces: PGUIDs; AInterfaceCount: DWORD; ACards: PANSIChar; var AReadCards: DWORD): LongInt; stdcall;
  TSCardListCardsW = function (AhContext: TSCARDCONTEXT; AAtr: PByte;
    var AInterfaces: PGUIDs; AInterfaceCount: DWORD; ACards: PWideChar; var AReadCards: DWORD): LongInt; stdcall;

{ SCardListInterfaces ANSI 39 24 Unicode 40 25
  provides a list of interfaces supplied by a given card.
  caller supplies name of a smart card previously introduced to the subsystem, and
  receives the list of interfaces supported by the card.

  Parameters
  hContext            Handle that identifies the resource manager context for the query. The resource manager context can be set by a previous call to SCardEstablishContext. This parameter cannot be NULL.
  szCard              Name of the smart card already introduced to the smart card subsystem.
  pguidInterfaces     Array of interface identifiers (GUIDs) that indicate the interfaces supported by the smart card.
                      If this value is NULL, SCardListInterfaces ignores the array length supplied
                      in pcguidInterfaces, returning the size of the array that would have been
                      returned if this parameter had not been NULL to pcguidInterfaces and a success code.
  pcguidInterfaces    Size of the pcguidInterfaces array, and receives the actual size of the returned
                      array. If the array size is specified as SCARD_AUTOALLOCATE, then
                      pcguidInterfaces is converted to a pointer to a GUID pointer,
                      and receives the address of a block of memory containing the array.
                      This block of memory must be deallocated with SCardFreeMemory.
}
  TSCardListInterfacesA = function(AhContext: TSCARDCONTEXT; ACard: PANSIChar;
    AInterfaces: PGUIDs; var AInterfaceCount): LongInt; stdcall;
  TSCardListInterfacesW = function(AhContext: TSCARDCONTEXT; ACard: PWideChar;
    AInterfaces: PGUIDs; var AInterfaceCount): LongInt; stdcall;

{ SCardListReaderGroups ANSI 41 26 Unicode
  provides list of reader groups that have previously been introduced to the system.

  Parameters
  hContext         Handle that identifies the resource manager context for the query. The resource manager context can be set by a previous call to SCardEstablishContext. This parameter cannot be NULL.
  mszGroups        [out] Multi-string that lists the reader groups defined to the system and
                   available to current user on the current terminal. If this value is NULL,
                   SCardListReaderGroups ignores the buffer length supplied in pcchGroups,
                   writes the length of the buffer that would have been returned if this
                   parameter had not been NULL to pcchGroups, and returns a success code.
  pcchGroups       [in, out] Length of the mszGroups buffer in characters, and receives
                   actual length of the multi-string structure, including all trailing null
                   characters. If the buffer length is specified as SCARD_AUTOALLOCATE, then
                   mszGroups is converted to a pointer to a byte pointer, and receives
                   address of a block of memory containing the multi-string structure.
                   This block of memory must be deallocated with SCardFreeMemory.
  A group is returned only if it contains at least one reader. This includes the group SCard$DefaultReaders.
  The group SCard$AllReaders cannot be returned, since it only exists implicitly.
}

  TSCardListReaderGroupsA = function (AhContext: TSCARDCONTEXT; var AGroups: PANSIChar;
    var AGroupsLen: DWORD): LongInt; stdcall;
  TSCardListReaderGroupsW = function (AhContext: TSCARDCONTEXT; var AGroups: PWideChar;
    var AGroupsLen: DWORD): LongInt; stdcall;

{ SCardListReaders ANDI 43 28 Unicode 44 29
  provides list of readers within a set of named reader groups, eliminating duplicates.
  caller supplies a list of reader groups, and receives the list of readers within.
  Unrecognized group names are ignored.

  Parameters
  hContext     Handle that identifies the resource manager context for the query. The resource manager context can be set by a previous call to SCardEstablishContext. This parameter cannot be NULL.
  mszGroups    Names of the reader groups defined to the system, as a multi-string. Use a NULL value to list all readers in the system (that is, the SCard$AllReaders group).
  mszReaders   [out] Multi-string that lists the card readers within the supplied reader groups. If this value is NULL, SCardListReaders ignores the buffer length supplied in pcchReaders, writes the length of the buffer that would have been returned if this parameter had not been NULL to pcchReaders, and returns a success code.
  pcchReaders  [in, out] Length of the mszReaders buffer in characters. This parameter receives the actual length of the multi-string structure, including all trailing null characters. If the buffer length is specified as SCARD_AUTOALLOCATE, then mszReaders is converted to a pointer to a byte pointer, and receives the address of a block of memory containing the multi-string structure. This block of memory must be deallocated with SCardFreeMemory.
}
  TSCardListReadersA = function(AhContext: TSCARDCONTEXT; AGroups: PANSIChar;
    var AReaders: PANSIChar; var AReadersLen: DWORD): LongInt; stdcall;
  TSCardListReadersW = function(AhContext: TSCARDCONTEXT; AGroups: PWideChar;
    var AReaders: PWideChar; var AReadersLen: DWORD): LongInt; stdcall;

// ------------------- Resource Manager Context Functions ----------------------

{ SCardEstablishContext 15 0C
  establishes resource manager context (scope) within which database operations are performed.

  Parameters
  dwScope     Scope of the resource manager context.Value Meaning
                SCARD_SCOPE_USER Database operations are performed within the domain of the user.
                SCARD_SCOPE_SYSTEM Database operations are performed within the domain of the system. (The calling application must have appropriate access permissions for any database actions.)
  pvReserved1
              Reserved for future use, and must be NULL. This parameter will allow a suitably privileged management application to act on behalf of another user.
  pvReserved2 Reserved for future use, and must be NULL. This parameter will allow a suitably privileged management application to act on behalf of another terminal.
  phContext   [out] Handle to the established resource manager context. This handle can now be supplied to other functions attempting to do work within this context.
}
  TSCardEstablishContext = function (AScope: DWORD; AReserved1, AReserved2: Pointer;
    var AContext: TSCARDCONTEXT): LongInt; stdcall;

{ SCardReleaseContext 50 30
  closes an established resource manager context, freeing any resources allocated
  under that context, including SCARDHANDLE objects and memory allocated using the
  SCARD_AUTOALLOCATE length designator.

  Parameters
  hContext  Handle that identifies the resource manager context. The resource manager context is set by a previous call to SCardEstablishContext.
}
  TSCardReleaseContext = function (AhContext: TSCARDCONTEXT): LongInt; stdcall;

// ------------------- Resource Manager Support Function -----------------------

{ SCardFreeMemory 22 13
  releases memory that has been returned from the resource manager using SCARD_AUTOALLOCATE
  length designator.

  Parameters
  hContext     Handle that identifies the resource manager context returned from SCardEstablishContext, or NULL if the creating function also specified NULL for its hContext parameter. For more information, see Smart Card Database Query Functions.
  pvMem        Memory block to be released.
}
  TSCardFreeMemory = function (AhContext: TSCARDCONTEXT; AMem: Pointer): LongInt; stdcall;

// ------------------- Smart Card Tracking Functions ---------------------------

{ SCardLocateCards ANSI 45 2A Unicode 48 2D
  searches readers listed in the rgReaderStates for card with ATR that matches one of the card
  names specified in mszCards, returning immediately with the result.

  Parameters
  hContext       Handle that identifies the resource manager context. The resource manager context is set by a previous call to SCardEstablishContext.
  mszCards       Multi-string that contains the names of the cards to search for.
  rgReaderStates [in, out] Array of SCARD_READERSTATE structures that specify the readers to search, and receives the result.
  cReaders       Number of elements in the rgReaderStates array.
  This service is especially useful when used in conjunction with SCardGetStatusChange.
  If no matching cards are found by means of SCardLocateCards, the calling application may
  use SCardGetStatusChange to wait for card availability changes.
}

  TSCardLocateCardsA = function(AhContext: TSCARDCONTEXT; ACards: PANSIChar;
    var AReaderStates: TSCARD_READERSTATEAs; AReaders: DWORD): LongInt; stdcall;
  TSCardLocateCardsW = function(AhContext: TSCARDCONTEXT; ACards: PWideChar;
    var AReaderStates: TSCARD_READERSTATEWs; AReaders: DWORD): LongInt; stdcall;

{ SCardLocateCardsByATR ANSI 46 2B Uniicode 47 2C
  searches the readers listed in the rgReaderStates parameter for card with ATR string
  that matches one of the card names specified in mszCards, returning immediately with
  result.

  Parameters
  hContext       Handle that identifies the resource manager context. The resource manager context is set by a previous call to SCardEstablishContext.
  rgAtrMasks     Array of SCARD_ATRMASK structures that contain the names of the cards for which to search.
  cAtrs          Number of elements in the rgAtrMasks array.
  rgReaderStates [in, out] Array of SCARD_READERSTATE structures that specify readers to
                 search, and receive the result.
  cReaders       Number of elements in the rgReaderStates array.
  This service is especially useful when used in conjunction with SCardGetStatusChange.
  If no matching cards are found by means of SCardLocateCards, calling application may
  use SCardGetStatusChange to wait for card availability changes.
}

  TSCardLocateCardsByATRA = function (AhContext: TSCARDCONTEXT; AAtrMasks: PSCARD_ATRMASKs;
    Atrs: DWORD; var AReaderStates: TSCARD_READERSTATEAs; AReaders: DWORD): Longint; stdcall;
  TSCardLocateCardsByATRW = function (AhContext: TSCARDCONTEXT; AAtrMasks: PSCARD_ATRMASKs;
    Atrs: DWORD; var AReaderStates: TSCARD_READERSTATEWs; AReaders: DWORD): Longint; stdcall;


{ SCardGetStatusChange ANSI 28 19 Unicode 29 1A
  blocks execution until current availability of cards in a specific set of readers changes.
  caller supplies list of readers to be monitored by an SCARD_READERSTATE array and the maximum
  amount of time (in milliseconds) that it is willing to wait for an action to occur on one of the listed readers.
  Note that SCardGetStatusChange uses the user-supplied value in the dwCurrentState members of
  rgReaderStates SCARD_READERSTATE array as the definition of the current state of the readers.
  The function returns when there is a change in availability, having filled in
  dwEventState members of rgReaderStates appropriately.

  Parameters
  hContext        Handle that identifies the resource manager context. The resource manager context is set by a previous call to SCardEstablishContext.
  dwTimeout       Maximum amount of time (in milliseconds) to wait for an action.
                  zero causes function to return immediately.
                  INFINITE causes this function never to time out.
  rgReaderStates  [in, out] Array of SCARD_READERSTATE structures that specify
                  readers to watch, and receives the result.
  cReaders        [in] Number of elements in the rgReaderStates array.
}

  TSCardGetStatusChangeA = function (AhContext: TSCARDCONTEXT; ATimeout: DWORD;
    AReaderStates: TSCARD_READERSTATEAs; AReaders: DWORD): Longint; stdcall;
  TSCardGetStatusChangeW = function (AhContext: TSCARDCONTEXT; ATimeout: DWORD;
    AReaderStates: TSCARD_READERSTATEWs; AReaders: DWORD): Longint; stdcall;

{ SCardCancel 9 6
  terminates all outstanding actions within a specific resource manager context.
  The only requests that you can cancel are those that require waiting for external
  action by the smart card or user. Any such outstanding action requests will terminate
  with a status indication that the action was canceled.
  This is especially useful to force outstanding SCardGetStatusChange calls to terminate.

  Parameters
  hContext    Handle that identifies the resource manager context.
              The resource manager context is set by a previous call to SCardEstablishContext.
}
  TSCardCancel = function (AhContext: TSCARDCONTEXT): LongInt; stdcall;

// ------------------- Smart Card and Reader Access Functions ------------------

{ SCardConnect ANSI 10 7 Unicode 11 8
  establishes a connection (using a specific resource manager context) between
  calling application and a smart card contained by a specific reader.
  If no card exists in the specified reader, an error is returned.

  Parameters
  hContext     Handle that identifies the resource manager context. The resource manager context is set by a previous call to SCardEstablishContext.
  szReader     Name of the reader containing the target card.
  dwShareMode  Flag that indicates whether other applications may form connections to the card. Value Meaning
                 SCARD_SHARE_SHARED This application is willing to share the card with other applications.
                 SCARD_SHARE_EXCLUSIVE This application is not willing to share the card with other applications.
                 SCARD_SHARE_DIRECT This application is allocating the reader for its private use, and will be controlling it directly. No other applications are allowed access to it.
  dwPreferredProtocols  Bit mask of acceptable protocols for the connection. Possible values may be combined with the OR operation. Value Meaning
                 SCARD_PROTOCOL_T0 T=0 is an acceptable protocol.
                 SCARD_PROTOCOL_T1 T=1 is an acceptable protocol.
                 0 This parameter may be zero only if dwShareMode is set to SCARD_SHARE_DIRECT. In this case, no protocol negotiation will be performed by the drivers until an IOCTL_SMARTCARD_SET_PROTOCOL control directive is sent with SCardControl.
  phCard       [out] Handle that identifies the connection to the smart card in the designated reader.
  pdwActiveProtocol  [out] Flag that indicates the established active protocol. Value Meaning
                 SCARD_PROTOCOL_T0 T=0 is the active protocol.
                 SCARD_PROTOCOL_T1 T=1 is the active protocol.
                 SCARD_PROTOCOL_UNDEFINED SCARD_SHARE_DIRECT has been specified, so that no protocol negotiation has occurred. It is possible that there is no card in the reader.
}
  TSCardConnectA = function (AhContext: TSCARDCONTEXT;
    AReader: PANSIChar; AShareMode: DWORD; APreferredProtocols: DWORD;
    var ACard: TSCARDHANDLE; var AActiveProtocol: DWORD): LongInt; stdcall;
  TSCardConnectW = function (AhContext: TSCARDCONTEXT;
    AReader: PWideChar; AShareMode: DWORD; APreferredProtocols: DWORD;
    var ACard: TSCARDHANDLE; var AActiveProtocol: DWORD): LongInt; stdcall;

{ SCardReconnect 49 2E
  reestablishes an existing connection between the calling application and a smart card. This function moves a card handle from direct access to general access, or acknowledges and clears an error condition that is preventing further access to the card.

  Parameters
  hCard        Reference value obtained from a previous call to SCardConnect.
  dwShareMode  Flag that indicates whether other applications may form connections to this card. Value Meaning
               SCARD_SHARE_SHARED This application will share this card with other applications.
               SCARD_SHARE_EXCLUSIVE This application will not share this card with other applications.
  dwPreferredProtocols  Bit mask of acceptable protocols for this connection. Possible values may be combined with the OR operation. Value Meaning
               SCARD_PROTOCOL_T0 T=0 is an acceptable protocol.
               SCARD_PROTOCOL_T1 T=1 is an acceptable protocol.
  dwInitialization  Type of initialization that should be performed on the card. Value Meaning
               SCARD_LEAVE_CARD Do not do anything special on reconnect.
               SCARD_RESET_CARD Reset the card (Warm Reset).
               SCARD_UNPOWER_CARD Power down the card and reset it (Cold Reset).
  pdwActiveProtocol [out] Flag that indicates the established active protocol. Value Meaning
               SCARD_PROTOCOL_T0 T=0 is the active protocol.
               SCARD_PROTOCOL_T1 T=1 is the active protocol.
}

  TSCardReconnect = function (AhCard: TSCARDHANDLE; AShareMode: DWORD; APreferredProtocols: DWORD;
    AInitialization: DWORD; var AActiveProtocol: DWORD): LongInt; stdcall;

{ SCardDisconnect 13 0A
  terminates a connection previously opened between the calling application and
  smart card in the target reader.

  Parameters
  hCard          Reference value obtained from a previous call to SCardConnect.
  dwDisposition  Action to take on the card in the connected reader on close. Value Meaning
                 SCARD_LEAVE_CARD Do not do anything special.
                 SCARD_RESET_CARD Reset the card.
                 SCARD_UNPOWER_CARD Power down the card.
                 SCARD_EJECT_CARD Eject the card.
  If an application (which previously called SCardConnect) exits without calling SCardDisconnect, the card is automatically reset.
}

  TSCardDisconnect = function (AhCard: TSCARDHANDLE; ADisposition: DWORD): LongInt; stdcall;

{ SCardBeginTransaction 8 5
  starts a transaction, waiting for the completion of all other transactions before it begins.
  When the transaction starts, all other applications are blocked from accessing the smart
  card while the transaction is in progress.

  Parameters
  hCard       Reference value obtained from a previous call to SCardConnect.
}
  TSCardBeginTransaction = function (AhCard: TSCARDHANDLE): LongInt; stdcall;

{ SCardEndTransaction 14 0B
  completes a previously declared transaction, allowing other applications to resume
  interactions with the card.

  Parameters
  hCard           Reference value obtained from a previous call to SCardConnect. This value would also have been used in an earlier call to SCardBeginTransaction.
  dwDisposition   Action to take on the card in the connected reader on close. Value Meaning
                    SCARD_LEAVE_CARD Do not do anything special.
                    SCARD_RESET_CARD Reset the card.
                    SCARD_UNPOWER_CARD Power down the card.
                    SCARD_EJECT_CARD Eject the card.
}
  TSCardEndTransaction = function(AhCard: TSCARDHANDLE; ADisposition: DWORD): LongInt; stdcall;

{ SCardStatus ANSI 58 39 Unicode 59 3A
  provides the current status of a smart card in a reader. You can call it any time
  after a successful call to SCardConnect and before a successful call to SCardDisconnect.
  It does not affect the state of the reader or reader driver.

  Parameters
  hCard              Reference value returned from SCardConnect.
  szReaderName       [out] List of friendly names (multiple string) by which the currently connected reader is known.
  pcchReaderLen      [in, out] On input, supplies the length of the szReaderName buffer.
                     On output, receives the actual length (in characters) of the reader name list, including the trailing NULL character. If this buffer length is specified as SCARD_AUTOALLOCATE, then szReaderName is converted to a pointer to a byte pointer, and it receives the address of a block of memory that contains the multiple-string structure.
  pdwState           [out] Current state of the smart card in the reader. Upon success, it receives one of the following state indicators. Value Meaning
                     SCARD_ABSENT There is no card in the reader.
                     SCARD_PRESENT There is a card in the reader, but it has not been moved into position for use.
                     SCARD_SWALLOWED There is a card in the reader in position for use. The card is not powered.
                     SCARD_POWERED Power is being provided to the card, but the reader driver is unaware of the mode of the card.
                     SCARD_NEGOTIABLE The card has been reset and is awaiting PTS negotiation.
                     SCARD_SPECIFIC The card has been reset and specific communication protocols have been established.
  pdwProtocol        [out] Current protocol, if any. The returned value is meaningful only if the returned value of pdwState is SCARD_SPECIFICMODE.Value Meaning
                     SCARD_PROTOCOL_RAW The Raw Transfer protocol is in use.
                     SCARD_PROTOCOL_T0 The ISO 7816/3 T=0 protocol is in use.
                     SCARD_PROTOCOL_T1 The ISO 7816/3 T=1 protocol is in use.
  pbAtr              [out] Pointer to a 32-byte buffer that receives the ATR string from the currently inserted card, if available.
  pcbAtrLen          [in, out] On input, supplies the length of the pbAtr buffer. On output, receives the number of bytes in the ATR string (32 bytes maximum). If this buffer length is specified as SCARD_AUTOALLOCATE, then pbAtr is converted to a pointer to a byte pointer, and it receives the address of a block of memory that contains the multiple-string structure.
}
  TSCardStatusA = function(AhCard: TSCARDHANDLE; AReaderName: PANSIChar; var AReaderLen: DWORD;
    var AState: DWORD; var AProtocol: DWORD; var AAtr: Pointer; var AtrLen: DWORD): LongInt; stdcall;
  TSCardStatusW = function(AhCard: TSCARDHANDLE; AReaderName: PWideChar; var AReaderLen: DWORD;
    var AState: DWORD; var AProtocol: DWORD; var AAtr: Pointer; var AtrLen: DWORD): LongInt; stdcall;

{ SCardTransmit 60 3B
  sends a service request to the smart card and expects to receive data back from the card.

  Parameters
  hCard        Reference value returned from SCardConnect.
  pioSendPci   Pointer to the protocol header structure for the instruction. This buffer
               is in the format of an SCARD_IO_REQUEST structure, followed by specific protocol
               control information (PCI).
               For the T=0, T=1, and Raw protocols, the PCI structure is constant. smart card subsystem
               supplies a global T=0, T=1, or Raw PCI structure, which you can reference by using
               symbols SCARD_PCI_T0, SCARD_PCI_T1, and SCARD_PCI_RAW respectively.
  pbSendBuffer Pointer to the actual data to be written to the card.
               Note  For T=0, the data parameters are placed into the address pointed to by
               pbSendBuffer according to the following structure:
                record
                  bCla,   // the instruction class
                  bIns,   // the instruction code
                  bP1,    // parameter to the instruction
                  bP2,    // parameter to the instruction
                  bP3: Byte;    // size of I/O transfer
                end;
               data sent to the card should immediately follow the send buffer.
               In the special case where no data is sent to card and no data is
               expected in return, bP3 is not sent.
                Value Meaning
                bCla The T=0 instruction class
                bIns An instruction code in the T=0 instruction class
                bP1, bP2 Reference codes that complete the instruction code
                bP3 The number of data bytes to be transmitted during the command, per ISO 7816-4, Section 8.2.1.

  cbSendLength Length, in bytes, of the pbSendBuffer parameter.
               Note  For T=0, in the special case where no data is sent to the card and no data expected in return, this length must reflect that the bP3 member is not being sent; the length should be sizeof(CmdBytes) – sizeof(BYTE).
  pioRecvPci   [in, out] Pointer to the protocol header structure for the instruction, followed by a buffer in which to receive any returned protocol control information (PCI) specific to the protocol in use. This parameter can be NULL if no PCI is returned.
  pbRecvBuffer [out] Pointer to any data returned from the card.
               Note  For T=0, the data is immediately followed by the SW1 and SW2 status bytes. If no data is returned from the card, then this buffer will only contain the SW1 and SW2 status bytes.
  pcbRecvLength [in, out] Supplies the length, in bytes, of the pbRecvBuffer parameter and receives the actual number of bytes received from the smart card.
               Note  For T=0, the receive buffer must be at least two bytes long to receive the SW1 and SW2 status bytes. If this buffer length is specified as SCARD_AUTOALLOCATE, then pbRecvBuffer is converted to a pointer to a byte pointer and receives the address of a block of memory that contains the structure.

  For the T=0 protocol, the data received back are the SW1 and SW2 status codes, possibly preceded by response data. The following paragraphs provide information about the send and receive buffers used to transfer data and issue a command.
  Sending data to the card To send n bytes of data to the card, where n>0, the send and receive buffers must be formatted as follows.
  The first four bytes of the pbSendBuffer buffer contain the CLA, INS, P1, and P2 values for the T=0 operation. The fifth byte must be set to n: the size, in bytes, of the data to be transferred to the card. The next n bytes must contain the data to be sent to the card.
  The cbSendLength parameter must be set to the size of the T=0 header information (CLA, INS, P1, and P2) plus a byte that contains the length of the data to be transferred (n), plus the size of data to be sent. In this example, this is n+5.
  The pbRecvBuffer will receive the SW1 and SW2 status codes from the operation.
  The pcbRecvLength should be at least two and will be set to two upon return.
  Retrieving data from the card To receive n>0 bytes of data from the card, the send and receive buffers must be formatted as follows.
  The first four bytes of the pbSendBuffer buffer contain the CLA, INS, P1, and P2 values for the T=0 operation. The fifth byte must be set to n: the size, in bytes, of the data to be transferred from the card. If 256 bytes are to be transferred from the card, then this byte must be set to zero.
  The cbSendLength parameter must be set to five, the size of the T=0 header information.
  The pbRecvBuffer will receive the data returned from the card, immediately followed by the SW1 and SW2 status codes from the operation.
  The pcbRecvLength should be at least n+2 and will be set to n+2 upon return.
  Issuing a command without exchanging data To issue a command to the card that does not involve the exchange of data (either sent or received), the send and receive buffers must be formatted as follows.
  The pbSendBuffer buffer must contain the CLA, INS, P1, and P2 values for the T=0 operation. The P3 value is not sent. (This is to differentiate the header from the case where 256 bytes are expected to be returned.)
  The cbSendLength parameter must be set to four, the size of the T=0 header information (CLA, INS, P1, and P2).
  The pbRecvBuffer will receive the SW1 and SW2 status codes from the operation.
  The pcbRecvLength should be at least two and will be set to two upon return.
}
  TSCardTransmit = function (AhCard: TSCARDHANDLE; ApioSendPci: PSCARD_IO_REQUEST;
    ASendBuffer: PByte; ASendLength: DWORD; var ApioRecvPci: PSCARD_IO_REQUEST;
    ARecvBuffer: PByte; var ARecvLength: DWORD): LongInt; stdcall;

// ------------------- Direct Card Access Functions ----------------------------

{ SCardControl  12 9
  gives you direct control of the reader. You can call it any time after a successful
  call to SCardConnect and before a successful call to SCardDisconnect.
  effect on the state of the reader depends on the control code.

  Parameters
  hCard          Reference value returned from SCardConnect.
  dwControlCode  Control code for the operation. This value identifies the specific operation to be performed.
  lpInBuffer     Pointer to a buffer that contains the data required to perform the operation. This parameter can be NULL if the dwControlCode parameter specifies an operation that does not require input data.
  nInBufferSize  Size, in bytes, of the buffer pointed to by lpInBuffer.
  lpOutBuffer    [out] Pointer to a buffer that receives the operation's output data. This parameter can be NULL if the dwControlCode parameter specifies an operation that does not produce output data.
  nOutBufferSize Size, in bytes, of the buffer pointed to by lpOutBuffer.
  lpBytesReturned [out] Pointer to a DWORD that receives the size, in bytes, of the data stored into the buffer pointed to by lpOutBuffer.
}
  TSCardControl = function (AhCard: TSCARDHANDLE; AControlCode: DWORD;
    AInBuffer: Pointer; AInBufferSize: DWORD; AOutBuffer: Pointer;
    AOutBufferSize: DWORD; var ABytesReturned: DWORD): Longint; stdcall;

{ SCardGetAttrib 23 14
  gets the current reader attributes for the given handle. It does not affect
  state of the reader, driver, or card.

  Parameters
  hCard       Reference value returned from SCardConnect.
  dwAttrId    Identifier for the attribute to get. The following table lists possible values for dwAttrId. These values are read-only. Note that vendors may not support all attributes. Value Meaning
              SCARD_ATTR_ATR_STRING Answer to reset (ATR) string.
              SCARD_ATTR_CHANNEL_ID DWORD encoded as 0xDDDDCCCC, where DDDD = data channel type and CCCC = channel number:
              The following encodings are defined for DDDD:
              0x01 serial I/O; CCCC is a port number.
              0x02 parallel I/O; CCCC is a port number.
              0x04 PS/2 keyboard port; CCCC is zero.
              0x08 SCSI; CCCC is SCSI ID number.
              0x10 IDE; CCCC is device number.
              0x20 USB; CCCC is device number.
              0xFy vendor-defined interface with y in the range 0 through 15; CCCC is vendor defined.

              SCARD_ATTR_CHARACTERISTICS DWORD indicating which mechanical characteristics are supported. If zero, no special characteristics are supported. Note that multiple bits can be set:
              0x00000001 Card swallowing mechanism
              0x00000002 Card ejection mechanism
              0x00000004 Card capture mechanism
              All other values are reserved for future use (RFU).

              SCARD_ATTR_CURRENT_BWT Current block waiting time.
              SCARD_ATTR_CURRENT_CLK Current clock rate, in kHz.
              SCARD_ATTR_CURRENT_CWT Current character waiting time.
              SCARD_ATTR_CURRENT_D Bit rate conversion factor.
              SCARD_ATTR_CURRENT_EBC_ENCODING Current error block control encoding.
              0 = longitudinal redundancy check (LRC)

              1 = cyclical redundancy check (CRC)

              SCARD_ATTR_CURRENT_F Clock conversion factor.
              SCARD_ATTR_CURRENT_IFSC Current byte size for information field size card.
              SCARD_ATTR_CURRENT_IFSD Current byte size for information field size device.
              SCARD_ATTR_CURRENT_N Current guard time.
              SCARD_ATTR_CURRENT_PROTOCOL_TYPE DWORD encoded as 0x0rrrpppp where rrr is RFU and should be 0x000. pppp encodes the current protocol type. Whichever bit has been set indicates which ISO protocol is currently in use. (For example, if bit 0 is set, T=0 protocol is in effect.)
              SCARD_ATTR_CURRENT_W Current work waiting time.
              SCARD_ATTR_DEFAULT_CLK Default clock rate, in kHz.
              SCARD_ATTR_DEFAULT_DATA_RATE Default data rate, in bps.
              SCARD_ATTR_DEVICE_FRIENDLY_NAME Reader's friendly name.
              SCARD_ATTR_DEVICE_IN_USE Reserved for future use.
              SCARD_ATTR_DEVICE_SYSTEM_NAME Reader's system name.
              SCARD_ATTR_DEVICE_UNIT Instance of this vendor's reader attached to the computer. The first instance will be device unit 0, the next will be unit 1 (if it is the same brand of reader) and so on. Two different brands of readers will both have 0 for this value.
              SCARD_ATTR_ICC_INTERFACE_STATUS Single byte. 0 if smart card electrical contact is not active; non-zero if contact is active.
              SCARD_ATTR_ICC_PRESENCE Single byte indicating smart card presence:
              0 = not present

              1 = card present but not swallowed (applies only if reader supports smart card swallowing)

              2 = card present (and swallowed if reader supports smart card swallowing)

              4 = card confiscated.

              SCARD_ATTR_ICC_TYPE_PER_ATR Single byte indicating smart card type:
              0 = unknown type

              1 = 7816 Asynchronous

              2 = 7816 Synchronous

              Other values RFU.

              SCARD_ATTR_MAX_CLK Maximum clock rate, in kHz.
              SCARD_ATTR_MAX_DATA_RATE Maximum data rate, in bps.
              SCARD_ATTR_MAX_IFSD Maximum bytes for information file size device.
              SCARD_ATTR_POWER_MGMT_SUPPORT 0 if device does not support power down while smart card is inserted. Non-zero otherwise.
              SCARD_ATTR_PROTOCOL_TYPES DWORD encoded as 0x0rrrpppp where rrr is RFU and should be 0x000. pppp encodes the supported protocol types. A '1' in a given bit position indicates support for the associated ISO protocol, so if bits 0 and 1 are set, both T=0 and T=1 protocols are supported.
              SCARD_ATTR_VENDOR_IFD_SERIAL_NO Vendor-supplied interface device serial number.
              SCARD_ATTR_VENDOR_IFD_TYPE Vendor-supplied interface device type (model designation of reader).
              SCARD_ATTR_VENDOR_IFD_VERSION Vendor-supplied interface device version (DWORD in the form 0xMMmmbbbb where MM = major version, mm = minor version, and bbbb = build number).
              SCARD_ATTR_VENDOR_NAME Vendor name.

  pbAttr      [out] Pointer to a buffer that receives the attribute whose ID is supplied in dwAttrId. If this value is NULL, SCardGetAttrib ignores the buffer length supplied in pcbAttrLen, writes the length of the buffer that would have been returned if this parameter had not been NULL to pcbAttrLen, and returns a success code.
  pcbAttrLen  [in, out] Length of the pbAttr buffer in bytes, and receives the actual length of the received attribute If the buffer length is specified as SCARD_AUTOALLOCATE, then pbAttr is converted to a pointer to a byte pointer, and receives the address of a block of memory containing the attribute. This block of memory must be deallocated with SCardFreeMemory.
}

  TSCardGetAttrib = function (AhCard: TSCARDHANDLE; AttrId: DWORD; AAttr: Pointer;
      var AAttrLen: DWORD): Longint; stdcall;

{ SCardSetAttrib 54 35
  sets the given reader attribute for the given handle. It does not affect the state of
  reader, reader driver, or smart card. Not all attributes are supported by all readers
  (nor can they be set at all times) as many of the attributes are under direct control of
  transport protocol.

  Parameters
  hCard         Reference value returned from SCardConnect.
  dwAttrId      Identifier for the attribute to set. The values are write-only. Note that vendors may not support all attributes. Value Meaning
                SCARD_ATTR_SUPRESS_T1_IFS_REQUEST Suppress sending of T=1 IFSD packet from the reader to the card. (Can be used if the currently inserted card does not support an IFSD request.)
  pbAttr        Pointer to a buffer that supplies the attribute whose ID is supplied in dwAttrId.
  cbAttrLen     Length (in bytes) of the attribute value in the pbAttr buffer.
}

  TSCardSetAttrib = function(AhCard: TSCARDHANDLE; AAttrId: DWORD; AAttr: Pointer;
    AAttrLen: DWORD): LongInt; stdcall;


// ------------------- Smart Card other Functions ----------------

{ SCardAccessStartedEvent Order: 5 Hint: 2
  returns an event handle when an event signals that the smart card resource manager is started.
  The event-object handle can be specified in a call to one of the wait functions.

  The function returns an event HANDLE if it succeeds or NULL if it fails.
  If the function fails use GetLastError

  The event-object handle returned can be specified in a call to one of the wait
  functions. Do not close the handle returned by this function.
  When you have finished using the handle, decrement the reference count by calling
  the SCardReleaseStartedEvent().
}

  TSCardAccessStartedEvent = function: THANDLE; stdcall;

{ SCardGetCardTypeProviderName ANSI 24 15 Unicode 25 16
  returns the name of the module (dynamic link library) containing the provider for a given card name and provider type.

  Parameters
  hContext        Handle that identifies the resource manager context. The resource manager context can be set by a previous call to SCardEstablishContext. This value can be NULL if the call to SCardGetCardTypeProviderName is not directed to a specific context.
  szCardName      Name of the card type with which this provider name is associated.
  dwProviderId    Identifier for the provider associated with this card type. Value Meaning
                  SCARD_PROVIDER_PRIMARY The function retrieves the name of the smart card'sprimary service provider as a GUID string.
                  SCARD_PROVIDER_CSP The function retrieves the name of the cryptographic service provider.
  szProvider      [out] String variable which will contain the retrieved provider name upon successful completion of this function.
  pcchProvider    [in, out] Pointer to DWORD value. On input, pcchProvider supplies the length of the szProvider buffer in characters. If this value is SCARD_AUTOALLOCATE, then szProvider is converted to a pointer to a byte pointer and receives the address of a block of memory containing the string. This block of memory must be deallocated by calling SCardFreeMemory.
  On output, this value represents the actual number of characters, including the null terminator, in the szProvider variable.
}
  TSCardGetCardTypeProviderNameA = function (AhContext: TSCARDCONTEXT;
    ACardName: PANSIChar; AProviderId: DWORD; AProviderName: PANSIChar; var AProvider: DWORD): LongInt; stdcall;
  TSCardGetCardTypeProviderNameW = function (AhContext: TSCARDCONTEXT;
    ACardName: PWideChar; AProviderId: DWORD; AProviderName: PWideChar; var AProvider: DWORD): LongInt; stdcall;

{ SCardIsValidContext 36 21
  determines whether a smart card context handle is valid.

  Parameters
  hContext     Handle that identifies the resource manager context. resource manager
               context can be set by a previous call to SCardEstablishContext.
  Call this function to determine if a smart card context handle is still valid.
  Once a smart card context handle has been set by SCardEstablishContext, it may
  become invalid if the resource manager service has been shut down.
}
  TSCardIsValidContext = function (AhContext: TSCARDCONTEXT): Longint; stdcall;

{ SCardSetCardTypeProviderName ANSI 55 36 Unicode 56 37
  specifies name of the module (DLL) containing provider for given card name and
  provider type.

  Parameters
  hContext     Handle that identifies the resource manager context. The resource manager context can be set by a previous call to SCardEstablishContext. This value can be NULL if the call to SCardSetCardTypeProviderName is not directed to a specific context.
  szCardName   Name of the card type with which this provider name is associated.
  dwProviderId Identifier for the provider associated with this card type. Value Meaning
               SCARD_PROVIDER_PRIMARY The function specifies the name of the smart card's primary service provider as a GUID string.
               SCARD_PROVIDER_CSP The function specifies the name of the cryptographic service provider.
  szProvider   String variable being assigned as the provider name, representing the cryptographic service provider (CSP).
}

  TSCardSetCardTypeProviderNameA = function (AhContext: TSCARDCONTEXT; ACardName: PANSIChar;
    AProviderId: DWORD; AProvider: PANSIChar): LongInt; stdcall;
  TSCardSetCardTypeProviderNameW = function (AhContext: TSCARDCONTEXT; ACardName: PWideChar;
    AProviderId: DWORD; AProvider: PWideChar): LongInt; stdcall;

{   winscard.dll exported functions:
     1  0 ClassInstall32
     2  1 SCardAccessNewReaderEvent
  *  5  2 SCardAccessStartedEvent
  D  6  3 SCardAddReaderToGroupA
  D  7  4 SCardAddReaderToGroupW
  R  8  5 SCardBeginTransaction
  T  9  6 SCardCancel
  R 10  7 SCardConnectA
  R 11  8 SCardConnectW
  A 12  9 SCardControl
  R 13 0A SCardDisconnect
  R 14 0B SCardEndTransaction
  C 15 0C SCardEstablishContext
  D 16 0D SCardForgetCardTypeA
  D 17 0E SCardForgetCardTypeW
  D 18 0F SCardForgetReaderA
  D 19 10 SCardForgetReaderGroupA
  D 20 11 SCardForgetReaderGroupW
  D 21 12 SCardForgetReaderW
  M 22 13 SCardFreeMemory
  A 23 14 SCardGetAttrib
  * 24 15 SCardGetCardTypeProviderNameA
  * 25 16 SCardGetCardTypeProviderNameW
  Q 26 17 SCardGetProviderIdA
  Q 27 18 SCardGetProviderIdW
  T 28 19 SCardGetStatusChangeA
  T 29 1A SCardGetStatusChangeW
  D 30 1B SCardIntroduceCardTypeA
  D 31 1C SCardIntroduceCardTypeW
  D 32 1D SCardIntroduceReaderA
  D 33 1E SCardIntroduceReaderGroupA
  D 34 1F SCardIntroduceReaderGroupW
  D 35 20 SCardIntroduceReaderW
  * 36 21 SCardIsValidContext
  Q 37 22 SCardListCardsA
  Q 38 23 SCardListCardsW
  Q 39 24 SCardListInterfacesA
  Q 40 25 SCardListInterfacesW
  Q 41 26 SCardListReaderGroupsA
  Q 42 27 SCardListReaderGroupsW
  Q 43 28 SCardListReadersA
  Q 44 29 SCardListReadersW
  T 45 2A SCardLocateCardsA
  T 46 2B SCardLocateCardsByATRA
  T 47 2C SCardLocateCardsByATRW
  T 48 2D SCardLocateCardsW
  T 49 2E SCardReconnect
     3 2F SCardReleaseAllEvents
  C 50 30 SCardReleaseContext
     4 31 SCardReleaseNewReaderEvent
    51 32 SCardReleaseStartedEvent
  D 52 33 SCardRemoveReaderFromGroupA
  D 53 34 SCardRemoveReaderFromGroupW
  A 54 35 SCardSetAttrib
  * 55 36 SCardSetCardTypeProviderNameA
  * 56 37 SCardSetCardTypeProviderNameW
    57 38 SCardState
  R 58 39 SCardStatusA
  R 59 3A SCardStatusW
  R 60 3B SCardTransmit
    61 3C g_rgSCardRawPci
    62 3D g_rgSCardT0Pci
    63 3E g_rgSCardT1Pci
}

implementation

end.

