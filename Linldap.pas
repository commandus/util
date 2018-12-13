unit
  Linldap;
(*##*)
(*******************************************************************************
*                                                                             *
*   L  i  n  l  d  a  p                                                        *
*                                                                             *
*   A conversion of the Windows Lightweight Directory Access Protocol -        *
*   LDAP header file to Delphi (WLDAP32.DLL)                                  *
*   Copyright © 2002-2003 Andrei Ivanov. All rights reserved.                  *
*   Copyright © 2000, Luk Vermeulen (lvermeulen@seria.com),                   *
*     Rudy Velthuis (rvelthuis@gmx.de)                                         *
*   Copyright © 1995-1999 Microsoft Corporation.                              *
*   This module is the header file for the 32 bit LDAP client API for          *
*   Windows NT and Windows 95.  This API is based on RFC 1823 with some       *
*   enhancements for LDAP v3.                                                  *
*                                                                             *
*   Conditional defines:                                                       *
*                                                                             *
*   Revisions    : Jul 19 2001                                                 *
*   Last fix     : Jul 29 2001                                                *
*   Lines        : 1492                                                        *
*   History      :                                                            *
*   Printed      : ---                                                         *
*                                                                             *
********************************************************************************)
(*##*)

interface

type
  ULONG = longword;
  UCHAR = byte;
  LONG = longint;
  PVOID = pointer;
  USHORT = word;
  PWCHAR = pwidechar;
  INT = integer;
  PPWCHAR = ^PWCHAR;
  PPCHAR = ^PCHAR;


const
  LDAP_PORT               = 389;
  LDAP_SSL_PORT           = 636;

  LDAP_VERSION1          = 1;
  LDAP_VERSION2          = 2;
  LDAP_VERSION3          = 3;
  LDAP_VERSION           = LDAP_VERSION2;

  LDAP_BIND_CMD          = $60;   // application + constructed
  LDAP_UNBIND_CMD        = $42;   // application + primitive
  LDAP_SEARCH_CMD        = $63;   // application + constructed
  LDAP_MODIFY_CMD        = $66;   // application + constructed
  LDAP_ADD_CMD           = $68;   // application + constructed
  LDAP_DELETE_CMD        = $4a;   // application + primitive
  LDAP_MODRDN_CMD        = $6c;   // application + constructed
  LDAP_COMPARE_CMD       = $6e;   // application + constructed
  LDAP_ABANDON_CMD       = $50;   // application + primitive
  LDAP_SESSION_CMD       = $71;   // application + constructed
  LDAP_EXTENDED_CMD      = $77;   // application + constructed

  LDAP_RES_BIND           = $61;   // application + constructed
  LDAP_RES_SEARCH_ENTRY   = $64;   // application + constructed
  LDAP_RES_SEARCH_RESULT  = $65;   // application + constructed
  LDAP_RES_MODIFY         = $67;   // application + constructed
  LDAP_RES_ADD            = $69;   // application + constructed
  LDAP_RES_DELETE         = $6b;   // application + constructed
  LDAP_RES_MODRDN         = $6d;   // application + constructed
  LDAP_RES_COMPARE        = $6f;   // application + constructed
  LDAP_RES_SESSION        = $72;   // application + constructed
  LDAP_RES_REFERRAL       = $73;   // application + constructed
  LDAP_RES_EXTENDED       = $78;   // application + constructed
  LDAP_RES_ANY            = -1;

  LDAP_INVALID_CMD         = $ff;
  LDAP_INVALID_RES         = $ff;

  LDAP_SUCCESS                    =   $00;
  LDAP_OPERATIONS_ERROR           =   $01;
  LDAP_PROTOCOL_ERROR             =   $02;
  LDAP_TIMELIMIT_EXCEEDED         =   $03;
  LDAP_SIZELIMIT_EXCEEDED         =   $04;
  LDAP_COMPARE_FALSE              =   $05;
  LDAP_COMPARE_TRUE               =   $06;
  LDAP_AUTH_METHOD_NOT_SUPPORTED  =   $07;
  LDAP_STRONG_AUTH_REQUIRED       =   $08;
  LDAP_REFERRAL_V2                =   $09;
  LDAP_PARTIAL_RESULTS            =   $09;
  LDAP_REFERRAL                   =   $0a;
  LDAP_ADMIN_LIMIT_EXCEEDED       =   $0b;
  LDAP_UNAVAILABLE_CRIT_EXTENSION =   $0c;

  LDAP_NO_SUCH_ATTRIBUTE          =   $10;
  LDAP_UNDEFINED_TYPE             =   $11;
  LDAP_INAPPROPRIATE_MATCHING     =   $12;
  LDAP_CONSTRAINT_VIOLATION       =   $13;
  LDAP_ATTRIBUTE_OR_VALUE_EXISTS  =   $14;
  LDAP_INVALID_SYNTAX             =   $15;

  LDAP_NO_SUCH_OBJECT             =   $20;
  LDAP_ALIAS_PROBLEM              =   $21;
  LDAP_INVALID_DN_SYNTAX          =   $22;
  LDAP_IS_LEAF                    =   $23;
  LDAP_ALIAS_DEREF_PROBLEM        =   $24;

  LDAP_INAPPROPRIATE_AUTH         =   $30;
  LDAP_INVALID_CREDENTIALS        =   $31;
  LDAP_INSUFFICIENT_RIGHTS        =   $32;
  LDAP_BUSY                       =   $33;
  LDAP_UNAVAILABLE                =   $34;
  LDAP_UNWILLING_TO_PERFORM       =   $35;
  LDAP_LOOP_DETECT                =   $36;

  LDAP_NAMING_VIOLATION           =   $40;
  LDAP_OBJECT_CLASS_VIOLATION     =   $41;
  LDAP_NOT_ALLOWED_ON_NONLEAF     =   $42;
  LDAP_NOT_ALLOWED_ON_RDN         =   $43;
  LDAP_ALREADY_EXISTS             =   $44;
  LDAP_NO_OBJECT_CLASS_MODS       =   $45;
  LDAP_RESULTS_TOO_LARGE          =   $46;
  LDAP_AFFECTS_MULTIPLE_DSAS      =   $47;

  LDAP_OTHER                      =   $50;
  LDAP_SERVER_DOWN                =   $51;
  LDAP_LOCAL_ERROR                =   $52;
  LDAP_ENCODING_ERROR             =   $53;
  LDAP_DECODING_ERROR             =   $54;
  LDAP_TIMEOUT                    =   $55;
  LDAP_AUTH_UNKNOWN               =   $56;
  LDAP_FILTER_ERROR               =   $57;
  LDAP_USER_CANCELLED             =   $58;
  LDAP_PARAM_ERROR                =   $59;
  LDAP_NO_MEMORY                  =   $5a;

type
  LDAP_RETCODE = integer;

const
  LDAP_AUTH_SIMPLE                = $80;
  LDAP_AUTH_SASL                  = $83;

  LDAP_AUTH_OTHERKIND             = $86;

  LDAP_AUTH_SICILY                =(LDAP_AUTH_OTHERKIND or $0200);

  LDAP_AUTH_MSN                   =(LDAP_AUTH_OTHERKIND or $0800);
  LDAP_AUTH_NTLM                  =(LDAP_AUTH_OTHERKIND or $1000);
  LDAP_AUTH_DPA                   =(LDAP_AUTH_OTHERKIND or $2000);
  LDAP_AUTH_SSPI                  =(LDAP_AUTH_OTHERKIND or $0400);

  LDAP_FILTER_AND         = $a0;    // context specific + constructed - SET OF Filters.
  LDAP_FILTER_OR          = $a1;    // context specific + constructed - SET OF Filters.
  LDAP_FILTER_NOT         = $a2;    // context specific + constructed - Filter
  LDAP_FILTER_EQUALITY    = $a3;    // context specific + constructed - AttributeValueAssertion.
  LDAP_FILTER_SUBSTRINGS  = $a4;    // context specific + constructed - SubstringFilter
  LDAP_FILTER_GE          = $a5;    // context specific + constructed - AttributeValueAssertion.
  LDAP_FILTER_LE          = $a6;    // context specific + constructed - AttributeValueAssertion.
  LDAP_FILTER_PRESENT     = $87;    // context specific + primitive   - AttributeType.
  LDAP_FILTER_APPROX      = $a8;    // context specific + constructed - AttributeValueAssertion.

  LDAP_SUBSTRING_INITIAL  = $80;   // class context specific
  LDAP_SUBSTRING_ANY      = $81;   // class context specific
  LDAP_SUBSTRING_FINAL    = $82;   // class context specific


  LDAP_DEREF_NEVER        = 0;
  LDAP_DEREF_SEARCHING    = 1;
  LDAP_DEREF_FINDING      = 2;
  LDAP_DEREF_ALWAYS       = 3;

  LDAP_NO_LIMIT       = 0;

  LDAP_OPT_DNS                = $00000001;  // utilize DN & DNS
  LDAP_OPT_CHASE_REFERRALS    = $00000002;  // chase referrals
  LDAP_OPT_RETURN_REFS        = $00000004;  // return referrals to calling app

{$ALIGN ON}

type
  PLDAP = ^LDAP;
  LDAP = record
    ld_sb: record
      sb_sd: ULONG;
      Reserved1: array [0..(10*sizeof(ULONG))] of UCHAR;
      sb_naddr: ULONG;   // notzero implies CLDAP available
      Reserved2: array [0..(6*sizeof(ULONG))] of UCHAR;
    end;

    ld_host: PCHAR;
    ld_version: ULONG;
    ld_lberoptions: UCHAR;
    ld_deref: ULONG;
    ld_timelimit: ULONG;
    ld_sizelimit: ULONG;
    ld_errno: ULONG;
    ld_matched: PCHAR;
    ld_error: PCHAR;
    ld_msgid: ULONG;
    Reserved3: array  [0..(6*sizeof(ULONG))] of UCHAR;
    ld_cldaptries: ULONG;
    ld_cldaptimeout: ULONG;
    ld_refhoplimit: ULONG;
    ld_options: ULONG;
  end;

  PLDAP_TIMEVAL = ^LDAP_TIMEVAL;
  LDAP_TIMEVAL = record
    tv_sec: LONG;
    tv_usec: LONG;
  end;

  PPLDAP_BERVAL = ^PLDAP_BERVAL;
  PLDAP_BERVAL = ^LDAP_BERVAL;
  LDAP_BERVAL = record
    bv_len: ULONG;
    bv_val: PCHAR;
  end;

  PPLDAPMessage = ^PLDAPMessage;
  PLDAPMessage = ^LDAPMessage;
  LDAPMessage = record
    lm_msgid: ULONG;             // message number for given connection
    lm_msgtype: ULONG;           // message type of the form LDAP_RES_xxx
    lm_ber: PVOID;               // ber form of message
    lm_chain: PLDAPMessage;      // pointer to next result value
    lm_next: PLDAPMessage;       // pointer to next message
    lm_time: ULONG;
    Connection: PLDAP;           // connection from which we received response
    Request: PVOID;              // owning request(opaque structure)
    lm_returncode: ULONG;        // server's return code
    lm_hopcount: USHORT;         // hop count for number of referrals followed
  end;

const
  LDAP_MOD_ADD            = $00;
  LDAP_MOD_DELETE         = $01;
  LDAP_MOD_REPLACE        = $02;
  LDAP_MOD_NOCHANGE       = $0F;
  LDAP_MOD_BVALUES        = $80;

type
  PLDAPMod = ^LDAPMod;
  LDAPMod = packed record
    mod_op: ULONG;
    mod_type: PCHAR;
    modv_strvals: array of PChar;
//    modvals: record
//      case integer of
//        0:(modv_strvals: ^PCHAR);
//        1:(modv_bvals: ^PLDAP_BERVAL);
//    end;
  end;

// XXX #pragma pack(pop)
{$ALIGN OFF}

//
//  macros compatible with reference implementation...
//

function LDAP_IS_CLDAP(ld: PLDAP): boolean;
function NAME_ERROR(n: integer): boolean;

function ldap_open(HostName: PCHAR; PortNumber: ULONG): PLDAP; cdecl;
function ldap_init(HostName: PCHAR; PortNumber: ULONG): PLDAP; cdecl;
function ldap_sslinit(HostName: PCHAR; PortNumber: ULONG; secure: integer): PLDAP; cdecl;

function cldap_open(HostName: PCHAR; PortNumber: ULONG): PLDAP; cdecl;

function ldap_unbind(ld: PLDAP): ULONG; cdecl;
function ldap_unbind_s(ld: PLDAP): ULONG; cdecl; // calls ldap_unbind

function ldap_get_option(ld: PLDAP; option: integer; outvalue: pointer): ULONG; cdecl;
function ldap_set_option(ld: PLDAP; option: integer; invalue: pointer): ULONG; cdecl;

const
  LDAP_OPT_DESC               = $01;
  LDAP_OPT_DEREF              = $02;
  LDAP_OPT_SIZELIMIT          = $03;
  LDAP_OPT_TIMELIMIT          = $04;
  LDAP_OPT_THREAD_FN_PTRS     = $05;
  LDAP_OPT_REBIND_FN          = $06;
  LDAP_OPT_REBIND_ARG         = $07;
  LDAP_OPT_REFERRALS          = $08;
  LDAP_OPT_RESTART            = $09;

  LDAP_OPT_IO_FN_PTRS         = $0a;
  LDAP_OPT_CACHE_FN_PTRS      = $0c;
  LDAP_OPT_CACHE_STRATEGY     = $0d;
  LDAP_OPT_CACHE_ENABLE       = $0e;
  LDAP_OPT_SSL                = $0f;
  LDAP_OPT_VERSION            = $10;
  LDAP_OPT_SORTKEYS           = $11;

//
//  These are new ones that we've defined, not in current RFC draft.
//

  LDAP_OPT_HOST_NAME          = $30;
  LDAP_OPT_ERROR_NUMBER       = $31;
  LDAP_OPT_ERROR_STRING       = $32;


  LDAP_OPT_ON                 = pointer(1);
  LDAP_OPT_OFF                = pointer(0);

function ldap_simple_bind(ld: PLDAP; dn: PCHAR; passwd: PCHAR): ULONG; cdecl;
function ldap_simple_bind_s(ld: PLDAP; dn: PCHAR; passwd: PCHAR): ULONG; cdecl;
function ldap_bind(ld: PLDAP; dn: PCHAR; cred: PCHAR; method: ULONG): ULONG; cdecl;
function ldap_bind_s(ld: PLDAP; dn: PCHAR; cred: PCHAR; method: ULONG): ULONG; cdecl;

const
  LDAP_SCOPE_BASE         = $00;
  LDAP_SCOPE_ONELEVEL     = $01;
  LDAP_SCOPE_SUBTREE      = $02;
function ldap_search(
        ld:        PLDAP;
        base:      PCHAR;         // distinguished name or ""
        scope:     ULONG;         // LDAP_SCOPE_xxxx
        filter:    PCHAR;
        attrs:     PCHAR;         // pointer to an array of PCHAR attribute names
        attrsonly: ULONG          // boolean on whether to only return attr names
   ): ULONG; cdecl;
function ldap_search_s(
        ld:        PLDAP;
        base:      PCHAR;
        scope:     ULONG;
        filter:    PCHAR;
        attrs:     PCHAR;
        attrsonly: ULONG;
        res:       PPLDAPMessage
   ): ULONG; cdecl;
function ldap_search_st(
        ld:        PLDAP;
        base:      PCHAR;
        scope:     ULONG;
        filter:    PCHAR;
        attrs:     PCHAR;
        attrsonly: ULONG;
        timeout:   PLDAP_TIMEVAL;
        res:       PPLDAPMessage
   ): ULONG; cdecl;

function ldap_modify(ld: PLDAP; dn: PCHAR; mods: PLDAPMod): ULONG; cdecl;
function ldap_modify_s(ld: PLDAP; dn: PCHAR; mods: PLDAPMod): ULONG; cdecl;

function ldap_modrdn2(
    ExternalHandle:       PLDAP;
    DistinguishedName:    PCHAR;
    NewDistinguishedName: PCHAR;
    DeleteOldRdn:         INT
   ): ULONG; cdecl;
function ldap_modrdn(
    ExternalHandle:       PLDAP;
    DistinguishedName:    PCHAR;
    NewDistinguishedName: PCHAR
   ): ULONG; cdecl;
function ldap_modrdn2_s(
    ExternalHandle:       PLDAP;
    DistinguishedName:    PCHAR;
    NewDistinguishedName: PCHAR;
    DeleteOldRdn:         INT
   ): ULONG; cdecl;
function ldap_modrdn_s(
    ExternalHandle:       PLDAP;
    DistinguishedName:    PCHAR;
    NewDistinguishedName: PCHAR
   ): ULONG; cdecl;

function ldap_add(ld: PLDAP; dn: PCHAR; attrs: PLDAPMod): ULONG; cdecl; stdcall
function ldap_add_s(ld: PLDAP; dn: PCHAR; attrs: PLDAPMod): ULONG; cdecl;

function ldap_compare(ld: PLDAP; dn: PCHAR; attr: PCHAR; value: PCHAR): ULONG; cdecl;
function ldap_compare_s(ld: PLDAP; dn: PCHAR; attr: PCHAR; value: PCHAR): ULONG; cdecl;

function ldap_delete(ld: PLDAP; dn: PCHAR): ULONG; cdecl;
function ldap_delete_s(ld: PLDAP; dn: PCHAR): ULONG; cdecl;

function ldap_abandon(ld: PLDAP; msgid: ULONG): ULONG; cdecl;


const
  LDAP_MSG_ONE       = 0;
  LDAP_MSG_ALL       = 1;
  LDAP_MSG_RECEIVED  = 2;

function ldap_result(
        ld:       PLDAP;
        msgid:    ULONG;
        all:      ULONG;
        timeout:  PLDAP_TIMEVAL;
        res:      PPLDAPMessage
   ): ULONG; cdecl;

function ldap_msgfree(res: PLDAPMessage): ULONG; cdecl;

function ldap_result2error(
        ld:      PLDAP;
        res:     PLDAPMessage;
        freeit:  ULONG             // boolean.. free the message?
   ): ULONG; cdecl;

function ldap_err2string(err: ULONG): PCHAR; cdecl;
procedure ldap_perror(ld: PLDAP; msg: PCHAR); cdecl;

function ldap_first_entry(ld: PLDAP; res: PLDAPMessage): PLDAPMessage; cdecl;
function ldap_next_entry(ld: PLDAP; entry: PLDAPMessage): PLDAPMessage; cdecl;
function ldap_count_entries(ld: PLDAP; res: PLDAPMessage): ULONG; cdecl;

type
  PBerElement = ^BerElement;
  BerElement = record
    opaque: PCHAR;      // this is an opaque structure used just for
                        // compatibility with reference implementation
  end;

const
  NULLBER = PBerElement(0);

function ldap_first_attribute(
        ld:      PLDAP;
        entry:   PLDAPMessage;
        var ptr: PBerElement
       ): PCHAR; cdecl;

function ldap_next_attribute(
        ld:      PLDAP;
        entry:   PLDAPMessage;
        ptr:     PBerElement
       ): PCHAR; cdecl;

function ldap_get_values(
        ld:      PLDAP;
        entry:   PLDAPMessage;
        attr:    PCHAR
       ): PPCHAR; cdecl;

function ldap_get_values_len(
    ExternalHandle:   PLDAP;
    Message:          PLDAPMessage;
    attr:             PCHAR
   ): PPLDAP_BERVAL; cdecl;

function ldap_count_values(vals: PPCHAR): ULONG; cdecl;

function ldap_count_values_len(vals: PPLDAP_BERVAL): ULONG; cdecl;
function ldap_value_free(vals: PPCHAR): ULONG; cdecl;
function ldap_value_free_len(vals: PPLDAP_BERVAL): ULONG; cdecl;
function ldap_get_dn(ld: PLDAP; entry: PLDAPMessage): PCHAR; cdecl;
function ldap_explode_dn(dn: PCHAR; notypes: ULONG): PPCHAR; cdecl;
function ldap_dn2ufn(dn: PCHAR): PCHAR; cdecl;
procedure ldap_memfree(Block: PCHAR); cdecl;
function ldap_ufn2dn(ufn: PCHAR; pDn: PPCHAR): ULONG; cdecl;

const
  LBER_USE_DER            = $01;
  LBER_USE_INDEFINITE_LEN = $02;
  LBER_TRANSLATE_STRINGS  = $04;

  LAPI_MAJOR_VER1     = 1;
  LAPI_MINOR_VER1     = 1;

type
  PLDAP_VERSION_INFO = ^LDAP_VERSION_INFO;
  LDAP_VERSION_INFO = record
     lv_size: ULONG;
     lv_major: ULONG;
     lv_minor: ULONG;
  end;

function ldap_startup(
    version: PLDAP_VERSION_INFO
   ): ULONG; cdecl;


function ldap_cleanup : ULONG; cdecl;


function ldap_escape_filter_element(
   sourceFilterElement:  PCHAR;
   sourceLength:         ULONG;
   destFilterElement:    PCHAR;
   destLength:           ULONG
  ): ULONG; cdecl;

function ldap_set_dbg_flags(NewFlags: ULONG): ULONG; cdecl;


implementation
{$IFDEF LINUX}
const
  sLDAPLIB = 'libldapssl41.so';
{$ENDIF}

{$IFDEF WIN32}
const
  sLDAPLIB = 'wldap32.dll';
{$ENDIF}

function ldap_open; external sLDAPLIB name 'ldap_open';
function ldap_init; external sLDAPLIB name 'ldap_init';
function ldap_sslinit; external sLDAPLIB name 'ldap_sslinit';
function cldap_open; external sLDAPLIB name 'cldap_open';
function ldap_simple_bind; external sLDAPLIB name 'ldap_simple_bind';
function ldap_simple_bind_s; external sLDAPLIB name 'ldap_simple_bind_s';
function ldap_bind; external sLDAPLIB name 'ldap_bind';
function ldap_bind_s; external sLDAPLIB name 'ldap_bind_s';
function ldap_search; external sLDAPLIB name 'ldap_search';
function ldap_search_s; external sLDAPLIB name 'ldap_search_s';
function ldap_search_st; external sLDAPLIB name 'ldap_search_st';
function ldap_modify; external sLDAPLIB name 'ldap_modify';
function ldap_modify_s; external sLDAPLIB name 'ldap_modify_s';
function ldap_modrdn2; external sLDAPLIB name 'ldap_modrdn2';
function ldap_modrdn; external sLDAPLIB name 'ldap_modrdn';
function ldap_modrdn2_s; external sLDAPLIB name 'ldap_modrdn2_s';
function ldap_modrdn_s; external sLDAPLIB name 'ldap_modrdn_s';
function ldap_add; external sLDAPLIB name 'ldap_add';
function ldap_add_s; external sLDAPLIB name 'ldap_add_s';
function ldap_compare; external sLDAPLIB name 'ldap_compare';
function ldap_compare_s; external sLDAPLIB name 'ldap_compare_s';
function ldap_delete; external sLDAPLIB name 'ldap_delete';
function ldap_delete_s; external sLDAPLIB name 'ldap_delete_s';
function ldap_err2string; external sLDAPLIB name 'ldap_err2string';
function ldap_first_attribute; external sLDAPLIB name 'ldap_first_attribute';
function ldap_next_attribute; external sLDAPLIB name 'ldap_next_attribute';
function ldap_get_values; external sLDAPLIB name 'ldap_get_values';
function ldap_get_values_len; external sLDAPLIB name 'ldap_get_values_len';
function ldap_count_values; external sLDAPLIB name 'ldap_count_values';
function ldap_value_free; external sLDAPLIB name 'ldap_value_free';
function ldap_get_dn; external sLDAPLIB name 'ldap_get_dn';
function ldap_explode_dn; external sLDAPLIB name 'ldap_explode_dn';
function ldap_dn2ufn; external sLDAPLIB name 'ldap_dn2ufn';
procedure ldap_memfree; external sLDAPLIB name 'ldap_memfree';
function ldap_unbind; external sLDAPLIB name 'ldap_unbind';
function ldap_unbind_s; external sLDAPLIB name 'ldap_unbind_s';
function ldap_get_option; external sLDAPLIB name 'ldap_get_option';
function ldap_set_option; external sLDAPLIB name 'ldap_set_option';
function ldap_abandon; external sLDAPLIB name 'ldap_abandon';
function ldap_ufn2dn; external sLDAPLIB name 'ldap_ufn2dn';
function ldap_escape_filter_element; external sLDAPLIB name 'ldap_escape_filter_element';
function ldap_result; external sLDAPLIB name 'ldap_result';
function ldap_msgfree; external sLDAPLIB name 'ldap_msgfree';
function ldap_result2error; external sLDAPLIB name 'ldap_result2error';
procedure ldap_perror; external sLDAPLIB name 'ldap_perror';
function ldap_first_entry; external sLDAPLIB name 'ldap_first_entry';
function ldap_next_entry; external sLDAPLIB name 'ldap_next_entry';
function ldap_count_entries; external sLDAPLIB name 'ldap_count_entries';
function ldap_count_values_len; external sLDAPLIB name 'ldap_count_entries_len';
function ldap_value_free_len; external sLDAPLIB name 'ldap_value_free_len';
function ldap_startup; external sLDAPLIB name 'ldap_startup';
function ldap_cleanup; external sLDAPLIB name 'ldap_cleanup';
function ldap_set_dbg_flags; external sLDAPLIB name 'ldap_set_dbg_flags';

function LDAP_IS_CLDAP(ld: PLDAP): boolean;
begin
  Result :=(ld^.ld_sb.sb_naddr > 0);
end;

function NAME_ERROR(n: integer): boolean;
begin
  Result :=((n and $f0) = $20);
end;

{# $OpenLDAP: pkg/ldap/servers/slapd/schema/inetorgperson.schema,v 1.4.2.6 2001/10/09 17:15:08 kurt Exp $
#
# InetOrgPerson (RFC2798)
#
# Depends upon
#   Definition of an X.500 Attribute Type and an Object Class to Hold
#   Uniform Resource Identifiers (URIs) [RFC2079]
#       (core.schema)
#   
#   A Summary of the X.500(96) User Schema for use with LDAPv3 [RFC2256]
#       (core.schema)
#
#   The COSINE and Internet X.500 Schema [RFC1274] (cosine.schema)
   
# carLicense
# This multivalued field is used to record the values of the license or
# registration plate associated with an individual.
attributetype ( 2.16.840.1.113730.3.1.1
        NAME 'carLicense'
        DESC 'RFC2798: vehicle license or registration plate'
        EQUALITY caseIgnoreMatch
        SUBSTR caseIgnoreSubstringsMatch
        SYNTAX 1.3.6.1.4.1.1466.115.121.1.15 )

# departmentNumber
# Code for department to which a person belongs.  This can also be
# strictly numeric (e.g., 1234) or alphanumeric (e.g., ABC/123).
attributetype ( 2.16.840.1.113730.3.1.2
        NAME 'departmentNumber'
        DESC 'RFC2798: identifies a department within an organization'
        EQUALITY caseIgnoreMatch
        SUBSTR caseIgnoreSubstringsMatch
        SYNTAX 1.3.6.1.4.1.1466.115.121.1.15 )

# displayName
# When displaying an entry, especially within a one-line summary list, it
# is useful to be able to identify a name to be used.  Since other attri-
# bute types such as 'cn' are multivalued, an additional attribute type is
# needed.  Display name is defined for this purpose.
attributetype ( 2.16.840.1.113730.3.1.241
        NAME 'displayName'
        DESC 'RFC2798: preferred name to be used when displaying entries'
        EQUALITY caseIgnoreMatch
        SUBSTR caseIgnoreSubstringsMatch
        SYNTAX 1.3.6.1.4.1.1466.115.121.1.15
        SINGLE-VALUE )

# employeeNumber
# Numeric or alphanumeric identifier assigned to a person, typically based
# on order of hire or association with an organization.  Single valued.
attributetype ( 2.16.840.1.113730.3.1.3
        NAME 'employeeNumber'
        DESC 'RFC2798: numerically identifies an employee within an organization'
        EQUALITY caseIgnoreMatch
        SUBSTR caseIgnoreSubstringsMatch
        SYNTAX 1.3.6.1.4.1.1466.115.121.1.15
        SINGLE-VALUE )

# employeeType
# Used to identify the employer to employee relationship.  Typical values
# used will be "Contractor", "Employee", "Intern", "Temp", "External", and
# "Unknown" but any value may be used.
attributetype ( 2.16.840.1.113730.3.1.4
        NAME 'employeeType'
        DESC 'RFC2798: type of employment for a person'
        EQUALITY caseIgnoreMatch
        SUBSTR caseIgnoreSubstringsMatch
        SYNTAX 1.3.6.1.4.1.1466.115.121.1.15 )

# jpegPhoto
# Used to store one or more images of a person using the JPEG File
# Interchange Format [JFIF].
# Note that the jpegPhoto attribute type was defined for use in the
# Internet X.500 pilots but no referencable definition for it could be
# located.    
attributetype ( 0.9.2342.19200300.100.1.60
        NAME 'jpegPhoto'
        DESC 'a JPEG image'
        SYNTAX 1.3.6.1.4.1.1466.115.121.1.28 )

# preferredLanguage
# Used to indicate an individual's preferred written or spoken
# language.  This is useful for international correspondence or human-
# computer interaction.  Values for this attribute type MUST conform to
# the definition of the Accept-Language header field defined in
# [RFC2068] with one exception:  the sequence "Accept-Language" ":"
# should be omitted.  This is a single valued attribute type.
attributetype ( 2.16.840.1.113730.3.1.39
        NAME 'preferredLanguage'
        DESC 'RFC2798: preferred written or spoken language for a person'
        EQUALITY caseIgnoreMatch
        SUBSTR caseIgnoreSubstringsMatch
        SYNTAX 1.3.6.1.4.1.1466.115.121.1.15
        SINGLE-VALUE )

# userSMIMECertificate
# A PKCS#7 [RFC2315] SignedData, where the content that is signed is
# ignored by consumers of userSMIMECertificate values.  It is
# recommended that values have a `contentType' of data with an absent
# `content' field.  Values of this attribute contain a person's entire
# certificate chain and an smimeCapabilities field [RFC2633] that at a
# minimum describes their SMIME algorithm capabilities.  Values for
# this attribute are to be stored and requested in binary form, as
# 'userSMIMECertificate;binary'.  If available, this attribute is
# preferred over the userCertificate attribute for S/MIME applications.
## OpenLDAP note: ";binary" transfer should NOT be used as syntax is binary
attributetype ( 2.16.840.1.113730.3.1.40
        NAME 'userSMIMECertificate'
        DESC 'RFC2798: PKCS#7 SignedData used to support S/MIME'
        SYNTAX 1.3.6.1.4.1.1466.115.121.1.5 )

# userPKCS12
# PKCS #12 [PKCS12] provides a format for exchange of personal identity
# information.  When such information is stored in a directory service,
# the userPKCS12 attribute should be used. This attribute is to be stored
# and requested in binary form, as 'userPKCS12;binary'.  The attribute
# values are PFX PDUs stored as binary data.
## OpenLDAP note: ";binary" transfer should NOT be used as syntax is binary
attributetype ( 2.16.840.1.113730.3.1.216
        NAME 'userPKCS12'
        DESC 'RFC2798: PKCS #12 PFX PDU for exchange of
                personal identity information'
        SYNTAX 1.3.6.1.4.1.1466.115.121.1.5 )


# inetOrgPerson
# The inetOrgPerson represents people who are associated with an
# organization in some way.  It is a structural class and is derived
# from the organizationalPerson which is defined in X.521 [X521].
objectclass     ( 2.16.840.1.113730.3.2.2
    NAME 'inetOrgPerson'
        DESC 'RFC2798: Internet Organizational Person'
    SUP organizationalPerson
    STRUCTURAL
        MAY (
                audio $ businessCategory $ carLicense $ departmentNumber $
                displayName $ employeeNumber $ employeeType $ givenName $
                homePhone $ homePostalAddress $ initials $ jpegPhoto $
                labeledURI $ mail $ manager $ mobile $ o $ pager $
                photo $ roomNumber $ secretary $ uid $ userCertificate $
                x500uniqueIdentifier $ preferredLanguage $
                userSMIMECertificate $ userPKCS12 )
        )
        }
end.

