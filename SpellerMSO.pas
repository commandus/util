unit
  SpellerMSO;
(*##*)
(*******************************************************************************
*                                                                             *
*   S  p  e  l  e  r  M  S  O                                                  *
*                                                                             *
*   Copyright © 2001, 2002 Andrei Ivanov. All rights reserved.                 *
*   Based on Andrew Baylis Interface with Word spellchecker wrapper 07/2001   *
*   Conditional defines:                                                       *
*                                                                             *
*   Revisions    : Jul 06 2001, Oct 11 2001                                    *
*   Last revision: Mar 29 2002                                                *
*   Lines        : 446                                                         *
*   History      : see todo file                                              *
*                                                                              *
*                                                                             *
*   Printed      :                                                             *
*                                                                             *
********************************************************************************)
(*##*)

interface

uses
  Windows, Messages, SysUtils, Classes,
  ComObj, OLECtrls, Variants, ActiveX, Registry,
  jclUnicode;

type
  TMSOSpellChecker = class(TComponent)
  private
    FChangedText: WideString;
    FConnected: Boolean;
    FHandle: HWND;
    FHookCaption: Boolean;
    FNumChanges: Integer;
    FOleOn: Boolean;
    FSpellCaption: String;
    FWordApp, FRange, FADoc, FCustDics: OLEVariant;
    FWordVersion: String;
    hMapObject: Cardinal;
    pMem: Pointer;
    function GetCheckGWS: Boolean;
    function GetGrammarErrors: Integer;
    function GetSpellChecked: Boolean;
    function GetSpellErrors: Integer;
    procedure SetCheckGWS(const Value: Boolean);
    procedure SetHookCaption(Value: Boolean);
    procedure SetSpellCaption(const Value: String);
  protected
    function Internal_CheckGrammar: Boolean;
    function Internal_CheckSpelling: Boolean;
    procedure SetHook;
    procedure UnSetHook;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CheckClipboardGrammar: Boolean;
    function CheckClipboardSpell: Boolean;
    function CheckGrammar(const Text: WideString): Boolean;
    function CheckSpelling(const Text: WideString): Boolean;
    procedure ClearText;
    procedure Connect;
    procedure Disconnect;
    function AddCustomDic(const FileName: String): Integer;
    procedure RemoveCustomDic(const Index: Integer); overload;
    procedure RemoveCustomDic(const Name: String); overload;

    procedure ResetIgnoreAll;
    procedure DialogSpellingOptions;

    property ChangedText: WideString read FChangedText;
    property CheckGrammarWithSpelling: Boolean read GetCheckGWS write SetCheckGWS;
    property Connected: Boolean read FConnected;
    property GrammarErrorCount: Integer read GetGrammarErrors;
    property NumChanges: Integer read FNumChanges;
    property SpellChecked: Boolean read GetSpellChecked;
    property SpellErrorCount: Integer read GetSpellErrors;
    property WordVersion: String read FWordVersion;
  published
    property HookCaption: Boolean read FHookCaption write SetHookCaption;
    property SpellCaption: String read FSpellCaption write SetSpellCaption;
  end;

  function IsMSOInstalled: Boolean;

implementation

// these constants used to implement the shared memory space
// using a FileMapping to hold two strings (up to 255 char each)
const
  DEF_REG_WORD_CLASS = 'Word.Application';
  DEF_REG_EXCEL_CLASS = 'Excel.Application';

  ERR_INIT_MSWORD = 'Unable to initialise MS Word';
  ERR_INIT_COMLIB = 'Error %d initializing COM library';

  MSDialogWndClass2002 = 'bosa_sdm_Microsoft Word 10.0';
  MSDialogWndClass2000 = 'bosa_sdm_Microsoft Word 9.0';
  MSDialogWndClass97 = 'bosa_sdm_Microsoft Word 8.0';
  MSWordWndClass = 'OpusApp';

  DEF_FILEMAPPING_NAME = 'EmsoSpell_Share';
  DEF_SHARE_MEM = 512;
  DEF_CAPTION_START = 0;
  DEF_CLASS_START = DEF_SHARE_MEM div 2;

  // memspace looks like this: |..Caption...|...WindowsClassID..|
  // Constants for MS Word

type
    pTCWPRetStruct = ^TCWPRetStruct;

var
  nHook: HHOOK;

function IsMSOInstalled: Boolean;
var
  reg: TRegistry;
begin
  reg:= TRegistry.Create;
  try
    reg.RootKey:= HKEY_CLASSES_ROOT;
    Result:= reg.KeyExists(DEF_REG_WORD_CLASS) or (reg.KeyExists(DEF_REG_EXCEL_CLASS));
  finally
    reg.Free;
  end;
end;

function MessWatch(nCode, wParam, lParam: Integer): Integer; stdcall;
var
  p: pTCWPRetStruct;
  h: HWND;
  pMem: Pointer;
  q: PChar;
  hMapObject: Cardinal;
begin
  Result:= 0;
  if nCode < 0 then
    Result:= CallNextHookEx(nHook, nCode, wParam, lParam)
  else begin
    p:= pTCWPRetStruct(lParam);
    if (P.Message = WM_NCPAINT) then begin //wait for NC area to be drawn
      //open shared memory
      hMapObject:= OpenFileMapping(FILE_MAP_READ, False, DEF_FILEMAPPING_NAME);
      pMem:= MapViewOfFile(hMapObject, FILE_MAP_READ, 0, 0, 0);
      if (pMem <> nil) then begin
        q:= pMem;
        h:= FindWindow(q + DEF_CLASS_START, nil);
        if (h <> 0)
        then SetWindowText(h, q + DEF_CAPTION_START);
      end;
      CloseHandle(hMapObject); //close shared memory
    end;
  end;
end;

function TMSOSpellChecker.GetCheckGWS: Boolean;
begin
  Result:= False;
  if FConnected
  then Result:= FWordApp.Options.CheckGrammarWithSpelling;
end;

function TMSOSpellChecker.GetGrammarErrors: Integer;
begin
  if FConnected
  then Result:= FRange.GrammaticalErrors.Count
  else Result:= 0;
end;

// returns false if spelling has yet to be checked
function TMSOSpellChecker.GetSpellChecked: Boolean;
begin
  if FConnected
  then Result:= not FRange.SpellingChecked
  else Result:= True;
end;

function TMSOSpellChecker.GetSpellErrors: Integer;
begin
  if FConnected
  then Result:= FRange.SpellingErrors.Count
  else Result:= 0;
end;

procedure TMSOSpellChecker.SetCheckGWS(const Value: Boolean);
begin
  if FConnected
  then FWordApp.Options.CheckGrammarWithSpelling:= Value;
end;

procedure TMSOSpellChecker.SetHookCaption(Value: Boolean);
begin
  FHookCaption:= Value;
  if not (csDesigning in ComponentState) then begin
    if Value
    then SetHook
    else UnSetHook;
  end;
end;

procedure TMSOSpellChecker.SetSpellCaption(const Value: String);
var
  p: PChar;
begin
  FSpellCaption:= Value;
  if FConnected then begin
     // copy to shared memory
    p:= pMem;
    if (p <> nil)
    then StrCopy(p + DEF_CAPTION_START, PChar(FSpellCaption));
  end;
end;

function TMSOSpellChecker.Internal_CheckGrammar: Boolean;
begin
  // ensures dialogs appear in front
  SetWindowPos(FHandle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOACTIVATE + SWP_HIDEWINDOW);
  // note if changes are made
  FADoc.TrackRevisions:= True;
  FNumChanges:= 0;
  OleCheck(FRange.CheckGrammar);
  // need to stop ActiveDocument appearing
  FWordApp.Visible:= False;
  // seems revisions counts the old word and the new one separately
  FNumChanges:= FRange.Revisions.Count div 2;
  Result:= (FRange.Revisions.Count > 0);
  if Result
  then FRange.Revisions.AcceptAll; // accept all changes
  FADoc.TrackRevisions:= False; // don't track future changes
end;

function TMSOSpellChecker.Internal_CheckSpelling: Boolean;
begin
  // ensures dialogs appear in front }
  SetWindowPos(FHandle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOACTIVATE + SWP_HIDEWINDOW);

  FADoc.TrackRevisions:= True; // note if changes are made
  FNumChanges:= 0;
  OleCheck(FADoc.CheckGrammar);

  FWordApp.Visible:= False; // need to stop ActiveDocument appearing
  FNumChanges:= FRange.Revisions.Count div 2; // seems revisions counts the old word and the new one separately
  Result:= (FRange.Revisions.Count > 0);
  if Result then FRange.Revisions.AcceptAll; // accept all changes
  FADoc.TrackRevisions:= False; // don't track future changes
end;

procedure TMSOSpellChecker.SetHook;
begin
  if (nHook = 0)
  then nHook:= SetWindowsHookEx(WH_CALLWNDPROCRET, MessWatch, HInstance, 0);
end;

procedure TMSOSpellChecker.UnSetHook;
begin
  if (nHook <> 0)
  then UnHookWindowsHookEx(nHook);
  nHook:= 0;
end;

function TMSOSpellChecker.AddCustomDic(const FileName: String): Integer;
begin
  FCustDics.Add(FileName);
  Result:= FCustDics.Count;
end;

// returns true if changes were made. Corrected text is on the clipboard
function TMSOSpellChecker.CheckClipboardGrammar: Boolean;
begin
  Result:= False;
  if not FConnected then Connect;
  if not FConnected then Exit; // if still not connected then no MS Word!

  FRange.Paste; // replace with new text to check
  Result:= Internal_CheckGrammar;
  if Result
  then FRange.Copy;
end;

// returns true if changes were made. Corrected text is on the clipboard
function TMSOSpellChecker.CheckClipboardSpell: Boolean;
begin
  Result:= False;
  if not FConnected then Connect;
  if not FConnected then Exit; // if still not connected then no MS Word!
  FRange.Paste; // replace with new text to check
  Result:= Internal_checkSpelling;
  if Result then FRange.Copy; // put onto clipboard
end;

// returns true if changes were made and the corrected text is placed in the Text String
function TMSOSpellChecker.CheckGrammar(const Text: WideString): Boolean;
begin
  Result:= False;
  if not FConnected then Connect;
  if not FConnected then Exit; // if still not connected then no MS Word!
  FChangedText:= '';
  FRange.Text:= Text; // replace with new text to check
  Result:= Internal_CheckGrammar;
  if Result
  then FChangedText:= FRange.Text;
end;

function TMSOSpellChecker.CheckSpelling(const Text: WideString): Boolean;
// returns true if changes were made and the corrected text is
// placed in the Text String
begin
  Result:= False;
  if not FConnected then Connect;
  if not FConnected then Exit; // if still not connected then no MS Word!
  FChangedText:= '';
  FRange.Text:= Text; // replace with new text to check
  Result:= Internal_CheckSpelling;
  if Result
  then FChangedText:= FRange.Text;
end;

procedure TMSOSpellChecker.ClearText;
begin
  if FConnected then FRange.Text:= '';
end;

procedure TMSOSpellChecker.Connect;
var
  s: String;
  p: PChar;
begin
  if FConnected then Exit; // don't create two instances
  try
    FWordApp:= CreateOleObject(DEF_REG_WORD_CLASS);
    FConnected:= True;
    FWordApp.Visible:= False; // hides the application
    FWordApp.ScreenUpdating:= False; // speed up winword's processing
    FWordApp.WindowState:= $00000002; // minimise
    FADoc:= FWordApp.Documents.Add(EmptyParam, False); // this will hold the text to be checked
    FRange:= FADoc.Range;
    FRange.WholeStory; // makes FRange point to all text in document
    FCustDics:= FWordApp.CustomDictionaries;
    FWordVersion:= FWordApp.Version;
    s:= FADoc.Name + ' - ' + FWordApp.Name;
    FHandle:= FindWindow(MSWordWndClass, PChar(s)); // winword
    if Pos('10', FWordVersion) = 1 then begin
      s:= MSDialogWndClass2002
    end else begin
      if FWordVersion[1] = '9'
      then s:= MSDialogWndClass2000
      else s:= MSDialogWndClass97;
    end;
    // set up shared memory space
    hMapObject:= CreateFileMapping($FFFFFFFF, nil, PAGE_READWRITE, 0, DEF_SHARE_MEM, DEF_FILEMAPPING_NAME);
    pMem:= MapViewOfFile(hMapObject, FILE_MAP_WRITE, 0, 0, 0);
    if pMem <> nil then begin
      FillChar(pMem^, DEF_SHARE_MEM, 0);
      p:= pMem;
      StrCopy(p + DEF_CLASS_START, PChar(s));
      StrCopy(p + DEF_CAPTION_START, PChar(FSpellCaption));
    end;
    // memory share set up
  except
    FWordApp:= Unassigned;
    FConnected:= False;
    raise Exception.Create(ERR_INIT_MSWORD);
  end;
end;

constructor TMSOSpellChecker.Create(AOwner: TComponent);
var
  init: Integer;
begin
  inherited;
  FConnected:= False;
  FChangedText:= '';
  init:= CoInitialize(nil);
  FHookCaption:= False;
  if (init = S_OK) or (init = S_FALSE)
  then FOleOn:= True
  else raise EOleSysError.CreateFmt(ERR_INIT_COMLIB, [init]);
end;

destructor TMSOSpellChecker.Destroy;
begin
  Disconnect;
  UnSetHook;
  if FOleOn
  then CoUninitialize;
  inherited;
end;

procedure TMSOSpellChecker.Disconnect;
var
  savechanges: OleVariant;
begin
  if not VarIsEmpty(FWordApp) then begin
    savechanges:= False;
    FWordApp.Quit(savechanges); // don't save changes
    FRange:= Unassigned;
    FADoc:= Unassigned;
    FWordApp:= Unassigned;
    FCustDics:= Unassigned;
    FConnected:= False;
    CloseHandle(hMapObject);
  end;
end;

procedure TMSOSpellChecker.RemoveCustomDic(const Index: Integer);
var
  dic: OleVariant;
begin
  dic:= FCustDics.Item(Index);
  if not VarIsEmpty(dic)
  then dic.Delete;
  dic:= Unassigned;
end;

procedure TMSOSpellChecker.RemoveCustomDic(const Name: String);
var
  dic: OleVariant;
begin
  dic:= FCustDics.Item(Name);
  if not VarIsEmpty(dic)
  then dic.Delete;
  dic:= Unassigned;
end;

procedure TMSOSpellChecker.ResetIgnoreAll;
begin
  if FConnected then begin
    FRange.Text:= ''; // ResetIgnoreAll performs an automatic spell check
    FWordApp.ResetIgnoreAll;
  end;
end;

procedure TMSOSpellChecker.DialogSpellingOptions;
begin
  if not FConnected then Connect;
  if not FConnected then Exit; // if still not connected then no MS Word!
  BringWindowToTop(FHandle); // ensures that dialog opens on top
  FWordApp.Dialogs.Item($000000D3).Show;
  FWordApp.Visible:= False;
end;

end.
