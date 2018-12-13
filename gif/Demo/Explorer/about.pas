unit about;
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Project:	TGIFImage demo application.                                   //
// Description:	GIF file viewer.                                              //
// Copyright	(c) 1997-99 Anders Melander.                                  //
//		All rights reserved.                                          //
// Formatting:	2 space indent, 8 space tabs, 80 columns.                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
interface

uses
  GIFImage,
  shellapi, // ShellExecute
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TFormAbout = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    ButtonOK: TButton;
    Label3: TLabel;
    LabelMail: TLabel;
    LabelHomePage: TLabel;
    Panel3: TPanel;
    ImageLogo: TImage;
    ImageCredits: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LabelGotoURL(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

{$IFDEF VER90}
// crHandPoint cursor from Delphi 3.x
{$R CURSORS.RES}
const
  crHandPoint = 1;
{$ENDIF}

procedure TFormAbout.FormCreate(Sender: TObject);
begin
  (ImageCredits.Picture.Graphic as TGIFImage).DrawOptions :=
    (ImageCredits.Picture.Graphic as TGIFImage).DrawOptions + [goDirectDraw];

{$IFDEF VER90}
  // Load crHandPoint cursor
  Screen.Cursors[crHandPoint] := LoadCursor(HInstance, PChar('CRHANDPOINT'));
{$ENDIF}
  LabelMail.Cursor := crHandPoint;
  LabelHomepage.Cursor := crHandPoint;
end;

procedure TFormAbout.FormDestroy(Sender: TObject);
begin
  ImageCredits.Picture.Graphic := nil;
end;

procedure TFormAbout.LabelGotoURL(Sender: TObject);
var
  URL			: string;
begin
  URL := TLabel(Sender).Caption;
  ShellExecute(Application.Handle, nil, PChar(URL), nil, nil, SW_SHOWNORMAL);
end;

end.
