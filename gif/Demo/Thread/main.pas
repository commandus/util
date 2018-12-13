unit main;
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Project:	TGIFImage demo application.                                   //
// Description:	Displays a GIF without the TImage component.                  //
// Copyright	(c) 1997-99 Anders Melander.                                  //
//		All rights reserved.                                          //
// Formatting:	2 space indent, 8 space tabs, 80 columns.                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Note: When running this demo on Windows 95/98, the multiple thread demo    //
// may appear to hang when a large number of threads are created and          //
// destroyed. This isn't the case, its just takes a long time for the         //
// threads to shut down.                                                      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
interface

uses
  GIFImage,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TFormMain = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    ButtonSingle: TBitBtn;
    ButtonExit: TBitBtn;
    ButtonMultiple: TBitBtn;
    ImageBackground: TImage;
    ButtonNative: TBitBtn;
    procedure ButtonExitClick(Sender: TObject);
    procedure ButtonMultipleClick(Sender: TObject);
    procedure ButtonSingleClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonNativeClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.DFM}

uses
  Single,
  Multiple,
  Native;

procedure TFormMain.ButtonExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.ButtonSingleClick(Sender: TObject);
begin
  with TFormSingle.Create(self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TFormMain.ButtonMultipleClick(Sender: TObject);
begin
  with TFormMultiple.Create(self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TFormMain.ButtonNativeClick(Sender: TObject);
begin
  with TFormNative.Create(self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  if not(Assigned(ImageBackground.Picture.Graphic)) then
  begin
    ButtonSingle.Enabled := False;
    ButtonMultiple.Enabled := False;
    ShowMessage('No GIF to display.'+#13+
      'ImageBackground is empty!'+#13+#13+
      'Did you forget to install TGIFImage'+#13+
      'before you loaded the form?'+#13+#13+
      'See the release notes for more info.');
  end;
end;

end.
