unit speed;
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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TFormAnimationSpeed = class(TForm)
    LabelSpeed: TLabel;
    EditSpeed: TEdit;
    UpDownSpeed: TUpDown;
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure EditSpeedChange(Sender: TObject);
  private
    procedure SetValue(const Value: integer);
    function GetValue: integer;
    { Private declarations }
  public
    { Public declarations }
    property Value: integer read GetValue write SetValue;
  end;

var
  FormAnimationSpeed: TFormAnimationSpeed;

implementation

{$R *.DFM}

uses
  Main;

procedure TFormAnimationSpeed.FormCreate(Sender: TObject);
begin
  Value := GIFImageDefaultAnimationSpeed;
end;

procedure TFormAnimationSpeed.FormDestroy(Sender: TObject);
begin
  FormAnimationSpeed := nil;
  if (FormExplorer <> nil) then
    FormExplorer.MenuAnimationSpeed100.Click;
end;

procedure TFormAnimationSpeed.SetValue(const Value: integer);
begin
  UpDownSpeed.Position := Value;
end;

function TFormAnimationSpeed.GetValue: integer;
begin
  Result := UpDownSpeed.Position;
end;

procedure TFormAnimationSpeed.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then
  begin
    Key := #0;
    Close;
  end;
end;

procedure TFormAnimationSpeed.EditSpeedChange(Sender: TObject);
begin
  GIFImageDefaultAnimationSpeed := Value;
  if (FormExplorer.ImagePreviewAnimate.Picture.Graphic <> nil) and
    (FormExplorer.ImagePreviewAnimate.Picture.Graphic is TGIFImage) then
      TGIFImage(FormExplorer.ImagePreviewAnimate.Picture.Graphic).AnimationSpeed :=
        GIFImageDefaultAnimationSpeed;
end;

end.
