unit main;
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Project:	TGIFImage demo application.                                   //
// Description:	Display GIF files in an owner-draw combo box.                 //
// Copyright	(c) 1997-99 Anders Melander.                                  //
//		All rights reserved.                                          //
// Formatting:	2 space indent, 8 space tabs, 80 columns.                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Based on an idea by Pete Wason.                                            //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

uses
{$IFDEF DELPHI}
  // C++ Builder has TImageList in controls unit
  ImgList,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TFormMain = class(TForm)
    ImageList: TImageList;
    OpenDialog: TOpenDialog;
    Panel1: TPanel;
    ImageView: TImage;
    Panel2: TPanel;
    ImagePicker: TComboBox;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure ImagePickerDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ImagePickerChange(Sender: TObject);
  private
    { Private declarations }
    Path: string;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.DFM}

uses
  gifimage;

procedure TFormMain.Button1Click(Sender: TObject);
var
  r		: integer;
  DirInfo	: TSearchRec;
begin
  OpenDialog.Filter := GraphicFilter(TGIFImage);
  if (not OpenDialog.Execute) then
    exit;

  Path := ExtractFilePath(OpenDialog.FileName);

  ImageList.Clear;
  ImagePicker.Items.Clear;

  r := SysUtils.FindFirst(Path+'*.gif', FaAnyfile AND NOT(faDirectory OR faVolumeID), DirInfo);
  try
    while (r = 0) do
    begin
      ImagePicker.Items.AddObject(DirInfo.Name, TObject(-1));
      r := SysUtils.FindNext(DirInfo);
    end;
  finally
    SysUtils.FindClose(DirInfo);
  end;
end;

procedure TFormMain.ImagePickerDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  i		: integer;
  s		: string;
  GIF		: TGIFImage;
  bmp		: TBitmap;
begin
  i := integer(ImagePicker.Items.Objects[Index]);

  if (i = -1) then
  begin
    Screen.Cursor := crHourGlass;
    try
      bmp:=TBitmap.Create;
      try
        bmp.Height := ImageList.Height;
        bmp.Width := ImageList.Width;
        GIF:=TGIFimage.Create;
        try
          GIF.LoadFromFile(Path+ImagePicker.Items[Index]);
          bmp.Canvas.StretchDraw(bmp.Canvas.ClipRect, GIF.Bitmap);
          i := ImageList.Add(bmp, nil);
          ImagePicker.Items.Objects[Index] := TObject(i);
        finally
          GIF.Free;
        end;
      finally
        bmp.Free;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;

  TComboBox(Control).Canvas.FillRect(Rect);
  ImageList.Draw(TComboBox(Control).Canvas, Rect.Left, Rect.Top, i);

  Rect.Left := Rect.Left + ImageList.Width + 8;
  s := ImagePicker.Items[Index];
  DrawText(TComboBox(Control).Canvas.Handle, PChar(s), length(s), Rect,
    DT_VCENTER OR DT_SINGLELINE);
end;

procedure TFormMain.ImagePickerChange(Sender: TObject);
begin
  ImageView.Picture := nil;
  ImageView.Update;
  if (ImagePicker.ItemIndex <> -1) then
    ImageView.Picture.LoadFromFile(Path+ImagePicker.Text);
end;

end.
