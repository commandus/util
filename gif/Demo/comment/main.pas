unit main;
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Project:	TGIFImage demo application.                                   //
// Description:	Display the comment block of GIF files.                       //
// Copyright	(c) 1997-99 Anders Melander.                                  //
//		All rights reserved.                                          //
// Formatting:	2 space indent, 8 space tabs, 80 columns.                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, FileCtrl, Grids, Outline, DirOutln;

type
  TForm1 = class(TForm)
    Panel2: TPanel;
    Panel3: TPanel;
    Panel1: TPanel;
    Panel4: TPanel;
    DirectoryOutline: TDirectoryOutline;
    Panel5: TPanel;
    DriveComboBox: TDriveComboBox;
    Panel6: TPanel;
    FileListBox: TFileListBox;
    Panel7: TPanel;
    MemoComment: TMemo;
    ImageView: TImage;
    procedure FileListBoxChange(Sender: TObject);
    procedure DriveComboBoxChange(Sender: TObject);
    procedure DirectoryOutlineChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  gifimage;

procedure TForm1.FileListBoxChange(Sender: TObject);
var
  i, j			: integer;
begin
  ImageView.Picture := nil;
  MemoComment.Lines.Clear;
  ImageView.Update;

  if (FileListBox.FileName <> '') then
  begin
    // Display GIF
    ImageView.Picture.LoadFromFile(FileListBox.FileName);

    // Scan GIF for first comment extension
    if (ImageView.Picture.Graphic is TGIFImage) then
      with TGIFImage(ImageView.Picture.Graphic) do
      begin
        // Loop through all frames
        for i := 0 to Images.Count-1 do
          // Loop through all extensions
          for j := 0 to Images[i].Extensions.Count-1 do
            // Test for comment extension
            if (Images[i].Extensions[j] is TGIFCommentExtension) then
            begin
              // Copy comment to memo
              MemoComment.Lines.Assign(TGIFCommentExtension(Images[i].Extensions[j]).Text);
              exit;
            end;
      end;
  end;
end;

procedure TForm1.DriveComboBoxChange(Sender: TObject);
begin
  DirectoryOutline.Drive := DriveComboBox.Drive;
end;

procedure TForm1.DirectoryOutlineChange(Sender: TObject);
begin
  FileListBox.Directory := DirectoryOutline.Directory;
end;

end.
