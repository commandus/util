unit main;
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Project:	TGIFImage demo application.                                   //
// Description:	Optimize GIF files to reduce size.                            //
// Copyright	(c) 1997-99 Anders Melander.                                  //
//		All rights reserved.                                          //
// Formatting:	2 space indent, 8 space tabs, 80 columns.                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
// Note: All optimizations are non-destructive. This means that the visual    //
// appearance of the GIFs are not altered in any way.                         //
// Some GIFs can not be optimized and would in fact grow in size if they were //
// optimized. The application will automatically disable optimization of      //
// these GIFs.                                                                //
////////////////////////////////////////////////////////////////////////////////
interface

uses
  gifimage,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, FileCtrl, Outline, DirOutln, ComCtrls, Grids;

type
  TFormMain = class(TForm)
    Panel2: TPanel;
    Panel3: TPanel;
    DirectoryOutline: TDirectoryOutline;
    Panel5: TPanel;
    DriveComboBox: TDriveComboBox;
    ListViewFiles: TListView;
    Panel4: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Label1: TLabel;
    Image1: TImage;
    ButtonAbort: TButton;
    ButtonTest: TButton;
    ButtonOptimize: TButton;
    CheckBoxTwoPass: TCheckBox;
    LabelSaved: TLabel;
    Label2: TLabel;
    LabelProgress: TLabel;
    ProgressBar: TProgressBar;
    procedure DriveComboBoxChange(Sender: TObject);
    procedure DirectoryOutlineChange(Sender: TObject);
    procedure ButtonTestClick(Sender: TObject);
    procedure ButtonOptimizeClick(Sender: TObject);
    procedure ListViewFilesClick(Sender: TObject);
    procedure ButtonAbortClick(Sender: TObject);
  private
    DoAbort: boolean;
    procedure OnProgress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte; RedrawNow: Boolean; const R: TRect;
      const Msg: string);
    procedure DoOptimize(Save: boolean);
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.DFM}

procedure TFormMain.DriveComboBoxChange(Sender: TObject);
begin
  DirectoryOutline.Drive := DriveComboBox.Drive;
end;

procedure TFormMain.DirectoryOutlineChange(Sender: TObject);
var
  Mask			: string;
  FileInfo		: TSearchRec;
  Item			: TListItem;
begin
  ListViewFiles.Items.Clear;

  Mask := DirectoryOutline.Directory + '\*.' + GraphicExtension(TGIFImage);
  if (FindFirst(Mask, 0, FileInfo) <> 0) then
    exit;

  try
    repeat
      Item := ListViewFiles.Items.Add;
      Item.Caption := FileInfo.Name;
    until (FindNext(FileInfo) <> 0);
  finally
    FindClose(FileInfo);
  end;

end;

procedure TFormMain.DoOptimize(Save: boolean);
var
  i			: integer;
  BestStream		,
  Stream1		,
  Stream2		: TMemoryStream;
  GIF1			,
  GIF2			: TGIFImage;
  BestSize		,
  Size0			,
  Size1			,
  Size2			: integer;
  DoTwoPass		: boolean;
  FirstPassBest		: boolean;
  SavedBytes		: integer;

begin
  Size1 := 0; // To avoid compiler warning
  SavedBytes := 0;
  DoAbort := False;
  ButtonTest.Hide;
  ButtonOptimize.Hide;
  ButtonAbort.Show;
  ButtonAbort.Enabled := True;
  try
    Stream1 := TMemoryStream.Create;
    Stream2 := TMemoryStream.Create;
    try
      GIF1 := TGIFImage.Create;
      GIF2 := TGIFImage.Create;
      try
        GIF1.OnProgress := OnProgress;
        GIF2.OnProgress := OnProgress;
        Screen.Cursor := crHourGlass;
        try
          for i := 0 to ListViewFiles.Items.Count-1 do
            ListViewFiles.Items[i].SubItems.Clear;
          for i := 0 to ListViewFiles.Items.Count-1 do
          begin
            if (DoAbort) then
              Exit;
            Stream1.Clear;
            Stream2.Clear;

            // Load GIF file into memory stream to determine file size...
            // ...and improve load performance
            Stream1.LoadFromFile(DirectoryOutline.Directory + '\' + ListViewFiles.Items[i].Caption);
            Size0 := Stream1.Size;
            ListViewFiles.Items[i].SubItems.Add(IntToStr(Size0));
            ListViewFiles.Items[i].Update;

            // Load GIF image from memory stream
            Stream1.Position := 0;
            try
              GIF2.LoadFromStream(Stream1);
            except
              on E: Exception do
              begin
                ListViewFiles.Items[i].SubItems.Add(E.Message);
                ListViewFiles.Items[i].Update;
                continue;
              end;
            end;
            if (DoAbort) then
              Exit;
            Stream1.Clear;

            // Only do two passes if GIF is animated
            DoTwoPass := CheckBoxTwoPass.Checked and (GIF2.Images.Count > 1);

            // Make a copy of the GIF in case we need it for the second pass
            if (DoTwoPass) then
              GIF1.Assign(GIF2);
            if (DoAbort) then
              Exit;

            // Perform optional pass first
            if (DoTwoPass) then
            begin
              // Optimize without the ooMerge option
              GIF1.Optimize([ooCrop, ooCleanup, ooColorMap, ooReduceColors], rmNone, dmNearest, 8);
              try
                // Save GIF to stream to determine optimized size
                GIF1.SaveToStream(Stream1);
              except
                on E: Exception do
                begin
                  ListViewFiles.Items[i].SubItems.Add(E.Message);
                  ListViewFiles.Items[i].Update;
                  continue;
                end;
              end;
              if (DoAbort) then
                Exit;
              GIF1.Clear;
              Size1 := Stream1.Size;
            end;

            // Second pass - Optimize with ooMerge option
            GIF2.Optimize([ooCrop, ooMerge, ooCleanup, ooColorMap, ooReduceColors], rmNone, dmNearest, 8);
            try
              // Save GIF to stream to determine optimized size
              GIF2.SaveToStream(Stream2);
            except
              on E: Exception do
              begin
                ListViewFiles.Items[i].SubItems.Add(E.Message);
                ListViewFiles.Items[i].Update;
                continue;
              end;
            end;
            if (DoAbort) then
              Exit;
            GIF2.Clear;
            Size2 := Stream2.Size;

            if (DoTwoPass) then
            begin
              // Determine which pass optimized best
              FirstPassBest := (Size1 < Size2);
              if (FirstPassBest) then
              begin
                BestSize := Size1;
                BestStream := Stream1;
              end else
              begin
                BestSize := Size2;
                BestStream := Stream2;
              end;
            end else
            begin
              BestSize := Size2;
              BestStream := Stream2;
            end;

            ListViewFiles.Items[i].SubItems.Add(IntToStr(BestSize));
            ListViewFiles.Items[i].SubItems.Add(IntToStr(MulDiv(Size0-BestSize, 100, Size0))+'%');
            ListViewFiles.Items[i].Update;
            if (BestSize < Size0) then
            begin
              inc(SavedBytes, Size0-BestSize);
              LabelSaved.Caption := IntToStr(SavedBytes)+' bytes';
              if (Save) then
              begin
                BestStream.Position := 0;
                BestStream.SaveToFile(DirectoryOutline.Directory + '\' + ListViewFiles.Items[i].Caption);
              end;
            end;
            Application.ProcessMessages;
          end;
        finally
          Screen.Cursor := crDefault;
        end;
      finally
        GIF2.Free;
        GIF1.Free;
      end;
    finally
      Stream2.Free;
      Stream1.Free;
    end;
  finally
    ButtonAbort.Hide;
    ButtonTest.Show;
    ButtonOptimize.Show;
  end;
end;

procedure TFormMain.OnProgress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if (Stage = psEnding) then
  begin
    ProgressBar.Position := 0;
    LabelProgress.Caption := '';
  end else
  begin
    ProgressBar.Position := PercentDone;
    LabelProgress.Caption := Msg;
    ProgressBar.Update;
    LabelProgress.Update;
    Application.ProcessMessages;
    if (DoAbort) then
      Abort;
  end;
end;

procedure TFormMain.ButtonTestClick(Sender: TObject);
begin
  DoOptimize(False);
end;

procedure TFormMain.ButtonOptimizeClick(Sender: TObject);
begin
  if (MessageDlg('Are you sure that you wish to Optimize the directory?'+#13+
                 'The files will be modified!',
                 mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    DoOptimize(True);
end;

procedure TFormMain.ListViewFilesClick(Sender: TObject);
begin
  if (ListViewFiles.Selected <> nil) then
    Image1.Picture.LoadFromFile(DirectoryOutline.Directory + '\' + ListViewFiles.Selected.Caption)
  else
    Image1.Picture.Assign(nil);
end;

procedure TFormMain.ButtonAbortClick(Sender: TObject);
begin
  ButtonAbort.Enabled := False;
  DoAbort := True;
  Abort;
end;

end.
