unit main;
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Project:	TGIFImage demo application.                                   //
// Description:	Loads a GIF from a resource and displays it.                  //
// Copyright	(c) 1997-99 Anders Melander.                                  //
//		All rights reserved.                                          //
// Formatting:	2 space indent, 8 space tabs, 80 columns.                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Use the batch file "build_resource.bat" to generate the GIF.RES resource   //
// file used by this application.                                             //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TFormDemo = class(TForm)
    ImageGIF: TImage;
    ButtonLoad: TButton;
    procedure ButtonLoadClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormDemo: TFormDemo;

implementation

{$R *.DFM}

// Include the resource that contains our GIF
{$R GIF.RES}

uses
  GIFimage;

procedure TFormDemo.ButtonLoadClick(Sender: TObject);
var
  Stream		: TStream;
  GIF			: TGIFImage;
begin
  // Do not use buffering.
  // This is safe since we have complete control over the TImage's canvas
  include(GIFImageDefaultDrawOptions, goDirectDraw);

  // Create a stream to load the GIF resource from
  Stream := TResourceStream.Create(hInstance, 'download', 'GIF');
  try
    GIF := TGIFImage.Create;
    try
      // Load the GIF from the resource stream
      GIF.LoadFromStream(Stream);
      // Display the GIF in the TImage
      ImageGIF.Picture.Assign(GIF);
    finally
      GIF.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TFormDemo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Since we are using the goDirectDraw options, we MUST stop the GIF paint
  // thread before the form is destroyed.
  ImageGIF.Picture.Graphic := nil;
end;

end.
