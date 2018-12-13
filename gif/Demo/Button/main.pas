unit main;
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Project:	TGIFImage demo application.                                   //
// Description:	TBitBtn like component that displays GIFs instead of BMPs.    //
// Copyright	(c) 1997-99 Anders Melander.                                  //
//		All rights reserved.                                          //
// Formatting:	2 space indent, 8 space tabs, 80 columns.                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// This demo requires that you have TGIFImage and the TGIFButton component    //
// installed.                                                                 //
//                                                                            //
// The TGIFButton component is implemented in the gifbutton.pas file located  //
// in the same directory as this file.                                        //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, GIFButton, ExtCtrls;

type
  TFormMain = class(TForm)
    Panel1: TPanel;
    GIFButton1: TGIFButton;
    GIFButton2: TGIFButton;
    Label1: TLabel;
    procedure GIFButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.DFM}

procedure TFormMain.GIFButton1Click(Sender: TObject);
begin
  GIFButton2.Enabled := not(GIFButton2.Enabled); 
end;

end.
