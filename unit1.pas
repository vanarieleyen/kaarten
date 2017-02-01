unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TplPanelUnit, uETilePanel, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, BGRABitmap, BGRABitmapTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    plPanel1: TplPanel;
    uETilePanel1: TuETilePanel;
    procedure FormCreate(Sender: TObject);
    procedure uETilePanel1Paint(Sender: TObject);
  private
  	image: TBGRABitmap;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure pave(bmp: TBGRABitmap);
  begin
    bmp.Canvas2D.fillStyle ('rgb(130,100,255)');
    bmp.Canvas2D.strokeStyle ('rgb(0,0,255)');
    bmp.Canvas2D.beginPath();
    bmp.Canvas2D.lineWidth:=2;
    bmp.Canvas2D.moveTo(5,5);
    bmp.Canvas2D.lineTo(20,10);
    bmp.Canvas2D.lineTo(55,5);
    bmp.Canvas2D.lineTo(45,18);
    bmp.Canvas2D.lineTo(30,50);
    bmp.Canvas2D.closePath();
    bmp.Canvas2D.stroke();
    bmp.Canvas2D.fill();
  end;

procedure six(bmp: TBGRABitmap);
  var
    i: Integer;
  begin
     bmp.Canvas2D.save();
     for i := 0 to 5 do
     begin
        bmp.Canvas2D.rotate(2*PI/6);
        //pave(bmp);
     end;
     bmp.Canvas2D.restore();
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  image := TBGRABitmap.Create('dice.png');
end;

procedure TForm1.uETilePanel1Paint(Sender: TObject);
var bmp: TBGRABitmap;
begin
	image.Draw(plPanel1.Canvas,0,0,false);
  bmp := TBGRABitmap.Create('dice.png');
	bmp.Canvas2D.translate(80,80);
	six(bmp);
	bmp.Draw(plPanel1.Canvas,0,0,false);
	bmp.Free;
end;

end.

