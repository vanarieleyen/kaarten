unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TplPanelUnit, uETilePanel, Forms, math,
  Controls, Graphics, Dialogs, ExtCtrls, BGRABitmap, BGRABitmapTypes, BGRACanvas2D;

type

  { TForm1 }

  TForm1 = class(TForm)
    table: TplPanel;
    uETilePanel1: TuETilePanel;
    procedure FormCreate(Sender: TObject);
    procedure uETilePanel1Paint(Sender: TObject);
  private
    deck: TBGRABitmap;	// complete deck of cards
    side: integer;			// canvas size for one card
  public
    procedure drawCard(scale: double; x, y, angle, suit, rank: integer);
  end;

var
  Form1: TForm1;

const
  DIAMONDS 	= 1;
  CLUBS 		= 2;
  HEARTS 		= 3;
  SPADES 		= 4;
  CARDHIGHT	= 312;
  CARDWIDTH = 224;
  HALFCARDHIGHT = round(CARDHIGHT/2);
  HALFCARDWIDTH = round(CARDWIDTH/2);

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
	deck := TBGRABitmap.Create('images/deck.png');
  side := round( sqrt(power(CARDWIDTH, 2) + power(CARDHIGHT, 2)));
end;

procedure TForm1.drawCard(scale: double; x, y, angle, suit, rank: integer);
var
  bmp: TBGRABitmap;
  ctx: TBGRACanvas2D;
  cx, cy: integer;
begin
	bmp := TBGRABitmap.Create(side, side, BGRAPixelTransparent);
  ctx := bmp.Canvas2D;
  ctx.scale(scale);
  ctx.translate(round((side-CARDWIDTH)/2)+HALFCARDWIDTH, round((side-CARDHIGHT)/2)+HALFCARDHIGHT);
  ctx.rotate(degtorad(angle));
	case suit of
    DIAMONDS:	cy := 3*352;
		CLUBS: 		cy := 0;
		HEARTS:		cy := 352;
    SPADES:		cy := 2*352;
  end;
  cx := (rank-1)*263+1;
  ctx.drawImage(deck.GetPart(Rect(cx,cy,CARDWIDTH+cx,CARDHIGHT+cy)) as TBGRABitmap, -HALFCARDWIDTH, -HALFCARDHIGHT, CARDWIDTH, CARDHIGHT);
	bmp.Draw(table.Canvas, x, y, false);
  bmp.Free;
end;

procedure TForm1.uETilePanel1Paint(Sender: TObject);
begin
  drawCard(0.7, 0, 0, -20, DIAMONDS, 7);
	drawCard(0.7, 20, 0, -10, SPADES, 13);
  drawCard(0.7, 40, 0, 20, HEARTS, 1);
  drawCard(0.7, 60, 0, 30, CLUBS, 12);
end;



end.

