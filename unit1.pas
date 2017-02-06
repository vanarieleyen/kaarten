unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, math, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, LazLogger, BGRABitmap, BGRABitmapTypes,
  BGRACanvas2D;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);

  private
    deck: TBGRABitmap;	// complete deck of cards
    mask: TBGRABitmap;	// mask of the drawn hand, used to recognize the selected card
    side: integer;			// canvas size for one card that allows maximum rotation without clipping
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
  mask := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);
  side := round( sqrt(power(CARDWIDTH, 2) + power(CARDHIGHT, 2)));
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  deck.Free;
  mask.Free;
end;

procedure TForm1.FormClick(Sender: TObject);
var
  pt: TPoint;
  suit, rank: integer;
begin
	pt := ScreenToClient(Mouse.CursorPos);

  //DebugLn(inttostr(pt.x)+':'+inttostr(pt.y));
  suit := min(4, mask.Canvas.Pixels[pt.x, pt.y] mod 256);
  rank := min(13, mask.Canvas.Pixels[pt.x, pt.y] div 256);
  DebugLn(inttostr(suit)+' '+inttostr(rank));

  drawCard(0.7, 300, 10, 0, suit, rank);
end;

procedure TForm1.FormPaint(Sender: TObject);
var
	X, Y: integer;
  png : TPortableNetworkGraphic;
  aRect, bRect: TRect;
begin
  png := TPortableNetworkGraphic.Create;
  png.LoadFromFile('images/felt.png');

  bRect.Left := 0;
  bRect.Top := 0;
  bRect.Right := png.Width;
  bRect.Bottom := png.Height;

  aRect.Left := 0;
  aRect.Top := 0;
  aRect.Right := png.Width;
  aRect.Bottom := png.Height;

	for X := 0 to (Width div png.Width) do begin
    for Y := 0 to (Height div png.Height) do begin
       Canvas.CopyRect(aRect, png.Canvas, bRect);
       aRect.Top := aRect.Bottom;
       aRect.Bottom := aRect.Bottom + png.Height;
    end;
    aRect.Left := aRect.Right;
    aRect.Right := aRect.Right + png.Width;
    aRect.Top := 0;
    aRect.Bottom := png.Height;
  end;
  png.Free;

  drawCard(0.7, 0, 0, -20, DIAMONDS, 7);
	drawCard(0.7, 20, 0, -10, SPADES, 13);
  drawCard(0.7, 40, 0, 20, HEARTS, 1);
  drawCard(0.7, 60, 0, 30, CLUBS, 12);
end;


procedure TForm1.drawCard(scale: double; x, y, angle, suit, rank: integer);
var
  bmp: TBGRABitmap;
  ctx: TBGRACanvas2D;
  cx, cy: integer;
begin
	case suit of
    DIAMONDS:	cy := 3*352;
		CLUBS: 		cy := 0;
		HEARTS:		cy := 352;
    SPADES:		cy := 2*352;
  end;
  cx := (rank-1)*263+1;

	bmp := TBGRABitmap.Create(side, side, BGRAPixelTransparent);
  ctx := bmp.Canvas2D;
  ctx.scale(scale);
  ctx.translate(round((side-CARDWIDTH)/2)+HALFCARDWIDTH, round((side-CARDHIGHT)/2)+HALFCARDHIGHT);
  ctx.rotate(degtorad(angle));
  ctx.drawImage(deck.GetPart(Rect(cx-1,cy,CARDWIDTH+cx+2,CARDHIGHT+cy)) as TBGRABitmap, -HALFCARDWIDTH, -HALFCARDHIGHT, CARDWIDTH, CARDHIGHT);
  bmp.Draw(Canvas, x, y, false);

	ctx.fillStyle('rgb('+inttostr(suit)+','+inttostr(rank)+',0)');
  ctx.roundRect(-HALFCARDWIDTH, -HALFCARDHIGHT, CARDWIDTH, CARDHIGHT, 10);
  ctx.fill;
  bmp.Draw(mask.Canvas, x, y, false);

  bmp.Free;
end;





end.

