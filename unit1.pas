unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, math, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, LazLogger, BGRABitmap, BGRABitmapTypes, BGRACanvas2D;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);

  private
    deck: TBGRABitmap;	// complete deck of cards
    virt:	TBGRABitmap;	// virtual screen
    mask: TBGRABitmap;	// mask of the drawn hand, used to recognize the selected card
    side: integer;			// canvas size for one card that allows maximum rotation without clipping
  public
    procedure drawCard(scale: double; x, y, angle, suit, rank: integer);
    procedure drawBackground();
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
  virt := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);
  side := round( sqrt(power(CARDWIDTH, 2) + power(CARDHIGHT, 2)));
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  deck.Free;
  mask.Free;
  virt.Free;
end;

procedure TForm1.FormClick(Sender: TObject);
var
  pt: TPoint;
  suit, rank: integer;
  pixel: TColor;
begin
	pt := ScreenToClient(Mouse.CursorPos);
  pixel := mask.Canvas.Pixels[pt.x, pt.y];
  if (pixel and $FF0000) shr 16 = 0 then begin	// make sure a card is clicked
  	suit := max(0, min(4, round( (pixel and $FF) /10)));
  	rank := max(0, min(13, round( ((pixel and $FF00) shr 8) /10)));
	  DebugLn(inttostr(suit)+' '+inttostr(rank));

    if (suit > 0) and (rank > 0) then begin
  		// drawBackground();
  	  drawCard(0.7, 300, 10, 0, suit, rank);
    end;
  end;

  Canvas.CopyRect(Canvas.ClipRect, virt.Canvas, Canvas.ClipRect);   // display virtual canvas
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Canvas.CopyRect(Canvas.ClipRect, mask.Canvas, Canvas.ClipRect);		// display the mask
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  drawBackground();

  drawCard(0.7, 0, 0, -20, DIAMONDS, 11);
	drawCard(0.7, 20, 0, -10, SPADES, 12);
  //drawCard(0.7, 40, 0, 20, HEARTS, 13);
  //drawCard(0.7, 60, 0, 30, CLUBS, 2);

  Canvas.CopyRect(Canvas.ClipRect, virt.Canvas, Canvas.ClipRect); 	// display virtual canvas
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
  cx := floor((rank-1)*(264-rank/15));	// calculate/compensate  the x-pos within the deck

  // draw the card on the virtual canvas
	bmp := TBGRABitmap.Create(side, side, BGRAPixelTransparent);
  ctx := bmp.Canvas2D;

  ctx.scale(scale);
  ctx.translate(round((side-CARDWIDTH)/2)+HALFCARDWIDTH, round((side-CARDHIGHT)/2)+HALFCARDHIGHT);
  ctx.rotate(degtorad(angle));
	ctx.drawImage(deck.GetPart(Rect(cx-1,cy,CARDWIDTH+cx+2,CARDHIGHT+cy)) as TBGRABitmap, -HALFCARDWIDTH, -HALFCARDHIGHT, CARDWIDTH, CARDHIGHT);
  virt.Canvas2d.drawImage(bmp, x, y);

  // draw the mask of the card
	ctx.fillStyle('rgb('+inttostr(suit*10)+','+inttostr(rank*10)+',0)');
  ctx.roundRect(-HALFCARDWIDTH, -HALFCARDHIGHT, CARDWIDTH, CARDHIGHT, 10);
  ctx.fill;
  mask.Canvas2d.drawImage(bmp, x, y);

  bmp.Free;

end;

procedure TForm1.drawBackground();
var
	X, Y: integer;
  aRect, bRect: TRect;
  tex: TBGRABitmap;
begin
  tex := TBGRABitmap.Create('images/felt.png');

  bRect.Left := 0;
  bRect.Top := 0;
  bRect.Right := tex.Width;
  bRect.Bottom := tex.Height;

  aRect.Left := 0;
  aRect.Top := 0;
  aRect.Right := tex.Width;
  aRect.Bottom := tex.Height;

	for X := 0 to (Width div tex.Width)+1 do begin
    for Y := 0 to (Height div tex.Height)+1 do begin
			virt.Canvas2d.drawImage(tex, x, y);
      aRect.Top := aRect.Bottom;
      aRect.Bottom := aRect.Bottom + tex.Height;
    end;
    aRect.Left := aRect.Right;
    aRect.Right := aRect.Right + tex.Width;
    aRect.Top := 0;
    aRect.Bottom := tex.Height;
  end;
  tex.Free;
end;



end.

