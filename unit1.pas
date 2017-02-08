unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Math, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, LazLogger, BGRABitmap, BGRABitmapTypes,
  BGRACanvas2D, FPimage;

type

  { TForm1 }

  THand = record
    id, x, y, suit, rank: integer;
    angle: double;
    bid: boolean;
  end;

  TForm1 = class(TForm)
    Button1: TButton;
    deck_png: TImageList;
    felt_png: TImageList;
    procedure Button1Click(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);

  private
    deck: TBGRABitmap;  // complete deck_png of cards
    virt: TBGRABitmap;  // virtual screen
    mask: TBGRABitmap;  // mask of the drawn hand, used to recognize the selected card
    side: integer;      // canvas size for one card that allows maximum rotation without clipping
    hand: array of THand;
  public
    procedure drawCard(scale: double; myhand: THand);
    procedure drawHand();
    procedure drawBackground();
  end;

var
  Form1: TForm1;

const
  DIAMONDS = 1;
  CLUBS = 2;
  HEARTS = 3;
  SPADES = 4;
  CARDHEIGHT = 312;
  CARDWIDTH = 224;
  HALFCARDHEIGHT = round(CARDHEIGHT / 2);
  HALFCARDWIDTH = round(CARDWIDTH / 2);
  SEGMENT = 40;    // hand is displayed in a circle of 40 degrees

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  bm: TBitmap;
begin
  bm := TBitmap.Create;
  deck_png.GetBitmap(0, bm);
  deck := TBGRABitmap.Create(bm);
  side := round(sqrt(power(CARDWIDTH, 2) + power(CARDHEIGHT, 2)));
  FreeAndNil(bm);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(deck);
  FreeAndNil(mask);
  FreeAndNil(virt);
end;

// select a card on the hand and refresh the display
procedure TForm1.FormClick(Sender: TObject);
var
  pt: TPoint;
  id: integer;
  pixel: TBGRAPixel;
begin
  pt := ScreenToClient(Mouse.CursorPos);
  pixel := Mask.GetPixel(pt.x, pt.y);
  id := pixel.red;
  DebugLn(IntToStr(hand[id].suit) + ' ' + IntToStr(hand[id].rank));

  if (hand[id].suit > 0) and (hand[id].rank > 0) then
  begin
    if hand[id].bid = True then
    begin
      hand[id].x -= round(40 * sin(hand[id].angle));
      hand[id].y += round(40 * cos(hand[id].angle));
    end
    else
    begin
      hand[id].x += round(40 * sin(hand[id].angle));
      hand[id].y -= round(40 * cos(hand[id].angle));
    end;
    hand[id].bid := not hand[id].bid;

    drawBackground();
    drawHand();
    Refresh;
  end;
end;

// displays the mask on the form
procedure TForm1.Button1Click(Sender: TObject);
begin
  Canvas.CopyRect(Canvas.ClipRect, Mask.Canvas, Canvas.ClipRect);
end;

// displays the content of the virtual canvas on the form
procedure TForm1.FormPaint(Sender: TObject);
begin
  Canvas.CopyRect(Canvas.ClipRect, virt.Canvas, Canvas.ClipRect);
  //DebugLn('repaint');
end;

// redraws the background and hand on the form
procedure TForm1.FormResize(Sender: TObject);
var
  i, aantal: integer;
  radius, angle, step: double;
begin
  drawBackground();

  radius := DegToRad(SEGMENT);
  aantal := 12;
  step := radius / aantal;
  angle := radius / 2 - radius + step / 2;
  setlength(hand, aantal + 1);
  for i := 1 to aantal do
  begin
    hand[i].id := i;
    hand[i].x := i * 20;
    hand[i].y := 100;
    hand[i].suit := random(4) + 1;
    hand[i].rank := random(13) + 1;
    hand[i].angle := angle;
    hand[i].bid := False;
    angle += step;
  end;
  drawHand();
end;

// draws the complete hand on the virtual canvas
procedure TForm1.drawHand();
var
  i: integer;
begin
  for i := 1 to length(hand) do
    drawCard(0.7, hand[i]);
end;

// draws a card on the virtual canvas
// draws a mask on the mask canvas (used to determine the clicked card)
// the cards are all in one large bitmap, this function copies one of those cards
procedure TForm1.drawCard(scale: double; myhand: THand);
var
  card, bmp: TBGRABitmap;
  ctx: TBGRACanvas2D;
  cx, cy, newsize: integer;
  pixel: TBGRAPixel;
begin
  // get the position of the card in the deck_png
  case myhand.suit of
    DIAMONDS: cy := 3 * 352;
    CLUBS: cy := 0;
    HEARTS: cy := 352;
    SPADES: cy := 2 * 352;
  end;
  cx := (myhand.rank - 1) * 263;
  if myhand.rank=1 then cx += 1;

  newsize := round(side*scale);

  // draw the card on the virtual canvas
  card := deck.GetPart(Rect(cx - 1, cy, CARDWIDTH + cx + 2, CARDHEIGHT + cy)) as TBGRABitmap;
  bmp := TBGRABitmap.Create(side, side, BGRAPixelTransparent);
  ctx := bmp.Canvas2D;
  ctx.antialiasing := true;
  ctx.translate(round(side / 2), round(side / 2));
  ctx.rotate(myhand.angle);
  ctx.drawImage(card, -HALFCARDWIDTH, -HALFCARDHEIGHT);
  virt.Canvas2d.drawImage(bmp, myhand.x, myhand.y, newsize, newsize);

  // draw the mask of the card
  pixel.red := myhand.id;
  pixel.alpha := 255;
  ctx.fillStyle(pixel);
  ctx.roundRect(-HALFCARDWIDTH, -HALFCARDHEIGHT, CARDWIDTH, CARDHEIGHT, 10);
  ctx.fill;
  Mask.Canvas2d.drawImage(bmp, myhand.x, myhand.y, newsize, newsize);

  FreeAndNil(bmp);
  FreeAndNil(card);
end;

// clears the masks and draws the green background on the form
// the texture is painted on a virtual canvas that is later copied to the form
procedure TForm1.drawBackground();
var
  X, Y: integer;
  texture: TBGRABitmap;
  bm: TBitmap;
begin
  if Assigned(mask) then
    FreeAndNil(mask);
  if Assigned(virt) then
    FreeAndNil(virt);
  mask := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);
  virt := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);

  bm := TBitmap.Create;
  felt_png.GetBitmap(0, bm);
  texture := TBGRABitmap.Create(bm, False);

  for X := 0 to (Width div texture.Width) do
  begin
    for Y := 0 to (Height div texture.Height) do
    begin
      virt.Canvas2d.drawImage(texture, X * texture.Width, Y * texture.Height);
    end;
  end;
  FreeAndNil(texture);
  FreeAndNil(bm);

  Mask.FillRect(0, 0, Width, Height, BGRAPixelTransparent, dmSet);
end;


end.
