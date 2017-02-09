unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Math, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, LazLogger, BGRAVirtualScreen, BGRABitmap, BGRABitmapTypes,
  BGRACanvas2D, FPimage, BCTypes;

type

  { TForm1 }

  THand = record
    id, x, y, suit, rank: integer;
    angle: double;
    bid: boolean;
  end;

  TForm1 = class(TForm)
    VirtualScreen: TBGRAVirtualScreen;
    Button1: TButton;
    cardlist: TImageList;
    procedure VirtualScreenClick(Sender: TObject);
    procedure VirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);

  private
    deck: TBGRABitmap;  // complete deck_png of cards
    virt: TBGRABitmap;  // virtual screen
    mask: TBGRABitmap;  // mask of the drawn hand, used to recognize the selected card
    side: integer;      // canvas size for one card that allows maximum rotation without clipping
    bm: TBitmap;
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
  CARDWIDTH = 223;
  HALFCARDHEIGHT = round(CARDHEIGHT / 2);
  HALFCARDWIDTH = round(CARDWIDTH / 2);
  SEGMENT = 80;    // hand is displayed in a circle of 40 degrees

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  bm := TBitmap.Create;
  side := round(sqrt(power(CARDWIDTH, 2) + power(CARDHEIGHT, 2)));
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(deck);
  FreeAndNil(mask);
  FreeAndNil(virt);
  FreeAndNil(bm);
end;

// select a card on the hand and refresh the display
procedure TForm1.VirtualScreenClick(Sender: TObject);
var
  pt: TPoint;
  id: integer;
  pixel: TBGRAPixel;
begin
  pt := ScreenToClient(Mouse.CursorPos);
  pixel := Mask.GetPixel(pt.x, pt.y);
  id := pixel.red shr 4;
  DebugLn(IntToStr(hand[id].suit) + ' ' + IntToStr(hand[id].rank));

  if (hand[id].suit > 0) and (hand[id].rank > 0) then begin
    if hand[id].bid = True then begin
      hand[id].x -= round(40 * sin(hand[id].angle));
      hand[id].y += round(40 * cos(hand[id].angle));
    end else begin
      hand[id].x += round(40 * sin(hand[id].angle));
      hand[id].y -= round(40 * cos(hand[id].angle));
    end;
    hand[id].bid := not hand[id].bid;

    drawBackground();
    drawHand();
    VirtualScreen.RedrawBitmap;
  end;
end;

// displays the mask on the form
procedure TForm1.Button1Click(Sender: TObject);
begin
  {$IFDEF Windows}
    virt.GetImageFromCanvas(mask.Canvas,0,0);
  {$ELSE}
    virt.Assign(mask);
  {$ENDIF}
  VirtualScreen.RedrawBitmap;
end;

// displays the content of the virtual canvas on the form
procedure TForm1.VirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  DebugLn('redraw');
  //Bitmap.PutImage(0,0,virt,dmSet,150);
  Bitmap.Assign(virt.Bitmap);
end;

// redraws the background and hand on the form
procedure TForm1.FormResize(Sender: TObject);
var
  i, aantal: integer;
  radius, angle, step: double;
begin
  drawBackground();

  aantal := 12;
  radius := DegToRad( SEGMENT/12*aantal );
  step := radius / aantal;
  angle := radius / 2 - radius + step / 2;
  setlength(hand, aantal + 1);
  for i := 1 to aantal do
  begin
    hand[i].id := i;
    hand[i].x := i * 15;
    hand[i].y := 100;
    hand[i].suit := random(4) + 1;
    hand[i].rank := random(13) + 1;
    hand[i].angle := angle;
    hand[i].bid := False;
    angle += step;
    DebugLn(inttostr(hand[i].suit)+' '+inttostr(hand[i].rank));
  end;
  drawHand();
end;

// draws the complete hand on the virtual canvas
procedure TForm1.drawHand();
var
  i: integer;
begin
  for i := 1 to length(hand)-1 do
    drawCard(0.7, hand[i]);
end;

// draws a card on the virtual canvas
// draws a mask on the mask canvas (used to determine the clicked card)
procedure TForm1.drawCard(scale: double; myhand: THand);
var
  card, bmp: TBGRABitmap;
  ctx: TBGRACanvas2D;
  newsize: integer;
  pixel: TBGRAPixel;
begin
  newsize := round(side*scale);

  cardlist.GetBitmap(myhand.suit*myhand.rank, bm);
  card := TBGRABitmap.Create(bm, False);

  // draw the card on the virtual canvas
  bmp := TBGRABitmap.Create(side, side, BGRAPixelTransparent);
  ctx := bmp.Canvas2D;
  ctx.antialiasing := true;
  ctx.translate(round(side / 2), round(side / 2));
  ctx.rotate(myhand.angle);
  ctx.drawImage(card, -HALFCARDWIDTH, -HALFCARDHEIGHT);
  virt.Canvas2d.drawImage(bmp, myhand.x, myhand.y, newsize, newsize);

  // draw the mask of the card
  pixel.red := myhand.id shl 4;
  pixel.blue:= myhand.suit shl 4; // not used (only for mask coloring)
  pixel.green:=myhand.rank shl 4; // not used (only for mask coloring)
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
begin
  if Assigned(mask) then
    FreeAndNil(mask);
  if Assigned(virt) then
    FreeAndNil(virt);
  mask := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);
  virt := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);

  cardlist.GetBitmap(0, bm);
  texture := TBGRABitmap.Create(bm, False);

  for X := 0 to (Width div texture.Width) do begin
    for Y := 0 to (Height div texture.Height) do begin
      //Canvas.Draw(X * texture.Width, Y * texture.Height, texture.Bitmap);
      virt.Canvas2d.drawImage(texture, X * texture.Width, Y * texture.Height);
    end;
  end;
  FreeAndNil(texture);

  Mask.FillRect(0, 0, Width, Height, BGRAPixelTransparent, dmSet);
end;


end.
