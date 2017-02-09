unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Math, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, LazLogger, BGRAVirtualScreen, BGRABitmap, BGRABitmapTypes,
  BGRACanvas2D, BGRALayers, FPimage;

type

  { TForm1 }

  THand = record
    id, x, y, suit, rank: integer;
    angle: double;
    bid: boolean;
    layer, mask: TBGRABitmap;
  end;

  TForm1 = class(TForm)
    VirtualScreen: TBGRAVirtualScreen;
    Button1: TButton;
    cardlist: TImageList;
    procedure Button1Click(Sender: TObject);
    procedure VirtualScreenClick(Sender: TObject);
    procedure VirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);

  private
    background: TBGRABitmap;  // the green background
    mask: TBGRABitmap;  // mask of the drawn hand, used to recognize the selected card
    side: integer;      // canvas size for one card that allows maximum rotation without clipping
    bm: TBitmap;
    hand: array of THand;
    layers, masks: TBGRALayeredBitmap;
  public
    procedure drawCard(scale: double; myhand: THand);
    procedure drawHand();
    procedure setBackground();
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
  side := round(sqrt(power(CARDWIDTH, 2) + power(CARDHEIGHT, 2)));
  bm := TBitmap.Create;
  background := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);
  mask := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);
  layers :=TBGRALayeredBitmap.Create(Width, Height);
  masks :=TBGRALayeredBitmap.Create(Width, Height);
  layers.AddOwnedLayer(background);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(mask);
  FreeAndNil(bm);
  FreeAndNil(layers);
  FreeAndNil(masks);
end;

// select a card on the hand and refresh the display
procedure TForm1.VirtualScreenClick(Sender: TObject);
var
  pt: TPoint;
  id: integer;
  pixel: TBGRAPixel;
begin
  masks.Draw(mask,0,0);
  pt := ScreenToClient(Mouse.CursorPos);
  pixel := mask.GetPixel(pt.x, pt.y);
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

    drawCard(0.7, hand[id]);
    VirtualScreen.RedrawBitmap;
  end;
end;

// displays/hides the mask on the form
procedure TForm1.Button1Click(Sender: TObject);
begin
  Button1.Tag := Button1.Tag xor 1;

  VirtualScreen.RedrawBitmap;
end;


// displays the content of the virtual canvas on the form
procedure TForm1.VirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  DebugLn('redraw');

  if Button1.Tag = 1 then begin
    masks.Draw(Bitmap,0,0);         // draw masks of the cards
  end else begin
    layers.Draw(Bitmap,0,0);        // draw cards
  end;

end;

// redraws the background and hand on the form
procedure TForm1.FormResize(Sender: TObject);
var
  i, aantal: integer;
  radius, angle, step: double;
begin
  setBackground();

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
    hand[i].layer := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);
    layers.AddOwnedLayer(hand[i].layer);
    hand[i].mask := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);
    masks.AddOwnedLayer(hand[i].mask);

    layers.MoveLayerDown(layers.NbLayers);
    angle += step;
    //DebugLn(inttostr(hand[i].suit)+' '+inttostr(hand[i].rank));
  end;
  drawHand();
end;

// draws the complete hand on the virtual canvas
procedure TForm1.drawHand();
var
  i: integer;
begin
  mask.FillTransparent;

  for i := 1 to length(hand)-1 do
    drawCard(0.7, hand[i]);
end;

// draws a card on the virtual canvas and sets the mask
// cards and masks are seperate layers
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
  myhand.layer.FillTransparent;
  myhand.layer.Canvas2d.drawImage(bmp, myhand.x, myhand.y, newsize, newsize);

  // draw the mask of the card
  pixel.red := myhand.id shl 4;
  pixel.blue:= myhand.suit shl 4; // not used (only for mask coloring)
  pixel.green:=myhand.rank shl 4; // not used (only for mask coloring)
  pixel.alpha := 255;
  ctx.fillStyle(pixel);
  ctx.roundRect(-HALFCARDWIDTH, -HALFCARDHEIGHT, CARDWIDTH, CARDHEIGHT, 10);
  ctx.fill;
  myhand.mask.FillTransparent;
  myhand.mask.Canvas2d.drawImage(bmp, myhand.x, myhand.y, newsize, newsize);

  FreeAndNil(bmp);
  FreeAndNil(card);
end;

// clears the masks and draws the green background on the form
// the texture is painted on a virtual canvas that is later copied to the form
procedure TForm1.setBackground();
var
  X, Y: integer;
  texture: TBGRABitmap;
begin
  cardlist.GetBitmap(0, bm);
  texture := TBGRABitmap.Create(bm, False);

  for X := 0 to (Width div texture.Width) do begin
    for Y := 0 to (Height div texture.Height) do begin
      background.Canvas2d.drawImage(texture, X * texture.Width, Y * texture.Height);
    end;
  end;
  FreeAndNil(texture);
end;


end.
