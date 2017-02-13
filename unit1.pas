unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Math, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Menus, LazLogger, BGRAVirtualScreen, BGRABitmap, BGRABitmapTypes, BGRACanvas2D,
  BGRALayers, FPimage;

type

  { TForm1 }

  THand = record
    id, x, y, suit, rank: integer;
    angle: double;
    bid: boolean;
    layer, mask: TBGRABitmap;
  end;

  TForm1 = class(TForm)
    IdleTimer1: TIdleTimer;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    VirtualScreen: TBGRAVirtualScreen;
    Button1: TButton;
    cardlist: TImageList;
    procedure Button1Click(Sender: TObject);
    procedure IdleTimer1Timer(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure VirtualScreenClick(Sender: TObject);
    procedure VirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);

  private
    background: TBGRABitmap;  // the green background
    diagonal: integer;      // canvas size for one card that allows maximum rotation without clipping
    hand: array of THand;
    layers, masks: TBGRALayeredBitmap;
    scale: single;      // the scale of the cards
    space: integer;     // the space between overlapping cards
    South,West,East,North: TPoint;    // the center-points where the cards of each player are displayed
    oldsize: integer;     // used to detect windowsize changes
  public
    procedure drawCard(myhand: THand);
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
  SEGMENT = 90;    // hand is displayed in a circle of 90 degrees (max)

implementation

{$R *.lfm}

{ TForm1 }

 procedure TForm1.FormCreate(Sender: TObject);
begin
  diagonal := Ceil(sqrt(power(CARDWIDTH, 2) + power(CARDHEIGHT, 2)));
  background := TBGRABitmap.Create(Screen.Width, Screen.Height, BGRAPixelTransparent);
  setBackground();

  layers :=TBGRALayeredBitmap.Create(Screen.Width, Screen.Height);
  masks :=TBGRALayeredBitmap.Create(Screen.Width, Screen.Height);
  layers.AddOwnedLayer(background);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(layers);
  FreeAndNil(masks);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  idleTimer1.Enabled := false;  // temporarily disable the idle timer (which handles resizes)
end;

// reset positions and scale after a resize of the window
procedure TForm1.IdleTimer1Timer(Sender: TObject);
begin
  if Width*Height <> oldsize then begin
    scale := min(1, Height/CARDHEIGHT/5);   // the size of the cards to make them fit in the window
    space := round(30*scale);
    South.x := round(Width/2);
    South.y := Height - round(CARDHEIGHT*scale);
    drawHand();
    oldsize := Width*Height;
  end;
end;

procedure TForm1.MenuItem1Click(Sender: TObject);
begin
  MenuItem1.Tag := MenuItem1.Tag xor 1;
  VirtualScreen.RedrawBitmap;
end;

// select a card on the hand and refresh the display
procedure TForm1.VirtualScreenClick(Sender: TObject);
var
  pt: TPoint;
  id, shift: integer;
  pixel: TBGRAPixel;
  mask: TBGRABitmap;
begin
  if (masks.NbLayers > 0) then begin
    mask := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);
    masks.Draw(mask,0,0);

    pt := ScreenToClient(Mouse.CursorPos);
    pixel := mask.GetPixel(pt.x, pt.y);
    id := pixel.red shr 4;
    mask.Free;
    DebugLn(IntToStr(hand[id].suit) + ' ' + IntToStr(hand[id].rank));

    shift := round(40*scale);  // the amount a card shifts out of a deck
    if (hand[id].suit > 0) and (hand[id].rank > 0) then begin
      if hand[id].bid = True then begin
        hand[id].x -= round(shift * sin(hand[id].angle));
        hand[id].y += round(shift * cos(hand[id].angle));
      end else begin
        hand[id].x += round(shift * sin(hand[id].angle));
        hand[id].y -= round(shift * cos(hand[id].angle));
      end;
      hand[id].bid := not hand[id].bid;

      drawCard(hand[id]);
      VirtualScreen.RedrawBitmap;
    end;
  end;
end;

// shuffle
procedure TForm1.Button1Click(Sender: TObject);
var
  i, aantal: integer;
  radius, angle, step: double;
begin
  while masks.NbLayers > 0 do begin
    masks.RemoveLayer(0);     // remove all masks
    layers.RemoveLayer(1);    // remove all cards and skip the first (background) layer
  end;

  aantal := 12;
  radius := DegToRad( SEGMENT/12*aantal );
  step := radius / aantal;
  angle := radius / 2 - radius + step / 2;
  setlength(hand, aantal + 1);
  for i := 1 to aantal do begin
    hand[i].id := i;
    hand[i].x := i*space;
    hand[i].y := 10;
    hand[i].suit := random(4) + 1;
    hand[i].rank := random(13) + 1;
    hand[i].angle := angle;
    hand[i].bid := False;
    hand[i].layer := TBGRABitmap.Create(Screen.Width, Screen.Height, BGRAPixelTransparent);
    layers.AddOwnedLayer(hand[i].layer);
    hand[i].mask := TBGRABitmap.Create(Screen.Width, Screen.Height, BGRAPixelTransparent);
    masks.AddOwnedLayer(hand[i].mask);
    angle += step;
  end;
  drawHand();
end;

// displays the content of the virtual canvas on the form
procedure TForm1.VirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  if MenuItem1.Tag = 1 then begin
    masks.Draw(Bitmap,0,0);         // draw masks of the cards
  end else begin
    layers.Draw(Bitmap,0,0);        // draw cards
  end;
end;

// draws the complete hand on the virtual canvas
procedure TForm1.drawHand();
var
  i: integer;
begin
  for i := 1 to length(hand)-1 do
    drawCard(hand[i]);
  VirtualScreen.RedrawBitmap;
end;

// draws a card on the virtual canvas and sets the mask,
// cards and masks are seperate layers
procedure TForm1.drawCard(myhand: THand);
var
  card, newcard, box: TBGRABitmap;
  ctx: TBGRACanvas2D;
  newsize, xpos, ypos: integer;
  pixel: TBGRAPixel;
  bm: TBitmap;
begin
  newsize := round(diagonal*scale);
  // calculate the position where to copy the card into the layer
  xpos := South.x-round(newsize+length(hand)/2) + myhand.x;
  ypos := South.y-round(newsize/2) + myhand.y;

  // generate the scaled card bitmap
  bm := TBitmap.Create;
  cardlist.GetBitmap((myhand.suit-1)*13 + myhand.rank, bm);
  card := TBGRABitmap.Create(bm, true);
  card.ResampleFilter := rfBestQuality;
  newcard := card.Resample(round(CARDWIDTH*scale), round(CARDHEIGHT*scale)) as TBGRABitmap;
  bm.Free;

  // draw the card in a rotated box
  box := TBGRABitmap.Create(newsize, newsize, BGRAPixelTransparent);
  ctx := box.Canvas2D;
  ctx.antialiasing := true;
  ctx.translate(round(newsize / 2), round(newsize / 2));
  ctx.rotate(myhand.angle);
  ctx.drawImage(newcard, -HALFCARDWIDTH*scale, -HALFCARDHEIGHT*scale);

  // draw the box with card in a virtual layer
  myhand.layer.FillTransparent;
  myhand.layer.Canvas2d.drawImage(box, xpos, ypos);

  // draw the mask of the card
  pixel.red := myhand.id shl 4;
  pixel.blue:= myhand.suit shl 4; // not used (only for mask coloring)
  pixel.green:=myhand.rank shl 4; // not used (only for mask coloring)
  pixel.alpha := 255;
  ctx.fillStyle(pixel);
  ctx.roundRect(-HALFCARDWIDTH*scale, -HALFCARDHEIGHT*scale, CARDWIDTH*scale, CARDHEIGHT*scale, 10*scale);
  ctx.fill;
  myhand.mask.FillTransparent;
  myhand.mask.Canvas2d.drawImage(box, xpos, ypos);

  card.Free;
  newcard.Free;
  box.Free;
end;

// clears the masks and draws the green background on the form
// the texture is painted on a virtual canvas that is later copied to the form
procedure TForm1.setBackground();
var
  X, Y: integer;
  texture: TBGRABitmap;
  bm: TBitmap;
begin
  bm := TBitmap.Create;
  cardlist.GetBitmap(0, bm);
  texture := TBGRABitmap.Create(bm, False);
  bm.Free;

  for X := 0 to (Screen.Width div texture.Width) do begin
    for Y := 0 to (Screen.Height div texture.Height) do begin
      background.Canvas2d.drawImage(texture, X * texture.Width, Y * texture.Height);
    end;
  end;
  FreeAndNil(texture);
end;


end.

