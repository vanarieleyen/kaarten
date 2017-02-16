unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Math, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Menus, LazLogger, TplPanelUnit,
  BGRAVirtualScreen, BGRABitmap, BGRABitmapTypes, BGRACanvas2D, BGRALayers, FPimage;

type

  { TForm1 }

  TCard = packed record
    id, x, y, suit, rank: integer;
    angle: double;
    deal: boolean;
    layer, mask: TBGRABitmap;
  end;

  TPlayer = record
    name: string;                   // name of the player
    hand: array of TCard;           // the cards of the player
    position: TPoint;               // the position where the hand of the player is displayed
  end;

  TCompare = function (var hand: array of TCard; i, pivot: Integer): integer;

  TForm1 = class(TForm)
    Button2: TButton;
    Button3: TButton;
    IdleTimer1: TIdleTimer;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    plPanel1: TplPanel;
    VirtualScreen: TBGRAVirtualScreen;
    Button1: TButton;
    cardlist: TImageList;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure IdleTimer1Timer(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure VirtualScreenMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure VirtualScreenMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure VirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);

  private
    background: TBGRABitmap;  // the green background
    diagonal: integer;      // canvas size for one card that allows maximum rotation without clipping
    deck: array [0..52] of TCard;
    layers, masks: TBGRALayeredBitmap;
    scale: single;      // the scale of the cards
    space: integer;     // the space between overlapping cards
    shift: integer;     // the amount a card shifts out of a deck
    South,West,East,North: TPlayer;  // the players at the table
    oldsize: integer;     // used to detect windowsize changes
    select: integer;      // the clicked card id
  public
    procedure drawCard(mycard: TCard; size: integer);
    procedure drawHand(player: TPlayer);
    procedure setBackground();
    procedure fyshuffle(var cards: array of TCard);
    procedure swapCard(src, dst: integer);
    procedure QuickSort(L, R: Integer; var hand: array of TCard; Compare: TCompare);
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

{$include functions.pas}

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
  layers.Free;
  masks.Free;
end;

// resets positions and scale during a resize of the window
procedure TForm1.FormResize(Sender: TObject);
begin
  scale := min(1, Height/CARDHEIGHT/5);   // the size of the cards to make them fit in the window
  space := round(30*scale);               // the space between each card
  shift := round(40*scale);  // the amount a card shifts out of a deck
  South.position.x := round(Width/2);
  South.position.y := Height - plPanel1.Height - 15 - round(CARDHEIGHT*scale);
  idleTimer1.Enabled := false;  // temporarily disable the idle timer (which handles resizes)
end;

// draws the screen again (only after resize is finished)
procedure TForm1.IdleTimer1Timer(Sender: TObject);
begin
  if Width*Height <> oldsize then begin
    plPanel1.Left := round((Width-plPanel1.Width)/2);
    drawHand(South);
    oldsize := Width*Height;
  end;
end;

// switch screen between mask and cards
procedure TForm1.MenuItem1Click(Sender: TObject);
begin
  MenuItem1.Tag := MenuItem1.Tag xor 1;
  VirtualScreen.RedrawBitmap;
end;

// start dragging a card
procedure TForm1.VirtualScreenMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  mask: TBGRABitmap;
begin
  if (masks.NbLayers > 0) then begin
    VirtualScreen.Cursor := crDrag;
    Application.ProcessMessages;
    mask := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);
    masks.Draw(mask,0,0);
    select := mask.GetPixel(X, Y).red shr 4;
    mask.Free;
    DebugLn('down:'+IntToStr(South.hand[select].suit) + ' ' + IntToStr(South.hand[select].rank));

    if (South.hand[select].suit > 0) and (South.hand[select].rank > 0) then begin
      South.hand[select].deal := not South.hand[select].deal;
      drawCard(South.hand[select], length(South.hand));
      VirtualScreen.RedrawBitmap;
    end;
  end;
end;

// stop drag card and swap both cards
procedure TForm1.VirtualScreenMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  mask: TBGRABitmap;
  id: integer;
begin
  VirtualScreen.Cursor := crDefault;
  if (masks.NbLayers > 0) then begin

    mask := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);
    masks.Draw(mask,0,0);
    id := mask.GetPixel(X, Y).red shr 4;
    mask.Free;
    DebugLn('up:'+IntToStr(South.hand[id].suit) + ' ' + IntToStr(South.hand[id].rank));

    if (id <> select) and (id <> 0) then begin
      DebugLn('swap id''s:'+IntToStr(id) + ' ' + IntToStr(select));
      swapCard(id, select);
      drawCard(South.hand[select], length(South.hand));
      drawCard(South.hand[id], length(South.hand));
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

  for i := Low(deck) to High(deck) do begin
    deck[i].suit := i div 13 +1;
    deck[i].rank := i mod 13 +1;
  end;
  fyshuffle(deck);

  aantal := 13;
  radius := DegToRad( SEGMENT/13*aantal );  // the radius of the cards in the hand
  step := radius / aantal;
  angle := radius / 2 - radius + step / 2;  // the angle at which to show the card
  setlength(South.hand, aantal + 1);
  setlength(West.hand, aantal + 1);
  setlength(North.hand, aantal + 1);
  setlength(East.hand, aantal + 1);
  for i := 1 to aantal do begin
    South.hand[i].id := i;
    South.hand[i].x := i;
    South.hand[i].y := 10;
    South.hand[i].suit := deck[i-1].suit;
    South.hand[i].rank := deck[i-1].rank;
    South.hand[i].angle := angle;
    South.hand[i].deal := False;
    South.hand[i].layer := TBGRABitmap.Create(Screen.Width, Screen.Height, BGRAPixelTransparent);
    layers.AddOwnedLayer(South.hand[i].layer);
    South.hand[i].mask := TBGRABitmap.Create(Screen.Width, Screen.Height, BGRAPixelTransparent);
    masks.AddOwnedLayer(South.hand[i].mask);
    angle += step;

    West.hand[i].suit := deck[i+13-1].suit;
    West.hand[i].rank := deck[i+13-1].rank;

    North.hand[i].suit := deck[i+26-1].suit;
    North.hand[i].rank := deck[i+26-1].rank;

    East.hand[i].suit := deck[i+39-1].suit;
    East.hand[i].rank := deck[i+39-1].rank;
  end;
  Button3.Tag := 0;
  QuickSort(Low(South.hand), High(South.hand), South.hand, @onRank);
  drawHand(South);
end;

// sort the hand on rank or suit
procedure TForm1.Button3Click(Sender: TObject);
begin
  if (Button3.Tag = 0) then
    QuickSort(Low(South.hand), High(South.hand), South.hand, @onSuit)
  else
    QuickSort(Low(South.hand), High(South.hand), South.hand, @onRank);
  Button3.Tag := not Button3.Tag;
  drawHand(South);
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
procedure TForm1.drawHand(player: TPlayer);
var
  i: integer;
begin
  for i := 1 to length(player.hand)-1 do
    drawCard(player.hand[i], length(player.hand));
  VirtualScreen.RedrawBitmap;
end;

// draws a card on the virtual canvas and sets the mask,
// cards and masks are seperate layers
procedure TForm1.drawCard(mycard: TCard; size: integer);
var
  card, newcard, box: TBGRABitmap;
  ctx: TBGRACanvas2D;
  newsize, xpos, ypos, cw, ch, hcw, hch, radius: integer;
  pixel: TBGRAPixel;
  bm: TBitmap;
begin
  newsize := ceil(diagonal*scale);      // the diagonal size of the card (to leave room for rotation)
  cw := round(CARDWIDTH*scale);         // scaled card width and height
  ch := round(CARDHEIGHT*scale);
  hcw := round(-HALFCARDWIDTH*scale);   // scaled half card width and height (negative)
  hch := round(-HALFCARDHEIGHT*scale);
  radius := round(10*scale);            // the radius of the rounded card corners (for drawing the mask)

  // calculate the position where to copy the card into the layer
  xpos := South.position.x-round(newsize+size/2) + mycard.x*space;
  ypos := South.position.y-round(newsize/2) + mycard.y;
  if mycard.deal then begin    // modify the position for cards that are shifted out of the deck
    xpos += round(shift * sin(mycard.angle));
    ypos -= round(shift * cos(mycard.angle));
  end;

  // generate the scaled card
  bm := TBitmap.Create;
  cardlist.GetBitmap((mycard.suit-1)*13 + mycard.rank, bm);
  card := TBGRABitmap.Create(bm, true);
  card.ResampleFilter := rfBestQuality;
  newcard := card.Resample(cw, ch) as TBGRABitmap;
  bm.Free;

  // draw the card in a rotated box
  box := TBGRABitmap.Create(newsize, newsize, BGRAPixelTransparent);
  ctx := box.Canvas2D;
  ctx.antialiasing := true;
  ctx.translate(round(newsize / 2), round(newsize / 2));    // center of the box
  ctx.rotate(mycard.angle);
  ctx.drawImage(newcard, hcw, hch);

  // draw the box with card in a virtual layer
  mycard.layer.FillTransparent;
  mycard.layer.Canvas2d.drawImage(box, xpos, ypos);

  // draw the mask of the card
  pixel.red := mycard.id shl 4;
  pixel.blue:= mycard.suit shl 4; // not used (only for mask coloring)
  pixel.green:=mycard.rank shl 4; // not used (only for mask coloring)
  pixel.alpha := 255;
  ctx.fillStyle(pixel);
  ctx.roundRect(hcw, hch, cw, ch, radius);
  ctx.fill;
  mycard.mask.FillTransparent;
  mycard.mask.Canvas2d.drawImage(box, xpos, ypos);

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

