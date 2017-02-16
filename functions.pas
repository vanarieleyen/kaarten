// functions.pas: include file, included into unit1.pas

// swap 2 cards in the hand
procedure TForm1.swapCard(src, dst: integer);
var
  temp: integer;
begin
  South.hand[dst].deal := false;
  South.hand[src].deal := false;
  temp := South.hand[src].suit;
  South.hand[src].suit := South.hand[dst].suit;
  South.hand[dst].suit := temp;
  temp := South.hand[src].rank;
  South.hand[src].rank := South.hand[dst].rank;
  South.hand[dst].rank := temp;
end;

// Fisher-Yates shuffle of the deck of cards
procedure TForm1.fyshuffle(var cards: array of TCard);
var
  m, i: integer;
  temp: TCard;
begin
  Randomize();
  for m := High(cards)-1 downto Low(cards) do begin
    i := Random(m);
    temp := cards[m];
    cards[m] := cards[i];
    cards[i] := temp;
  end;
end;

// compare on ranks - keeps the ranks together
function onRank(var hand: array of TCard; i, p: Integer): integer;
var
  ri, rp: integer;
begin
  // put the ace on top
  ri := hand[i].rank-1;
  rp := hand[p].rank-1;
  if ri = 0 then ri := 13;
  if rp = 0 then rp := 13;

  if (ri > rp) then
    Result := 1
  else if (ri = rp) then
    Result := hand[i].suit - hand[p].suit
  else
    Result := -1;
end;

// compare on suits - keeps the suits together
function onSuit(var hand: array of TCard; i, p: Integer): integer;
var
  t1, t2, ri, rp: integer;
begin
  // put the ace on top
  ri := hand[i].rank-1;
  rp := hand[p].rank-1;
  if ri = 0 then ri := 13;
  if rp = 0 then rp := 13;

  t1 := hand[i].suit*13+ri;
  t2 := hand[p].suit*13+rp;
  Result := t1 - t2;
end;

// sort the hand of cards
procedure TForm1.QuickSort(L, R: Integer; var hand: array of TCard; Compare: TCompare);
var
  I, J, Pivot: Integer;
begin
  repeat
    I := L;
    J := R;
    Pivot := (L + R) shr 1;
    repeat
      while Compare(hand, I, Pivot) < 0 do Inc(I);
      while Compare(hand, J, Pivot) > 0 do Dec(J);
      if I <= J then begin
        swapCard(I, J);
        if Pivot = I then
          Pivot := J
        else if Pivot = J then
          Pivot := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, hand, Compare);
    L := I;
  until I >= R;
end;

// draws the complete hand on the virtual canvas
procedure TForm1.drawHand(player: TPlayer);
var
  i: integer;
begin
  if player.location=1 then
    for i := 1 to length(player.hand)-1 do
      drawCard(player.hand[i], length(player.hand))
  else
    drawBack(player);
end;

// draw the back of the cards
procedure TForm1.drawBack(player: TPlayer);
var
  card, hcard, vcard: TBGRABitmap;
  ctx: TBGRACanvas2D;
  newsize, xpos, ypos, cw, ch, hcw, hch, i: integer;
  pixel: TBGRAPixel;
  bm: TBitmap;
begin
  newsize := ceil(diagonal*scale);      // the diagonal size of the card (to leave room for rotation)
  cw := round(CARDWIDTH*scale);         // scaled card width and height
  ch := round(CARDHEIGHT*scale);
  hcw := round(-HALFCARDWIDTH*scale);   // scaled half card width and height (negative)
  hch := round(-HALFCARDHEIGHT*scale);

  // generate the scaled card
  bm := TBitmap.Create;
  cardlist.GetBitmap(53, bm);
  card := TBGRABitmap.Create(bm, true);
  card.ResampleFilter := rfBestQuality;
  vcard := card.Resample(cw, ch) as TBGRABitmap;
  bm.Free;

  xpos := player.position.x;
  ypos := player.position.y;

  if (player.location mod 2 = 1) then begin   // North
    xpos -= round((length(player.hand)*shift)/2)+round(cw/2);
    //player.layer.FillTransparent;
    for i := Low(player.hand) to High(player.hand)-1 do begin
       player.layer.Canvas2d.drawImage(vcard, xpos, ypos);
       xpos += shift;
    end;
  end else begin      // East and West
    hcard := vcard.RotateCW as TBGRABitmap;
    ypos -= round((length(player.hand)*shift)/2)+cw;
    //player.layer.FillTransparent;
    for i := Low(player.hand) to High(player.hand)-1 do begin
      player.layer.Canvas2d.drawImage(hcard, xpos, ypos);
      ypos += shift;
    end;
    hcard.Free;
  end;

  card.Free;
  vcard.Free;
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



