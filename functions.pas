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

  hand[i].deal := false;
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

  hand[i].deal := false;
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
    for i := 1 to length(player.hand)-1 do begin
      drawCard(player.hand[i], length(player.hand))
    end
  else
    drawBack(player);
end;

// draw the back of the cards
procedure TForm1.drawBack(player: TPlayer);
var
  card, hcard, vcard, box: TBGRABitmap;
  ctx: TBGRACanvas2D;
  newsize, xpos, ypos, cw, ch, hcw, hch, i: integer;
  bm: TBitmap;
  radius, step, angle, schaal: single;
  aantal, ruimte, skip: integer;
begin
  aantal := 13;
  radius := DegToRad( SEGMENT/13*aantal );  // the radius of the cards in the hand
  step := radius / aantal;
  angle := radius / 2 - radius + step / 2;  // the angle at which to show the card

  schaal := scale*0.7;
  ruimte := round(shift*0.5);
  skip := floor((13-length(player.hand))/2);

  newsize := ceil(diagonal*schaal);      // the diagonal size of the card (to leave room for rotation)
  cw := round(CARDWIDTH*schaal);         // scaled card width and height
  ch := round(CARDHEIGHT*schaal);
  hcw := round(-HALFCARDWIDTH*schaal);   // scaled half card width and height (negative)
  hch := round(-HALFCARDHEIGHT*schaal);

  // generate the scaled card
  bm := TBitmap.Create;
  cardlist.GetBitmap(53, bm);
  card := TBGRABitmap.Create(bm, true);
  card.ResampleFilter := rfLinear; //rfBox; //rfBestQuality;
  vcard := card.Resample(cw, ch) as TBGRABitmap;
  hcard := vcard.RotateCW as TBGRABitmap;

  xpos := player.position.x;
  ypos := player.position.y;

  player.layer.FillTransparent;

  // draw the card in a rotated box
  box := TBGRABitmap.Create(newsize, newsize, BGRAPixelTransparent);
  ctx := box.Canvas2D;
  ctx.antialiasing := false;
  ctx.translate(round(newsize / 2), round(newsize / 2));    // center of the box
  ctx.rotate(angle);
  while skip > 0 do begin
    skip -= 1;
    ctx.rotate(step);
  end;

  case player.location of
    2: begin  // West
        ypos -= round((length(player.hand)*ruimte)/2)+cw;
        for i := Low(player.hand) to High(player.hand)-1 do begin
          ctx.drawImage(hcard, hch, hcw);
          player.layer.Canvas2d.drawImage(box, xpos, ypos);
          ctx.rotate(step);
          box.FillTransparent;
          ypos += ruimte;
        end;
      end;
    3: begin // North
        xpos += round(length(player.hand)/2)+hcw;
        for i := Low(player.hand) to High(player.hand)-1 do begin
          ctx.drawImage(vcard, hcw, hch);
          player.layer.Canvas2d.drawImage(box, xpos, ypos);
          ctx.rotate(step);
          box.FillTransparent;
          xpos -= ruimte;
        end;
      end;
    4: begin // East
        ypos += round((length(player.hand)*ruimte)/2)-cw-round(cw/2);
        for i := Low(player.hand) to High(player.hand)-1 do begin
          ctx.drawImage(hcard, hch, hcw);
          player.layer.Canvas2d.drawImage(box, xpos, ypos);
          ctx.rotate(step);
          box.FillTransparent;
          ypos -= ruimte;
        end;
      end;
  end;
  box.Free;
  bm.Free;
  card.Free;
  vcard.Free;
  hcard.Free;
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
  card.ResampleFilter := rfLinear; //rfBox; //rfBestQuality;
  newcard := card.Resample(cw, ch) as TBGRABitmap;

  // draw the card in a rotated box
  box := TBGRABitmap.Create(newsize, newsize, BGRAPixelTransparent);
  ctx := box.Canvas2D;
  ctx.antialiasing := false;
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

  bm.Free;
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



