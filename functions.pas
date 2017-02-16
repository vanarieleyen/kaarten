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



