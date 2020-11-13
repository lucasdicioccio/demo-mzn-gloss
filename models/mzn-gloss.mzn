include "globals.mzn";

int: nTowers;
int: nBuildings;
int: coverTarget;
set of int: TOWER = 1..nTowers;
set of int: BUILDING = 1..nBuildings;
array[BUILDING,1..2] of int: buildingPosition;
array[TOWER] of int: range;
array[TOWER] of int: price;

function int: xb(BUILDING: b) = buildingPosition[b,1];
function int: yb(BUILDING: b) = buildingPosition[b,2];

function var int: distance(int: x1,int: y1, var int: x2, var int: y2) =
  abs(x1 - x2) + abs(y1 - y2);

int: maxrange = max(range);
int: minx     = min([xb(b) | b in BUILDING]) - maxrange - 1;
int: miny     = min([yb(b) | b in BUILDING]) - maxrange - 1;
int: maxx     = max([xb(b) | b in BUILDING]);
int: maxy     = max([yb(b) | b in BUILDING]);

array[TOWER,1..2] of var int: towerPosition;
function var int: xt(TOWER: t) = towerPosition[t,1];
function var int: yt(TOWER: t) = towerPosition[t,2];


constraint forall(t in TOWER)
  (  xt(t) >= minx
  /\ xt(t) <= maxx
  /\ yt(t) >= miny
  /\ yt(t) <= maxy
  );

array[TOWER] of var bool: active;
constraint active =
  [ xt(t) > minx /\ yt(t) > miny | t in TOWER ];

function var int: coverage(BUILDING: b) =
  sum(t in TOWER)(distance(xb(b), yb(b), xt(t), yt(t)) <= range[t]);

function var int: service(TOWER: t) =
  sum(b in BUILDING)(distance(xb(b), yb(b), xt(t), yt(t)) <= range[t]);

% THE ONLY MODELING CONSTRAINT
constraint forall(b in BUILDING)(coverage(b) >= coverTarget);

constraint forall(t1, t2 in TOWER where t1 < t2 /\ range[t1] = range[t2])
  (active[t2] -> active[t1] /\ lex_lesseq([xt(t1), yt(t1)], [xt(t1), yt(t2)]));

constraint forall(t in TOWER)
  (active[t] -> service(t) > 0);

var int: total_active;
constraint total_active = sum(active);

var int: total_cost;
constraint total_cost = sum(t in TOWER)(active[t] * price[t]);


solve minimize total_cost;
