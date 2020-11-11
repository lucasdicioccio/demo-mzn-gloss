include "globals.mzn";

int: nTowers;
int: nBuildings;
int: coverTarget;
set of int: TOWER = 1..nTowers;
set of int: BUILDING = 1..nBuildings;
array[BUILDING,1..2] of int: buildingPosition;
array[TOWER] of int: range;
array[TOWER] of int: price;
array[TOWER,1..2] of var int: towerPosition;

function int: xb(BUILDING: b) = buildingPosition[b,1];
function int: yb(BUILDING: b) = buildingPosition[b,2];
function var int: xt(TOWER: t) = towerPosition[t,1];
function var int: yt(TOWER: t) = towerPosition[t,2];

function var int: distance(int: x1,int: y1, var int: x2, var int: y2) =
  abs(x1 - x2) + abs(y1 - y2);

function var int: coverage(BUILDING: b) =
  sum(t in TOWER)(distance(xb(b), yb(b), xt(t), yt(t)) <= range[t]);

constraint forall(b in BUILDING)(coverage(b) >= coverTarget);

int: maxrange = max(range);
constraint forall(t in TOWER)
  (  xt(t) >= min([xb(b) | b in BUILDING]) - maxrange - 1
  /\ xt(t) <= max([xb(b) | b in BUILDING])
  /\ yt(t) >= min([yb(b) | b in BUILDING]) - maxrange - 1
  /\ yt(t) <= max([yb(b) | b in BUILDING])
  );

array[TOWER] of var bool: active;
constraint active =
  [ xt(t) > 0 - maxrange /\ yt(t) > 0 - maxrange | t in TOWER ];

constraint forall(t1, t2 in TOWER where t1 < t2 /\ range[t1] = range[t2])
  (active[t2] -> active[t1] /\ lex_lesseq([xt(t1), yt(t1)], [xt(t1), yt(t2)]));

var int: total_active;
constraint total_active = sum(active);

var int: total_cost;
constraint total_cost = sum(t in TOWER)(active[t] * price[t]);


solve minimize total_cost;
