
int: x;
var int: y;

constraint y > -x;
constraint y < x;

solve satisfy;
