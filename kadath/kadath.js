fullscreen(); // sets global variables w, h

var R3 = Math.sqrt(3) / 2;
var problemNumber = 0;

function load_problem(problemNumber) {
  $.ajax("../qualifiers/problem_" + problemNumber + ".json", {success:function(data) {
    g_board = JSON.parse(data);
    draw_board(g_board);
  }});
}

function draw_hex(scale, x, y, c) {
  // "scale" is the edge length of the hexagon in pixels
  d.save();
  d.translate(2 * scale, 3 * scale);
  d.translate(scale * R3 * (2 * x + (y % 2)), scale * 2 * y * 0.75);
  d.beginPath();
  d.moveTo(0, 0);
  d.lineTo(scale * R3, scale * -1/2);
  d.lineTo(scale * 2 * R3, 0);
  d.lineTo(scale * 2 * R3, scale);
  d.lineTo(scale * R3, scale * 1.5);
  d.lineTo(0, scale);
  d.closePath();
  d.fillStyle = c;
  d.fill();
  d.strokeStyle = "black";
  d.lineJoin = "round";
  d.lineWidth = 0.5;
  d.stroke();
  d.restore();
}

function draw_board(board) {
  d.clearRect(0,0,w,h);
  board.occup =
    _.map(_.range(board.width), function(x) {
      return _.map(_.range(board.height), function(y) {
        return false;
      })
    });
  board.filled.forEach(function(p) {
    board.occup[p.x][p.y] = true;
  });
  var scale = 0.5 * Math.min(w, h) / Math.max(board.width, board.height);
  console.log(board.width, board.height);
  for (var i = 0; i < board.width; i++) {
    for (var j = 0; j < board.height; j++) {
      draw_hex(scale, i,j,board.occup[i][j] ? "#fc0" : "#f7f7f7");
    }
  }
}

load_problem(0);

$(document).on('keydown', function(e) {
  // can use "," and "." keys to move around problems
  console.log(e.keyCode);
  if (e.keyCode == 188) {
    problemNumber--;
    load_problem(problemNumber);
  }
  if (e.keyCode == 190) {
    problemNumber++;
    load_problem(problemNumber);
  }
});
