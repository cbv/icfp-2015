fullscreen(); // sets global variables w, h

var R3 = Math.sqrt(3) / 2;
var problemNumber = 0;

function load_problem(problemNumber) {
  $.ajax("../qualifiers/problem_" + problemNumber + ".json",
         {dataType: "json",
          success:function(data) {
            g_board = data;
            g_board.occup =
              _.map(_.range(g_board.height), function(y) {
                return _.map(_.range(g_board.width), function(x) {
                  return false;
                })
              });
            g_board.filled.forEach(function(p) {
              g_board.occup[p.y][p.x] = true;
            });
            g_board.cur_piece = {unit_id: 0, rotation: 0};
            draw_board(g_board);
          }});
}

// convert from the coordinate system in the problem to
// a uniform coordinate system where x increases to the east
// and y increases to the southeast
function uniformize_coords(x, y) {
  return {x:x - Math.floor(y / 2), y:y};
}

function draw_hex(scale, xx, yy, c) {
  var p = uniformize_coords(xx, yy);
  var x = p.x;
  var y = p.y;
  // "scale" is the edge length of the hexagon in pixels
  d.save();
  d.translate(2 * scale, 3 * scale);
  d.translate(scale * R3 * (2 * x + y), scale * 2 * y * 0.75);
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
  d.fillStyle = "gray";
  d.textAlign = "center";
  var font_size = 0.4 * scale;
  d.font = font_size + "px sans-serif";
  d.fillText(xx + "," + yy, scale * R3, scale * 0.5 + font_size / 2);
  d.restore();
}

function draw_board(board) {
  d.clearRect(0,0,w,h);

  board.color =
    _.map(_.range(board.height), function(y) {
      return _.map(_.range(board.width), function(x) {
        return board.occup[y][x] ? "#fc0" : "#f7f7f7";
      })
    });

  var scale = 0.5 * Math.min(w, h) / Math.max(board.width, board.height);

  for (var i = 0; i < board.width; i++) {
    for (var j = 0; j < board.height; j++) {
      draw_hex(scale, i, j, board.color[j][i]);
    }
  }
}

load_problem(0);

$(document).on('keydown', function(e) {
  // can use "," and "." keys to move around problems

  if (e.keyCode == 188) {
    problemNumber--;
    load_problem(problemNumber);
  }
  if (e.keyCode == 190) {
    problemNumber++;
    load_problem(problemNumber);
  }
});
