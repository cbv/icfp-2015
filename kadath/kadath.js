fullscreen(); // sets global variables w, h
c.width = (w = innerWidth) * devicePixelRatio;
c.height = (h = innerHeight) * devicePixelRatio;
c.style.width = innerWidth + "px";
c.style.height = innerHeight + "px";

var R3 = Math.sqrt(3) / 2;
var problemNumber = 0;

function init_piece(board, unit_id) {
  g_board.cur_piece = {unit_id: unit_id, rotation:0};
  var members = board.units[unit_id].members;
//  console.log(JSON.stringify(members));
  var xs = members.map(function(z) { return z.x; });
  var ys = members.map(function(z) { return z.y; });
  var minx = _.min(xs);
  var maxx = _.max(xs);
  var miny = _.min(ys);
  var maxy = _.max(ys);
  g_board.cur_piece.translation = {x: Math.ceil((board.width - minx - maxx) / 2) - 1, y: -miny }
//  console.log(JSON.stringify(g_board.cur_piece.translation));
}

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
            init_piece(g_board, 0);
            draw_board(g_board);
          },
          error:function(error) {
            draw_board(g_board);
          }});
}

// Convert from the "nonuniform coordinate system" (the one specified
// in the problem description) to a uniform coordinate system where x
// (still) increases to the east and y consistently increases to the
// southeast.
function uniformize_coords(p) {
  return {x:p.x - Math.floor(p.y / 2), y:p.y};
}

// Invert the above coordinate change
function deuniformize_coords(p) {
  return {x:p.x + Math.floor(p.y / 2), y:p.y};
}

// p1 and p2 are both in uniform coordinates; return p1 rotated about p2 clockwise by n * 60 degrees
function rotate_about(p1, p2, n) {
  var rotated = rotate_about_origin({x: p1.x - p2.x, y: p1.y - p2.y}, n);
  return {x: p2.x + rotated.x, y: p2.y + rotated.y};
}

// p is in uniform coordinates; return p rotated about origin clockwise by n * 60 degrees
function rotate_about_origin(p, n) {
  var q = p;
  for (var i = 0; i < n; i++) {
    q = {x:-q.y, y:q.x+q.y};
  }
  return q;
}

function vadd(p1, p2) {
  return {x:p1.x + p2.x, y:p1.y + p2.y};
}

function draw_hex(scale, xx, yy, c) {
  var p = uniformize_coords({x:xx, y:yy});
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

function valid_pt(board, pt) {
  return pt.x >= 0 && pt.y >= 0 && pt.x < board.width && pt.y < board.height;
}
function draw_board(board) {
  d.save();
  d.scale(devicePixelRatio, devicePixelRatio);
  d.clearRect(0,0,w,h);
  try {
    board.color =
      _.map(_.range(board.height), function(y) {
        return _.map(_.range(board.width), function(x) {
          return board.occup[y][x] ? "#fc0" : "#f7f7f7";
        })
      });

    var unit = board.units[board.cur_piece.unit_id];
    var upivot = uniformize_coords(unit.pivot);
    unit.members.forEach(function(member) {
      var xmember = deuniformize_coords(
        vadd(
          board.cur_piece.translation,
          rotate_about(uniformize_coords(member), upivot, board.cur_piece.rotation)
        )
      );
      if (valid_pt(board, xmember)) {
        board.color[xmember.y][xmember.x] = "#f54";
      }
    });
    var xpivot = deuniformize_coords(vadd(board.cur_piece.translation, upivot));
    if (valid_pt(board, xpivot)) {
      board.color[xpivot.y][xpivot.x] = board.color[xpivot.y][xpivot.x] == "#f54" ?
        "#f5f" : "#ff0";
    }

    var scale = 0.5 * Math.min(w/board.width, h/board.height);

    for (var i = 0; i < board.width; i++) {
      for (var j = 0; j < board.height; j++) {
        draw_hex(scale, i, j, board.color[j][i]);
      }
    }
  }
  finally {
    d.fillStyle = "rgba(0,0,100,0.2)";
    d.font = "50px sans-serif";
    d.fillText("Problem: " + problemNumber + " Unit: " + (board.cur_piece.unit_id + 1) + "/" +
               board.units.length, 10, h - 10);
    d.restore();
  }
}

load_problem(0);
console.log("Keybindings:\n ','/'.' prev/next problem\n '/','\\' rotate piece cw,ccw \n 'uihknm' move piece \n '['/']' prev/next unit");
$(document).on('keydown', function(e) {
  //   console.log(e.keyCode);
  if (e.keyCode == 188) { // ","
    problemNumber--;
    load_problem(problemNumber);
  }
  if (e.keyCode == 190) { // "."
    problemNumber++;
    load_problem(problemNumber);
  }
  if (e.keyCode == 191) { // "/"
    g_board.cur_piece.rotation = (g_board.cur_piece.rotation + 1) % 6;
    draw_board(g_board);
  }
  if (e.keyCode == 220) { // "\\"
    g_board.cur_piece.rotation = (g_board.cur_piece.rotation + 5) % 6;
    draw_board(g_board);
  }
  if (e.keyCode == 85) { // "u"
    g_board.cur_piece.translation.y--;
    draw_board(g_board);
  }
  if (e.keyCode == 73) { // "i"
    g_board.cur_piece.translation.x++;
    g_board.cur_piece.translation.y--;
    draw_board(g_board);
  }
  if (e.keyCode == 72) { // "h"
    g_board.cur_piece.translation.x--;
    draw_board(g_board);
  }
  if (e.keyCode == 75) { // "k"
    g_board.cur_piece.translation.x++;
    draw_board(g_board);
  }
  if (e.keyCode == 78) { // "n"
    g_board.cur_piece.translation.x--;
    g_board.cur_piece.translation.y++;
    draw_board(g_board);
  }
  if (e.keyCode == 77) { // "m"
    g_board.cur_piece.translation.y++;
    draw_board(g_board);
  }
  if (e.keyCode == 219) { // "["
    init_piece(g_board, (g_board.cur_piece.unit_id + g_board.units.length- 1) % g_board.units.length);
    draw_board(g_board);
  }
  if (e.keyCode == 221) { // "]"
    init_piece(g_board, (g_board.cur_piece.unit_id + 1) % g_board.units.length);
    draw_board(g_board);
  }


});
