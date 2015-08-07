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
  var ys = members.map(function(z) { return z.y; });
  var miny = _.min(ys);

  var xs = members.map(function(z) { return z.x - Math.floor(z.y / 2) + Math.floor ((z.y - miny) / 2); });

  var minx = _.min(xs);
  var maxx = _.max(xs);

  var maxy = _.max(ys);
  g_board.cur_piece.translation = {x: Math.ceil((board.width - minx - maxx) / 2) - 1, y: -miny }
//  console.log(JSON.stringify(g_board.cur_piece.translation));
}

function load_problem(problemNumber) {
  g_moves = "";
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
            init_piece(g_board, Math.floor(Math.random() * g_board.units.length));
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
function uniformize(p) {
  return {x:p.x - Math.floor(p.y / 2), y:p.y};
}

// Invert the above coordinate change
function deuniformize(p) {
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

function draw_spot(scale, xx, yy, c, r) {
  r = r || 0.9;
  var p = uniformize({x:xx, y:yy});
  var x = p.x;
  var y = p.y;
  d.save();
  d.translate(2 * scale, 3 * scale);
  d.translate(scale * R3 * (2 * x + y), scale * 2 * y * 0.75);
  d.beginPath();
  d.arc(scale * R3, scale * 0.5, scale * R3 * r, 0, 2 * Math.PI);
  d.fillStyle = c;
  d.fill();
  d.restore();
}

function draw_hex(scale, xx, yy, c) {
  var p = uniformize({x:xx, y:yy});
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
  d.restore();
}

function draw_text(scale, xx, yy, c) {
  var p = uniformize({x:xx, y:yy});
  var x = p.x;
  var y = p.y;
  d.save();
  d.translate(2 * scale, 3 * scale);
  d.translate(scale * R3 * (2 * x + y), scale * 2 * y * 0.75);
  d.fillStyle = c;
  d.textAlign = "center";
  var font_size = 0.4 * scale;
  d.font = font_size + "px sans-serif";
  d.fillText(xx + "," + yy, scale * R3, scale * 0.5 + font_size / 2);
  d.restore();
}

function valid_pt(board, pt) {
  return pt.x >= 0 && pt.y >= 0 && pt.x < board.width && pt.y < board.height;
}

function empty_pt(board, pt) {
  return valid_pt(board, pt) && !board.occup[pt.y][pt.x];
}

function valid_piece_placement(board, piece) {
  return _.all(in_situ(board, piece), function(member) {
    return empty_pt(board, member);
  });
}

function in_situ(board, piece) {
  var unit = board.units[piece.unit_id];
  var upivot = uniformize(unit.pivot);
  return unit.members.map(function(member) {
    return deuniformize(
      vadd(
        piece.translation,
        rotate_about(uniformize(member), upivot, piece.rotation)
      )
    );
  });
}

function draw_board(board) {
  d.save();
  d.scale(devicePixelRatio, devicePixelRatio);
  d.clearRect(0,0,w,h);

  board.color =
    _.map(_.range(board.height), function(y) {
      return _.map(_.range(board.width), function(x) {
        return board.occup[y][x] ? "#fc0" : "#f7f7f7";
      })
    });

  var scale = 0.5 * Math.min(w/board.width, h/board.height);

  for (var i = 0; i < board.width; i++) {
    for (var j = 0; j < board.height; j++) {
      draw_hex(scale, i, j, board.color[j][i]);
    }
  }

  in_situ(board, board.cur_piece).forEach(function(xmember) {
    draw_spot(scale, xmember.x, xmember.y, "#f77");
  });

  var upivot = uniformize(board.units[board.cur_piece.unit_id].pivot);
  var xpivot = deuniformize(vadd(board.cur_piece.translation, upivot));
  draw_spot(scale, xpivot.x, xpivot.y, "#a00", 0.5);

  for (var i = 0; i < board.width; i++) {
    for (var j = 0; j < board.height; j++) {
      draw_text(scale, i, j, "#333");
    }
  }


  d.fillStyle = "rgba(0,0,100,0.2)";
  d.font = "50px sans-serif";
  d.fillText("Problem: " + problemNumber + " Unit: " + (board.cur_piece.unit_id + 1) + "/" +
             board.units.length, 10, h - 10);
  d.restore();


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
    attempt_move(0, 0, 1, "d");
  }
  if (e.keyCode == 220) { // "\\"
    attempt_move(0,0,-1, "k");
  }
  if (e.keyCode == 85) { // "u"
    attempt_move(0,-1,0, "=");
  }
  if (e.keyCode == 73) { // "i"
    attempt_move(1,-1,0, "=");
  }
  if (e.keyCode == 72) { // "h"
    attempt_move(-1,0,0,"p");
  }
  if (e.keyCode == 75) { // "k"
    attempt_move(1,0,0,"b");
  }
  if (e.keyCode == 78) { // "n"
    attempt_move(-1,1,0,"a");
  }
  if (e.keyCode == 77) { // "m"
    attempt_move(0,1,0,"l");
  }
  // if (e.keyCode == 219) { // "["
  //   init_piece(g_board, (g_board.cur_piece.unit_id + g_board.units.length- 1) % g_board.units.length);
  //   draw_board(g_board);
  // }
  // if (e.keyCode == 221) { // "]"
  //   init_piece(g_board, (g_board.cur_piece.unit_id + 1) % g_board.units.length);
  //   draw_board(g_board);
  // }


});

function attempt_move(dx, dy, drot, chr) {
  g_moves += chr;
  var pc = g_board.cur_piece;
  var new_piece = {unit_id: pc.unit_id,
                   translation: {x: pc.translation.x + dx,
                                 y: pc.translation.y + dy},
                   rotation: (pc.rotation + 6 + drot) % 6,
                  }
  if (valid_piece_placement(g_board, new_piece)) {
    g_board.cur_piece = new_piece;
  }
  else {
    in_situ(g_board, g_board.cur_piece).forEach(function(pt) {
      g_board.occup[pt.y][pt.x] = true;
      clear_lines(g_board);
      init_piece(g_board, Math.floor(Math.random() * g_board.units.length));
    });
  }
  draw_board(g_board);
}

function clear_lines(board) {
  var new_occup = [];
  for (var y = 0; y < board.occup.length; y++) {
    if (!_.all(board.occup[y], function(x) { return x; })) {
      new_occup.push(board.occup[y]);
    }
  }
  var missing = board.height - new_occup.length;
  for (var i = 0; i < missing; i++) {
    new_occup.unshift(_.map(_.range(board.width), function() { return false; }));
  }
  board.occup = new_occup;
}
