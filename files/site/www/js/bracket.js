               
var paper; 
	
var n_riders = 22;

function tourny() {
    //grab the json
    $.ajax({url: "bracket",
	    data: {n: n_riders},
            dataType: 'json',
            success: draw_tournament,
	    error: function() {
		alert("Bad request")
	    }
	   });
}

function draw_tournament(data) {
    rounds = data.length;
    riders = Math.pow(2, rounds);
    
    if(paper) {
	paper.clear();
    } 

    paper = Raphael("canvas", $(window).width(),
		    $(window).height() * (Math.floor( riders / 16) ));

    
    roundWidth = Math.floor(paper.width / (rounds+1));
    
    for(var i = 0; i < rounds; i++) {
	drawRound(data[i].round, roundWidth, i);
    }
}

function drawRound(round, roundWidth, roundWidthMultiplier) {
  var mx = roundWidth * roundWidthMultiplier;
  var lx = roundWidth * (roundWidthMultiplier + 1);
	
  var yOff = paper.height / (round.matches.length * 4);

  var sy = yOff;
	
  for(var x=0; x < round.matches.length; x++) {
      var match = round.matches[x].match;
      var rider1 = match.rider1.rider;
      var rider2 = match.rider2.rider;
      
      if(rider1.name != "bye" && rider2.name != "bye") {
	  drawLine(mx, sy, lx, sy);
	  writeName(mx + 5, sy - 10, match.rider1.rider.name);

          paper.text(mx + (roundWidth / 2), sy + yOff - 5, "Match " + match.number).attr({"text-anchor": "start", "font-size": 13});
	
	  sy = sy + yOff;
	  sy = sy + yOff;
	
	  drawLine(mx, sy, lx, sy);
	  writeName(mx + 5, sy - 10, match.rider2.rider.name);

	  drawLine(lx, sy , lx,  sy - ( yOff * 2 ) );

	  sy = sy + yOff;
	  sy = sy + yOff;
      } else {
	  sy = sy + yOff;
	  sy = sy + yOff;
	  sy = sy + yOff;
	  sy = sy + yOff;
      }
  }
}

function writeName(x, y, name) {
    paper.text(x, y, name).attr({"text-anchor": "start", "font-size": 15});
}

function drawLine(sx, sy, ex, ey) {
    paper.path([["M", sx, sy], ["L", ex, ey]]);
}


function reveal() {
    $("#panel").slideToggle("slow");
    $("form > div:last-child input[name='seed']").unbind('keydown', next_rider);
    $("form > div:last-child input[name='seed']").bind('keydown', 'tab', next_rider);
}

function next_rider(retVal) {
    $("form > div:last-child input[name='seed']").unbind('keydown', next_rider);
    $("form > div:first-child").clone(true).insertAfter("form > div:last-child");
    $("form > div:last-child input[name='seed']").bind('keydown', 'tab', next_rider);
    $("form > div:last-child input[type=text]").bind('keydown', 'ctrl+d', remove_current);
    $("form > div:last-child input[type=text]").bind('keydown', 'esc', function() {$("#panel").slideToggle("slow");});
    $("form > div:last-child input[type=text]").bind('keydown', 'return', send_riders);
    return retVal;
}

function remove_last() {
    $("form > div:last-child").remove();
    $("form > div:last-child input[name='seed']").bind('keydown', 'tab', next_rider);
    return false;
}

function remove_current(evt) {
    $(evt.currentTarget).parent().parent().remove();
    $("form > div:last-child input[name='seed']").bind('keydown', 'tab', next_rider);
    $("form > div:last-child input[name='seed']").focus();
    return false;
}

function send_riders() {
    $.post("bracket", $("#riders").serialize(), draw_tournament);
}

