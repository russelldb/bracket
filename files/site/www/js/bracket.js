var paper; 
	
var n_riders = 32;
var tournament;

function tourny() {
    //grab your json
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
    if(data) {
	tournament = data;
    }
    rounds = tournament.length;
    riders = Math.pow(2, (rounds-1));
    
    if(paper) {
	paper.clear();
    } 

    paper = Raphael("canvas", ($(window).width() * .95),
		    $(window).height() * (Math.floor( riders / 16) ));

    roundWidth = Math.floor(paper.width / (rounds));
    
    for(var i = 0; i < rounds; i++) {
	drawRound(tournament[i].round, roundWidth);
    }
}

function drawRound(round, roundWidth) {
    var mx = roundWidth * (round.number - 1);
    var lx = roundWidth * round.number;
    var yOff = paper.height / (round.matches.length * 4);

    var sy = yOff;

    for(var x=0; x < round.matches.length; x++) {
	var match = round.matches[x].match;

	if(match) {
	    

	    var rider1 = match.rider1.rider;
	    var rider2 = match.rider2.rider;
	
	    if(rider1.name != "bye" && rider2.name != "bye") {
		drawLine(mx, sy, lx, sy);
		writeName(1, mx + 5, sy - 10, match.rider1, round.number, match.number);
	    
		paper.text(mx + (roundWidth / 2), sy + yOff - 5, "Match " + match.number).attr({"text-anchor": "start", "font-size": 13});
		
		sy = sy + yOff;
		sy = sy + yOff;
	
		drawLine(mx, sy, lx, sy);
		writeName(2, mx + 5, sy - 10, match.rider2, round.number, match.number);

		drawLine(lx, sy , lx,  sy - ( yOff * 2 ) );

		sy = sy + yOff;
		sy = sy + yOff;
	    } else {
		sy = sy + yOff;
		sy = sy + yOff;
		sy = sy + yOff;
		sy = sy + yOff;
	    }

	} else {
	    var champ = round.matches[x].champion;
	    var champRider = champ.rider;
	    sy = sy + yOff;
	    drawLine(mx, sy, lx, sy);
	    writeName(1, mx+5, sy-10, champRider, round.number, 0);
	}
    }
}

//Write a riders name and attatch handlers for editting it
function writeName(riderNumber, x, y, rider, roundNumber, matchNumber) {
    var n =  paper.text(x, y, rider.rider.name).attr({"text-anchor": "start", "font-size": 15});
    var node = $(n.node);

    //n.click(function(ev) { update_tournament(rider, matchNumber, roundNumber) });

    n.click(function(ev){
	var height = $('#changeName').height();
	var offset = node.offset();
	$('#changeName').css({left: offset.left,top: offset.top-(height/2)}).show();
	$('#changeName :input').val(rider.rider.name).focus().select();
	$('#changeName :input').bind('keydown', 'esc', function() { $('#changeName').hide();});
	$('#changeName :input').bind('keydown', 'return',  function() { 
	    var newName = $('#changeName :input').val();
	   $('#changeName').hide();
	    $('#changeName :input').unbind();
	    n.attr("text", newName);
	   update_rider( riderNumber ,newName, matchNumber, roundNumber) });
    });

    n.hover(function (event) {
	this.attr({fill: "red", cursor: "pointer"});
    }, function (event) {
	this.attr({fill: "black"});
    });
}

//Simply puts a path from start x (sx), start y (sy) to end x (ex), end y (ey)
function drawLine(sx, sy, ex, ey) {
    paper.path([["M", sx, sy], ["L", ex, ey]]);
}

// Updates a riders name (they keep the same seed)
//Useful for where a winning rider drops out for injury and the loser replaces them
// Rider number is 1 or 2 (depending on which rider in the match)
// newName is the new name, matchNumber and roundNumber are so we can update the backing JSON
function update_rider(riderNumber, newName, matchNumber, roundNumber) {
    for(var i = 0 ; i < tournament.length; i++) {
	var round = tournament[i].round;
	if(round.number == roundNumber) {
	    for(var j = 0; j < round.matches.length; j++) {
		var match = round.matches[j].match;
		if(match.number == matchNumber) {
		    match["rider"+riderNumber].rider.name = newName;
		    tournament[i].round.matches[j].match = match;
		}
	    }
	}
    }
    draw_tournament(tournament);
 }

//when a winner is selected we need to generate the next round(s) based on that
//Winner is a rider, matchNumber and roundNumber help us update the backing JSON
//Then call the server with the new JSON
function update_tournament(winner, matchNumber, roundNumber) {
    for(var i = 0 ; i < tournament.length; i++) {
	var round = tournament[i].round;
	if(round.number == roundNumber) {
	    for(var j = 0; j < round.matches.length; j++) {
		var match = round.matches[j].match;
		if(match.number == matchNumber) {
		    match.result.result.winner = winner;
		    tournament[i].round.matches[j].match = match;
		}
	    }
	}
    }
    // heh, now that is done, tell the server
    $.ajax({
	type: 'PUT',
	url: "bracket",
	contentType: 'application/json',
	data: JSON.stringify(tournament),
	success: draw_tournament,
	dataType: "json"
    });
    
}