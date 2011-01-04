var paper; 
	
var n_riders = 22;
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
		writeName(mx + 5, sy - 10, match.rider1, round.number, match.number);
	    
		paper.text(mx + (roundWidth / 2), sy + yOff - 5, "Match " + match.number).attr({"text-anchor": "start", "font-size": 13});
		
		sy = sy + yOff;
		sy = sy + yOff;
	
		drawLine(mx, sy, lx, sy);
		writeName(mx + 5, sy - 10, match.rider2, round.number, match.number);

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
	    writeName(mx+5, sy-10, champRider, round.number, 0);
	}
    }
}

function writeName(x, y, rider, roundNumber, matchNumber) {
    var n =  paper.text(x, y, rider.rider.name).attr({"text-anchor": "start", "font-size": 15});
    n.click(function(ev) { update_tournament(rider, matchNumber, roundNumber) });
    n.hover(function (event) {
	this.attr({fill: "red", cursor: "pointer"});
    }, function (event) {
	this.attr({fill: "black"});
    });
}


function drawLine(sx, sy, ex, ey) {
    paper.path([["M", sx, sy], ["L", ex, ey]]);
}

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