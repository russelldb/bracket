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
    var gender = $("#gender");
    $("#riders").append(gender);
    $.post("bracket", $("#riders").serialize(), draw_tournament);
}