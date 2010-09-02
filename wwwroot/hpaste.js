$(document).ready(function(){
  // Update created date on paste
  $('#created').each(function(){
    var created = $(this);
    var localDate = parseUTCToLocal(created.text());
    created.text(formatDate(localDate,true,true));
    setInterval(function(){
      created.text(formatDate(localDate,true,true));
    },1000);
  });
  // Resize paste box appropriately
  $('div.hpaste-new-paste-form textarea').each(function(){
    var textarea = $(this);
    textarea.keydown(update_size);
    var last = '';
    function update_size(){
      var str = textarea.val();
      var lines = str.length - str.replace(/\n/g,'').length;
      textarea.attr('rows',Math.min(70,Math.max(15,lines+1)));
    }
    setInterval(function(){
      var str = textarea.val();
      if (str != last) {
        update_size();
      }
    },200);
  });
  // Create a copy of the submit button that is more convenient
  // to press at the top
  $('div.hpaste-new-paste-form .submit').each(function(){
    var copy = $(this).clone();
    var paste_form = $('div.hpaste-new-paste-form form');
    copy.addClass('top-right-submit');
    paste_form.append(copy);
  });
});

function parseUTCToLocal(str){
  var d = /^([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2}):([0-9]{2})/;
  var m = str.match(d);
  m = m.slice(1);
  return new Date(Date.UTC(m[0], m[1]-1, m[2], m[3], m[4], m[5]));
};

function formatDate(d,z,span) {
  var tz = '';
  if (z) {
    var m = d.toString().match(/\(([^\)]+)\)$/);
    if (m && m[1]) tz = ' (' + m[1] + ')';
  }
  if (span) sp = ' (' + ago(d) + ')';
  return d.getFullYear() + '-' + pad(d.getMonth() + 1) + '-' + pad(d.getDate()) + ' ' +
    pad(d.getHours()) + ':' + pad(d.getMinutes()) + ':' + pad(d.getSeconds()) + tz + sp;
  function pad(n){
    if (('' + n).length == 1) return '0' + n;
    else return n;
  }
};

var ago = (function(){
  function seconds(ms){return ms*1000;};
  function minutes(ms){return seconds(ms)*60;};
  function hours(ms){return minutes(ms)*60;}
  function days(ms){return hours(ms)*24;}
  function weeks(ms){return days(ms)*7;}
  function ago(d) {
    var ms = Date.now() - d;
    var rnd = Math.round;
    var out = '';
    function range(x,y){return ms >= x && ms <= y;};
    if (range(0,seconds(1))) out = "a second";
    else if (range(0,minutes(1) - seconds(10))) out = "just now";
    else if (ms >= minutes(1) - seconds(10)
             && ms <= minutes(1) + seconds(10)) out = "about a minute";
    else if (ms >= minutes(25) && ms <= minutes(35)) out = "half an hour";
    else if (ms > hours(1) - minutes(10) && ms < hours(1) + minutes(10)) out = "about an hour";
    else if (ms < hours(1)) out = rnd(ms / minutes(1)) + " minutes";
    else if (rnd(ms / hours(1)) < 2) out = "an hour";
    else if (ms < days(1)) out = rnd(ms / hours(1)) + " hours";
    else if (ms >= days(1) && ms <= days(2)) out = "a day";
    else if (ms < weeks(1)) out = rnd(ms / days(1)) + " days";
    else if (ms > weeks(1) && ms < weeks(2)) out = "a week";
    else if (ms > weeks(1) && ms < weeks(4)) out = rnd(ms / weeks(1)) + " weeks";
    else if (rnd(ms / weeks(4)) < 2) out = "a month";
    else if (ms < weeks(20)) out = "about " + rnd(ms / weeks(4)) + " months";
    else if (rnd(ms / weeks(52)) < 2) out = "a year";
    else out = rnd(ms / weeks(52)) + " years";
    return out == "just now"? out : out + " ago";
  };

  return ago;
})();
