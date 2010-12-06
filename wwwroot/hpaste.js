String.prototype.trim = function(){
  return this.replace(/^[ ]+/g,'').replace(/[ ]+$/g,'');
};

$(document).ready(function(){
  // Update dates to show local times
  update_dates('.hpaste-latest-pastes .utctime',true);
  update_dates('.hpaste-info .utctime',false);
  // Resize paste box appropriately
  resize_text_box();
  // Create a copy of the submit button that is more convenient
  // to press at the top
  copy_submit_button();
  // Resize code container to fit containing code
  resize_code_container();
  // Add a clear after the form textarea
  $('.hpaste-new-paste-form textarea').after($('<div class="hpaste-clear"></div>'));
  // Form validation
  form_validation();
  // Auto-fill details
  form_fill();
  // Dynamic display switcher
  display_switch();
  // HLint clickable
  clickable_hlint_errors();
});

function jquery_cookie(a,b) {
  return $.cookie(a,b,{domain:'hpaste.org',path:'/'});
}

function layout_switcher() {
  var span =
    $('<span id="hpaste-choose-layout" href="javascript:">Layout: </span>');
  var select = $('<select><option>Thin</option><option>Wide</option></select>');
  span.append(select);
  $('.hpaste-nav').append(span);
  if (jquery_cookie('hpaste-layout')) select.val(jquery_cookie('hpaste-layout'));
  select.each(changeLayout);
  select.change(changeLayout);
  function changeLayout() {
    var choice = $(this).val();
    jquery_cookie('hpaste-layout',choice);
    if (choice == 'Thin') {
      $('#hpaste-wrap').attr('class','hpaste-wrap-fixed');
    } else if (choice == 'Wide') {
      $('#hpaste-wrap').attr('class','hpaste-wrap');
    }
  }
}

function display_switch(){
  $('.lang-switcher').each(function(){
    var s = $(this);
    s.find('input[type=submit]').hide();
    s.find('select').change(function(){
      s.submit();
    });
  });
};

function update_dates(selector,onlyago){
  $(selector).each(function(){
    var created = $(this);
    var localDate = parseUTCToLocal(created.text());
    created.text(formatDate(localDate,true,true,onlyago));
    setInterval(function(){
      created.text(formatDate(localDate,true,true,onlyago));
    },1000*30);
  });
}

function parseUTCToLocal(str){
  var d = /^([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2}):([0-9]{2})/;
  var m = str.match(d);
  m = m.slice(1);
  return new Date(Date.UTC(m[0], m[1]-1, m[2], m[3], m[4], m[5]));
};

function formatDate(d,z,span,onlyspan) {
  if (onlyspan) return ago(d);
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

function form_validation(){
  function check(form_submitted){
    var invalid = false;
    var first_invalid;
    var inputs = "*title,*author,language,channel,*paste".split(/,/g);
    for (var i = 0; i < inputs.length; i++) {
      var req = inputs[i].match(/\*/);
      var x = 0;
      $('.hpaste-new-paste-form').find('input,textarea,select').each(function(){
        if (x == i) {
          var input = $(this);
          var li = input.parent();
          if (i == 4) li = li.parent();
          if (req && input.val().trim() == '') {
            li.addClass('invalid-input');
            invalid = true;
            first_invalid = first_invalid || input;
          } else {
            li.removeClass('invalid-input');
          }
        } 
        x++;
      });
    }
    if (invalid && form_submitted) first_invalid.focus();
    return !invalid;
  }
  $('.hpaste-new-paste-form').find('input,textarea,select').each(function(){
    $(this).change(function(){ check(); });
  });
  var interval;
  $('.hpaste-new-paste-form form').submit(function(){
    if (!interval) interval = setInterval(check,1000);
    return check(true);
  });
}

// TODO: Refactor this crap, hehe.
function form_fill(){
  var i = 0;
  $('.hpaste-new-paste-form').find('input,select,textarea').each(function(){
    var inp = $(this);
    switch (i) {
    case 0: {
      $('.hpaste-info .hpaste-section-title').each(function(){
        inp.val($(this).text() + ' (annotation)');
        return false;
      });
      break;
    }
    case 1: {
      inp.val(jquery_cookie('author'));
      break;
    }
    case 2: {
      inp.val(jquery_cookie('language'));
      break;
    }
    case 3: {
      inp.val(jquery_cookie('channel'));
      break;
    }
    }
    i++;
  });
  $('.hpaste-new-paste-form').submit(function(){
    var i = 0;
    $('.hpaste-new-paste-form').find('input,select,textarea').each(function(){
      var inp = $(this);
      switch (i) {
      case 1: {
        jquery_cookie('author',inp.val());
        break;
      }
      case 2: {
        jquery_cookie('language',inp.val());
        break;
      }
      case 3: {
        jquery_cookie('channel',inp.val());
        break;
      }
      }
      i++;
    });
  });
}

function copy_submit_button(){
  $('div.hpaste-new-paste-form .submit').each(function(){
    var copy = $(this).clone();
    var paste_form = $('div.hpaste-new-paste-form form');
    copy.addClass('top-right-submit');
    paste_form.append(copy);
  });
}

function resize_text_box(){
  $('div.hpaste-new-paste-form textarea').each(function(){
    var textarea = $(this);
    textarea.keydown(update_size);
    textarea.change(update_size);
    var last = '';
    var interval;
    function update_size(){
      if (!interval) {
        interval = setInterval(function(){
          var str = textarea.val();
          if (str != last) {
            update_size();
          }
        },200);
      }
      var str = textarea.val();
      var lines = str.length - str.replace(/\n/g,'').length;
      textarea.attr('rows',Math.min(70,Math.max(15,lines+1)));
    }
  });
}

function resize_code_container(){
  $('.hpaste-wrap-fixed').each(function(){
    var codeWidth = 0;
    $('table.sourceCode').each(function(){
      codeWidth = Math.max(codeWidth,$(this).width());
    });
    var container = $('.hpaste-paste');
    var oldWidth = container.width();
    var diff = codeWidth - oldWidth;
    if (diff > 0) {
      container.width(codeWidth);
      $(this).width(codeWidth).css('max-width',codeWidth);
      $('.hpaste-info').width(codeWidth);
      $('.hpaste-create-new-paste').width(codeWidth);
    }
  });
}

function clickable_hlint_errors(){
  $('.hlint-hints li').each(function(){
    var m = $(this).text().match(/^([0-9]+)/);
    if (m) {
      $(this).click(function(){
        var line = m[0];
        window.location.href = 
          window.location.href.replace(/#.*/,'') + '#' + line;
      }).addClass('hlint-clickable');
    }
  });
}