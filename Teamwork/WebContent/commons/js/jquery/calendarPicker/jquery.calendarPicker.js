/*
  Copyright (c) 2009 Open Lab
  Written by Roberto Bicchierai http://roberto.open-lab.com
  Permission is hereby granted, free of charge, to any person obtaining
  a copy of this software and associated documentation files (the
  "Software"), to deal in the Software without restriction, including
  without limitation the rights to use, copy, modify, merge, publish,
  distribute, sublicense, and/or sell copies of the Software, and to
  permit persons to whom the Software is furnished to do so, subject to
  the following conditions:

  The above copyright notice and this permission notice shall be
  included in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
  LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
  OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
  WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

jQuery.fn.calendarPicker = function(options) {
  // --------------------------  start default option values --------------------------
  if (!options.date) {
    options.date = new Date();
  }

  if (typeof(options.years) == "undefined")
    options.years=1;

  if (typeof(options.months) == "undefined")
    options.months=3;

  if (typeof(options.days) == "undefined")
    options.days=4;

  if (typeof(options.fullMonth) == "undefined")
    options.fullMonth=false;

  if (typeof(options.firstDayOfWeek) == "undefined")
    options.firstDayOfWeek=0;

  if (typeof(options.useWheel) == "undefined")
    options.useWheel=true;

  if (typeof(options.callbackDelay) == "undefined")
    options.callbackDelay=500;

  if (typeof(options.monthNames) == "undefined")
    //options.monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
    options.monthNames =  Date.monthAbbreviations;

  if (typeof(options.dayNames) == "undefined")
    //options.dayNames = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
    options.dayNames =  Date.dayAbbreviations;


  // --------------------------  end default option values --------------------------



  //build the calendar on the first element in the set of matched elements.
  var theDiv = this.eq(0);//$(this);
  theDiv.addClass("calBoxPicker");
	theDiv.data("options",options);

  //empty the div
  theDiv.empty();

  var divYears = $("<div>").addClass("calYearCP");
  var divMonths = $("<div>").addClass("calMonthCP");
  var divDays = $("<div>").addClass("calDayCP");
  if (options.fullMonth)
    divDays.addClass("calFullMonthCP");


  theDiv.append(divYears).append(divMonths).append(divDays);


  theDiv.bind("changeDate",function(event,date,noCallback) {

    var theDiv=$(this);
	  var options= theDiv.data("options");

    var divYears = theDiv.find(".calYearCP:first");
    var divMonths = theDiv.find(".calMonthCP:first");
    var divDays = theDiv.find(".calDayCP:first");
    
    //calendar.currentDate = date;
    options.currentDate=date;

    var fillYears = function(date) {
      var year = date.getFullYear();
      var t = new Date();
      divYears.empty();
      var nc = options.years*2+1;
      var w = theDiv.width()/nc;
      for (var i = year - options.years; i <= year + options.years; i++) {
        var d = new Date(date);
        d.setFullYear(i);
        var span = $("<span>").addClass("calElementCP").attr("millis", d.getTime()).html(i);
        span.css("width",w);
        if (d.getYear() == t.getYear())
          span.addClass("today");
        if (d.getYear() == date.getYear())
          span.addClass("selected");
        divYears.append(span);
      }
    };

    var fillMonths = function(date) {
      var month = date.getMonth();
      var t = new Date();
      divMonths.empty();
      var oldday = date.getDay();
      var nc = options.months*2+1;
      var w = theDiv.width()/nc;
      for (var i = -options.months; i <= options.months; i++) {
        var d = new Date(date);
        var oldday = d.getDate();
        d.setMonth(month + i);

        if (d.getDate() != oldday) {
          d.setMonth(d.getMonth() - 1);
          d.setDate(28);
        }
        var span = $("<span>").addClass("calElementCP").attr("millis", d.getTime()).html(options.monthNames[d.getMonth()]);
        span.css("width",w);
        if (d.getYear() == t.getYear() && d.getMonth() == t.getMonth())
          span.addClass("today");
        if (d.getYear() == date.getYear() && d.getMonth() == date.getMonth())
          span.addClass("selected");
        divMonths.append(span);

      }
    };

    var fillDays = function(date) {
      var day = date.getDate();
      var t = new Date();
      divDays.empty();
      var nc = options.days*2+1;
      var w = theDiv.width()/nc;
      for (var i = -options.days; i <= options.days; i++) {
        var d = new Date(date);
        d.setDate(day + i);
        var span = $("<span>").addClass("calElementCP").attr("millis", d.getTime());
        span.html("<span class=dayNumber>" + d.getDate() + "</span><br>" + options.dayNames[d.getDay()]).css("width",w);
        if (d.getYear() == t.getYear() && d.getMonth() == t.getMonth() && d.getDate() == t.getDate())
          span.addClass("today");
        if (d.getYear() == date.getYear() && d.getMonth() == date.getMonth() && d.getDate() == date.getDate())
          span.addClass("selected");

        //call the dayRenderer
        if (typeof(options.dayRenderer) == "function")
          options.dayRenderer(span,date);

        divDays.append(span);
      }
    };

    var fillDaysFullMonth = function(date) {
      divDays.empty();
      var t = new Date();//today
      var w = theDiv.width()/7;
      // draw day headers
      var d = new Date(date);
      d.setDate(d.getDate()-d.getDay()+options.firstDayOfWeek);
      for (var i = 0; i < 7; i++) {
        var span = $("<span>").addClass("calDayHeaderCP").attr("day", d.getDay());
        span.css("width",w);
        span.html(options.dayNames[d.getDay()]);

        //call the dayHeaderRenderer
        if (typeof(options.dayHeaderRenderer) == "function")
          options.dayHeaderRenderer(span,d.getDay());

        divDays.append(span);
        d.setDate(d.getDate()+1);
      }

      //draw cells
      d = new Date(date);
      d.setDate(1); // set day to start of month
      d.setDate(d.getDate()-(d.getDay()==0?7:d.getDay())+options.firstDayOfWeek);//go to first day of week

      var i=0;

      while ((d.getMonth()<=date.getMonth() && d.getFullYear()<=date.getFullYear()) || d.getFullYear()<date.getFullYear() || (i%7!=0)) {
        var span = $("<span>").addClass("calElementCP").attr("millis", d.getTime());

        span.html("<span class=dayNumber>" + d.getDate() + "</span>").css("width",w);
        if (d.getYear() == t.getYear() && d.getMonth() == t.getMonth() && d.getDate() == t.getDate())
          span.addClass("today");

	      // 04-03-2017 Added "&& noCallback" to prevent cross month day selection.
        if (d.getYear() == date.getYear() && d.getMonth() == date.getMonth() && d.getDate() == date.getDate() && noCallback)
          span.addClass("selected");

        if(d.getMonth()!=date.getMonth())
          span.addClass("calOutOfScope");

        //call the dayRenderer
        if (typeof(options.dayRenderer) == "function")
          options.dayRenderer(span,d);

        divDays.append(span);
        d.setDate(d.getDate()+1);
        i++;
      }

    };

    var deferredCallBack = function() {
      if (typeof(options.callback) == "function") {
        if (options.timer)
          clearTimeout(options.timer);

        options.timer = setTimeout(function() {
          options.callback(theDiv);
        }, options.callbackDelay);
      }
    };


    fillYears(date);
    fillMonths(date);
    if (options.fullMonth)
      fillDaysFullMonth(date);
    else
      fillDays(date);

    if (!noCallback)
      deferredCallBack();

  });


  theDiv.bind("repaintDays", function(){
    var theDiv=$(this);
    var options= theDiv.data("options");

    if (typeof(options.dayRenderer) == "function") {
      theDiv.find(".calDayCP .calElementCP").each(function() {
        //call the dayRenderer
        options.dayRenderer($(this), new Date(parseInt($(this).attr("millis"))));
      });
    }
  });

  theDiv.click(function(ev) {
    var el = $(ev.target).closest(".calElementCP");
    var date = new Date(parseInt(el.attr("millis")));

    var theDiv= el.closest(".calBoxPicker");
    var options= theDiv.data("options");


    //click on day?
    if (el.parents(".calDayCP").size()>0) {
      theDiv.trigger("changeDate",[date,true]);
      if (typeof(options.dayClick)=="function"){
        options.dayClick(theDiv);
      }
    } else  {
       theDiv.trigger("changeDate",[date]);
    }
  });

  theDiv.bind("resize",function(){
    var w= theDiv.width();
    var years=theDiv.find(".calYearCP .calElementCP");
    var ny=years.size();
    years.each(function(){
      $(this).css("width",(w/ny));
    });
    var months=theDiv.find(".calMonthCP .calElementCP");
    var nm=months.size();
    months.each(function(){
      $(this).css("width",(w/nm));
    });

    theDiv.find(".calFullMonthCP .calDayHeaderCP,.calFullMonthCP .calElementCP").css("width",w/7);
  });

  theDiv.trigger("changeDate",[options.date,true]);

};
