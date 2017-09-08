<%@ page import="org.jblooming.waf.settings.I18n, org.jblooming.utilities.JSP" %>
<%----------------------------------------------------------  PAGES DEFINITION  ---------------------------------------------------------%>

<%-------------------------------------  calendar filter page ----------------------------------------%>
<div data-role="page" id="calendarPage" title="<%=I18n.get("AGENDA")%>">
  <div data-role="content">
    <div id="calendarPlace"></div>
  </div>

  <div data-role="footer" data-position="fixed" class="groupRow">
    <div class="groupCell inputBox col6">
      <div data-role="button" onmousedown="backPage();" class="back" title="<%=I18n.get("BACK")%>"></div>
    </div>
    <div class="groupCell inputBox col6">
      <button onmousedown="goToDayAndList(new Date());" class=""><%=I18n.get("TODAY")%></button>
    </div>
  </div>
</div>

<%-------------------------------------  calendar day list events page ----------------------------------------%>
<div data-role="page" id="calendarDayList">
  <div data-role="content" class="scroll">

    <%--<table width="100%" cellspacing="3">
      <tr>
        <td width="50%"><div data-role="button" onclick="agendaMove(false);" class="prev full" title="<%=I18n.get("PREVIOUS")%>"></div></td>
        <td width="50%"><div data-role="button" onclick="agendaMove(true);" class="next full" title="<%=I18n.get("NEXT")%>"></div></td>
      </tr>
    </table>--%>
    <div id="calendarDayListPlace" style="position:relative;"></div>
  </div>



  <div data-role="footer" data-position="fixed" class="groupRow">
    <div class="groupCell inputBox col6">
      <div data-role="button" onmousedown="backPage();" class="back" title="<%=I18n.get("BACK")%>"></div>
    </div>
    <div class="groupCell inputBox col6">
      <button onmousedown="addAgendaEvent($(this));" millis="(#=obj.millis#)" class="first"><span class="teamworkIcon big">P</span></button>
    </div>
  </div>

</div>

<%-------------------------------------  event view page ----------------------------------------%>
<div data-role="page" id="eventView">
  <div data-role="content" class="scroll">
    <div id="eventViewPlace"></div>
  </div>
  <div data-role="footer" data-position="fixed" class="groupRow">
    <div class="groupCell inputBox col6">
      <div data-role="button" onmousedown="backPage();" class="back" title="<%=I18n.get("BACK")%>"></div>
    </div>
    <div class="groupCell inputBox col6">
      <button class="delete" msg="<%=I18n.get("FLD_CONFIRM_DELETE")%>" onmousedown="deleteAgendaEvent($(this));" style="display:(#=obj.authorId!=applicationCache.user.person.id?'none':''#)"><%=I18n.get("DELETE")%></button>
    </div>
  </div>
</div>

<%-------------------------------------  event editor page ----------------------------------------%>
<div data-role="page" class="editor noFooter" id="eventEditor" title="<%=I18n.get("EVENT_DATA")%>">
  <div data-role="header" data-position="fixed">
    <div data-role="button" onmousedown="backPage();" class="close" ></div>
    <div data-role="title"></div>
  </div>

  <div data-role="content" class="scroll">
    <div id="eventEditorPlace"></div>
  </div>


  <div data-role="footer" data-position="fixed" class="groupRow">
    <div class="groupCell inputBox col6">
      <div data-role="button" onmousedown="backPage();" class="back" title="<%=I18n.get("BACK")%>"></div>
    </div>
    <div class="groupCell inputBox col6">
      <button onmousedown="saveAgendaEvent($(this));" class="save" ><%=I18n.get("SAVE")%></button>
    </div>
  </div>



</div>



<%----------------------------------------------------------  TEMPLATES  ---------------------------------------------------------%>
<div class="_mobileTemplates">


  <%-- ---------------------------------  AGENDA_GRID template ------------------------------------------------------------------------------------------------------------------- --%>
  <%=JST.start("AGENDA_GRID")%>
  <div class="agendaGrid" onclick="addAgendaEvent($(this))" millis="(#=obj.millis#)" style="top:(#=obj.top#)px;">
    (#=getMillisInHours(obj.millis)#)
  </div>
  <%=JST.end()%>

  <%-- ---------------------------------  EVENT_ROW template ------------------------------------------------------------------------------------------------------------------- --%>
  <%=JST.start("EVENT_ROW")%>
  <div class="agendaEvent (#=obj.isPersonal?'personal':''#) (#=obj.isUnavailability?'unavailable':''#) (#=obj.isReminder?'reminder':''#) (#=obj.isMeeting?'meeting':''#)" eventId="(#=id#)" onclick="viewEvent($(this))" startMillis="(#=obj.startMillis#)"  endMillis="(#=obj.endMillis#)"  style="top:(#=obj.top#)px;height:(#=obj.height#)px;">
    <b>(#=obj.summary#)</b><br>
    (#=obj.description#)
  </div>
  <%=JST.end()%>

  <%-- ---------------------------------  EVENT_VIEW template ------------------------------------------------------------------------------------------------------------------- --%>
  <%=JST.start("EVENT_VIEW")%>
  <div class="editor viewer" eventId="(#=id#)">

    <div class="groupRow">
      <div class="groupCell col12">(#=new Date(obj.startMillis).format("HH:mm")#)
        <h3>(#=obj.summary#)

        </h3>

        <div style="(#=obj.description?'margin-top:10px':'display:none'#)">
          <%--<label><%=I18n.get("AGENDA_DESCRIPTION")%>
          </label>--%>
          (#=obj.description#)
        </div>
        (#=obj.isPersonal?'<span class="stateEvent personal"><%=I18n.get("IS_PERSONAL")%></span> ':''#) (#=obj.isUnavailability?'<span class="stateEvent unavailable"><%=I18n.get("IS_UNAVAILABLE")%></span> ':''#) (#=obj.isReminder?'<span class="stateEvent reminder"><%=I18n.get("IS_REMINDER")%></span>':''#)

        <div id="schedulePlace"></div>
      </div>
    </div>

   <div class="groupRow" style="(#=obj.location?'':'display:none'#)">
      <div class="groupCell col12">
        <label><%=I18n.get("FLD_LOCATION")%></label>
        (#=obj.location#)
      </div>
    </div>
    <div class="groupRow" style="(#=obj.targets?'':'display:none'#)">
      <div class="groupCell col12">
        <label><%=I18n.get("TARGETS")%></label>
        (#
        for (var i=0;i<obj.targets.length;i++){
        var res=obj.targets[i];
        #)<span onclick="viewResource($(this));" resourceId="(#=res.resId#)"><img id="userAvatar" title="TODO" class="face" src="/img/noPhoto_TW.jpg"> (#=res.resName#)</span>&nbsp;(#
        }
        #)
      </div>
    </div>
  </div>

  <%=JST.end()%>



  <%-- ---------------------------------  EVENT_EDITOR TEMPLATE ------------------------------------------------------------------------------------------------------------------- --%>
  <%=JST.start("EVENT_EDITOR")%>
  <div class="editor">
    <h1>(#=obj.date.format("EEEE, d MMMM, yyyy")#)</h1>
    <div class="groupRow">
      <div class="groupCell inputBox touchEl col12">
        <label><%=I18n.get("AGENDA_SUMMARY")%>
        </label>
        <input type="text" size="60" class="subject" required="true">
      </div>
    </div>
    <div class="groupRow">
      <div class="groupCell inputBox touchEl col12">
        <label><%=I18n.get("AGENDA_DESCRIPTION")%>
        </label>
        <textarea size="30" class="description"></textarea>
      </div>
    </div>
    <div class="groupRow">
      <div class="groupCell inputBox touchEl col12">
        <label><%=I18n.get("LOCATION")%>
        </label>
        <input type="text" size="60" class="location">
      </div>
    </div>
    <div class="groupRow">
      <div class="groupCell inputBox touchEl col6">
        <label><%=I18n.get("START_HOUR")%></label>

        <select class="startMillis">
          (#
          var d=new Date(currentDate.getTime());
          d.setHours(0,0,0,0);
          var sd=d.getTime();
          d.setDate(d.getDate()+1);
          var ed=d.getTime();
          for  (var i=sd;i< ed; i+=15*60000){
          #)<option value="(#=i#)" (#=Math.abs(i-sd-obj.millis)<=5*60000?'selected':''#)>(#=new Date(i).format('HH:mm')#)</option>(#
          }
          #)
        </select>
      </div>

      <div class="groupCell inputBox touchEl col6">
        <label><%=I18n.get("DURATION")%>
        </label><select class="duration" name="DURATION">
        <%@include file="durationSelector.jsp" %>
      </select>
      </div>
    </div>


    <label for="" class="checkbox checkbox--material reminder">
      <input id="" name="" type="checkbox" class="checkbox__input checkbox-skin__input reminder">
      <div class="checkbox__checkmark checkbox-skin__checkmark" ></div>
      <%=I18n.get("IS_REMINDER")%>
    </label>

    <label for="" class="checkbox checkbox--material personal">
      <input id="" name="" type="checkbox" class="checkbox__input checkbox-skin__input personal">
      <div class="checkbox__checkmark checkbox-skin__checkmark "></div>
      <%=I18n.get("IS_PERSONAL")%>
    </label>

    <label for="" class="checkbox checkbox--material unavailable">
      <input id="" name="" type="checkbox" class="checkbox__input checkbox-skin__input unavailable">
      <div class="checkbox__checkmark checkbox-skin__checkmark"></div>
      <%=I18n.get("IS_UNAVAILABLE")%>
    </label>



<%--
    <br>
    <br>
    <br>
    <br>

    <input type="radio" class="a" name="a" >&nbsp;<span>test</span><br>
    <input type="radio" class="a" name="a">&nbsp;<span>test 1</span><br>

    <br>
    <br>
    <br>
    <br>

    <button class="full first" onclick="saveAgendaEvent($(this));"><%=I18n.get("SAVE")%> </button>
--%>
    <br>
    <br>

  </div><%=JST.end()%>


  <%=JST.start("SCHEDULE_NO_REPEAT")%>
  <div>
    (#
    var start=new Date(obj.startMillis);
    var end=new Date(obj.endMillis);
    if (start.toInt()==end.toInt()){
    #)<big>(#=new Date(obj.startMillis).format(Date.masks.shortTime)#) - (#=new Date(obj.endMillis).format(Date.masks.shortTime)#)</big>(#
    } else {
    #)
    <%=JSP.convertLineFeedToBR(I18n.get("SCHEDULE_PERIOD_CONTENT_%%...",
        "(#=new Date(obj.startMillis).format(Date.masks.fullDate)#)",
        "(#=new Date(obj.endMillis).format(Date.masks.fullDate)#)"))%>
    (#
    }
    #)
  </div>
  <%=JST.end()%>


  <%=JST.start("SCHED_DAILY")%>
  <div>
    <big>(#=new Date(obj.startMillis).format(Date.masks.shortTime)#) - (#=new Date(obj.endMillis).format(Date.masks.shortTime)#)</big><br>
    <%=I18n.get("SCHED_DAILY")%>:&nbsp;
    <%=JSP.convertLineFeedToBR(I18n.get("SCHEDULE_DAILY_CONTENT_%%...",
        "(#=new Date(obj.startMillis).format(Date.masks.fullDate)#)",
        "(#=new Date(obj.endMillis).format(Date.masks.fullDate)#)",
        "(#=obj.freq#)",
        "(#=obj.repeat#)"))%>
  </div>
  <%=JST.end()%>

  <%=JST.start("SCHED_WEEKLY")%>
  <div>
    <big>(#=new Date(obj.startMillis).format(Date.masks.shortTime)#) - (#=new Date(obj.endMillis).format(Date.masks.shortTime)#)</big><br>
    <%=I18n.get("SCHEDULE_WEEKLY")%>:&nbsp;
    <%=JSP.convertLineFeedToBR(I18n.get("SCHEDULE_WEEKLY_CONTENT_%%...",
        "(#=new Date(obj.startMillis).format(Date.masks.fullDate)#)",
        "(#=new Date(obj.endMillis).format(Date.masks.fullDate)#)",
        "(#=obj.days#)",
        "(#=obj.freq#)",
        "(#=obj.repeat#)"))%>
  </div>
  <%=JST.end()%>

  <%=JST.start("SCHED_MONTLY")%>
  <div>
    <big>(#=new Date(obj.startMillis).format(Date.masks.shortTime)#) - (#=new Date(obj.endMillis).format(Date.masks.shortTime)#)</big><br>
    <%=I18n.get("MONTLY")%>:&nbsp;
    <%=JSP.convertLineFeedToBR(I18n.get("SCHEDULE_MONTLY_CONTENT_%%...",
        "(#=new Date(obj.startMillis).format(Date.masks.fullDate)#)",
        "(#=new Date(obj.endMillis).format(Date.masks.fullDate)#)"))%>
  </div>
  <%=JST.end()%>

  <%=JST.start("SCHED_YEARLY")%>
  <div>
    <big>(#=new Date(obj.startMillis).format(Date.masks.shortTime)#) - (#=new Date(obj.endMillis).format(Date.masks.shortTime)#)</big><br>
    <%=I18n.get("SCHED_YEARLY")%>:&nbsp;
    <%=JSP.convertLineFeedToBR(I18n.get("SCHEDULE_YEARLY_CONTENT_%%...",
        "(#=new Date(obj.startMillis).format(Date.masks.fullDate)#)",
        "(#=new Date(obj.endMillis).format(Date.masks.fullDate)#)"))%>
  </div>
  <%=JST.end()%>
</div>

<%----------------------------------------------------------  DECORATORS  ---------------------------------------------------------%>
<script>
</script>


<%----------------------------------------------------------  RESOURCE PAGES FUNCTIONS  ---------------------------------------------------------%>
<script>
/* -------------------------------------------------------------------------------------  CALENDAR FUNCTIONS -------------------------------------------------*/
function calendarPageEnter(event, data, fromPage, isBack){
  $("#calendarPlace").oneTime(10,"dpCD",function(){$(this).trigger("changeDate", [currentDate]).resize()});
}

function calendarDayListEnter(event, data, fromPage, isBack){
  //console.debug("calendarDayListEnter",event, data, fromPage, isBack);
  fillDayList();

  var ndo = $("#calendarDayListPlace");

  ndo.swipe({
    swipeLeft:function(event, direction, distance, duration, fingerCount) {5
      if (distance > 140)
        agendaMove(true);
    },
    swipeRight:function(event, direction, distance, duration, fingerCount) {
      if (distance > 140)
        agendaMove(false);
    },
    /*
    swipeStatus:function(event, phase, direction, distance , duration , fingerCount, pinchZoom, fingerData) {

      if (direction == "left" && distance > 200){
        agendaMove(true);
      }else if(direction == "right" && distance > 200){
        agendaMove(false)
      }

    },
     */
    threshold:50,
    fingers:'all'
  });



}

function fillDayList(){
  loadCalendar(currentDate,function() {
    var ndo = $("#calendarDayListPlace");
    ndo.empty();
    var oneHourHeight = 60;
    var oneHour = 1000 * 60 * 60;
    var startDayMillis = 1000 * 60 * 60 * 7;
    var endDayMillis = 1000 * 60 * 60 * 23;

    millis = startDayMillis;
    //print the grid
    while (millis <= endDayMillis) {
      ndo.append($.JST.createFromTemplate({millis:millis,top:(millis - startDayMillis) / oneHour * oneHourHeight}, "AGENDA_GRID"));
      millis += oneHour;
    }
    ndo.css("height", ((endDayMillis - startDayMillis) / oneHour + 1) * oneHourHeight);
   // $("#calendarDayList [data-role=title]").html("October");
    $("#calendarDayList [data-role=title]").html(currentDate.format(mobileFullDateFormat));

    var dateInt = currentDate.toInt();

    var monthEvents = applicationCache.events[parseInt(dateInt / 100)]; //201105
    if (monthEvents && monthEvents.length > 0) {
      //console.debug("monthEvents",monthEvents);
      for (var i in monthEvents) {
        var ev = monthEvents[i];
        if (new Date(ev.startMillis).toInt() == dateInt) {
          //console.debug("event ",ev);
          var ed = new Date(ev.startMillis);
          ed.setHours(0, 0, 0, 0);
          ev.top = (ev.startMillis - ed.getTime() - startDayMillis) / oneHour * oneHourHeight;
          ev.top = ev.top < 0 ? 0 : ev.top;
          ev.height = (ev.endMillis - ev.startMillis) / oneHour * oneHourHeight;
          var evRow = $.JST.createFromTemplate(ev, "EVENT_ROW");
          evRow.data("event", ev);
          ndo.append(evRow);
        }
      }

    } else {
      ndo.append($.JST.createFromTemplate({}, "NO_ELEMENT_FOUND"));
    }

    //test glieogg
    var ora = new Date();
    if (currentDate.toInt() == ora.toInt()) {
      //appiccik rig
      var now = $("<div>").css({"border-top":"2px solid #F05D5D","position":"absolute","width":"100%","height":"4px","top":((ora.getHours() * 60 * 60 + ora.getMinutes() * 60 + ora.getSeconds()) * 1000 - startDayMillis) / oneHour * oneHourHeight});
      //now.append("&nbsp;");
      ndo.append(now);
    }

  });
}


function loadCalendar(date, callback) {
  //console.debug("loadCalendar ",date);
  var dateInt = date.toInt();
  var monthEvents = applicationCache.events[parseInt(dateInt / 100)];
  if (!monthEvents) {

    var filter = {"CM": "LOADEVENTS", "DATEINT": dateInt, "LDHOLY":!applicationCache.eventsHolidays ? "yes" : "no" };
    callController(filter, function(response) {

      //update cache
      if (response.eventsHolidays) {
        applicationCache.eventsHolidays = response.eventsHolidays;
      }

      applicationCache.events[parseInt(dateInt / 100)] = response.events;
      for (var i in response.events) {
        var ev = response.events[i];
        applicationCache.eventsDays.push(new Date(ev.startMillis).toInt());
      }

      $("#calendarPlace").trigger("repaintDays");

      if (typeof(callback) == "function")
        callback();
    });

  } else {
    if (typeof(callback) == "function")
      callback();
  }
}


function goToDayAndList(date) {
  currentDate=date;
  goToPage("calendarDayList");
}



function agendaMove(forward) {
  currentDate = new Date(currentDate.getTime() + (forward ? 1 : -1) * 24 * 60 * 60 * 1000);
  fillDayList();
}



function eventViewEnter(event, data, fromPage, isBack){
  var page=$(this);
  var ageEvent=getEventById(data.eventId);

  $("#eventView [data-role=title]").html(new Date(ageEvent.startMillis).format(mobileFullDateFormat));
  if (ageEvent.description)
    ageEvent.description = ageEvent.description.replace(/\n/g, "<br>");
  var view = $.JST.createFromTemplate(ageEvent, "EVENT_VIEW");
  var schedule = ageEvent.schedule;

  if (schedule.days) {
    var ret = [];
    for (var i in schedule.days) {
      //console.debug(schedule.days[i])
      ret.push(Date.dayNames[schedule.days[i] + 6].toLowerCase());
    }
    schedule.days = ret;
  }

  var scheduleView = $.JST.createFromTemplate(schedule, "SCHEDULE_" + (schedule.type.toUpperCase()));
  view.find("#schedulePlace").append(scheduleView);
  $("#eventViewPlace").empty().append(view);
}


function viewEvent(row) {
  goToPage('eventView',{eventId:row.attr("eventId")});
}

function eventEditorEnter(event, data, fromPage, isBack){
  var page=$(this);
  page.find("#eventEditorPlace").empty().append($.JST.createFromTemplate({"millis":data.millis,date:currentDate}, "EVENT_EDITOR"))
}

function addAgendaEvent(el) {
  goToPage("eventEditor",{millis:el.attr("millis")});
}


function saveAgendaEvent(el) {

  var ed = $("#eventEditorPlace");

  if (ed.find(":input[required=true]").isFullfilled()) {

    var start = parseInt(ed.find("select.startMillis").val());
    var dur = millisFromHourMinute(ed.find("select.duration").val());
    var filter = {
      "CM": "SAVEEVENT",
      "SCHEDULE":JSON.stringify({type:"period",startMillis:start,endMillis:start+dur}),
      "AGENDA_SUMMARY":ed.find("input.subject").val(),
      "IS_UNAVAILABLE":ed.find("input.unavailable").get(0).checked ? "yes" : "no",
      "IS_PERSONAL":ed.find("input.personal").get(0).checked ? "yes" : "no",
      "IS_REMINDER":ed.find("input.reminder").get(0).checked ? "yes" : "no",
      "AGENDA_DESCRIPTION":ed.find("textarea.description").val(),
      "LOCATION":ed.find("input.location").val()
    };



    callController(filter, function(response) {
      //update cache
      var agEv = response.event;
      if (agEv) {

        var dateInt = new Date(agEv.startMillis).toInt();
        var monthEvents = applicationCache.events[parseInt(dateInt / 100)];
        if (monthEvents) {
          monthEvents.push(agEv);
        }
      }

      backPage();
      //goToPage("calendarDayList");
      //goToDayAndList(currentDate);

    });
  }
}

function deleteAgendaEvent(el){
  el.confirm(function() {
    var evid = currentPage.find("div[eventId]").attr("eventId")
    var filter = {"CM": "DLAGEEVENT", "EVID": evid};
    callController(filter, function (response) {
      //update cache
      var dateInt = currentDate.toInt();
      var monthEvents = applicationCache.events[parseInt(dateInt / 100)];
      if (monthEvents) {
        for (var i in monthEvents) {
          if (monthEvents[i].id == evid) {
            monthEvents.splice(i, 1);
            break;
          }
        }
      }
      backPage();
    });
  });
}



//the calendar use the daysWithStrip var to highlight cells
$("#calendarPlace").calendarPicker({
  useWheel:false,
  callback:function(theDiv) {
    loadCalendar(theDiv.data("options").currentDate);
  },
  dayClick:function(theDiv) {
    currentDate=theDiv.data("options").currentDate;
    goToPage("calendarDayList");
  },
  fullMonth:true,
  firstDayOfWeek:Date.firstDayOfWeek,
  dayRenderer:function(el, date) {
    if (jQuery.inArray(date.toInt(), applicationCache.eventsDays) >= 0)
      el.addClass("dayWithStrip");
    else
      el.removeClass("dayWithStrip");

    if (date.isHoliday())
      el.addClass("holy");
    else
      el.removeClass("holy");
  },

  dayHeaderRenderer:function(el, day) {
    var d=new Date();
    d.getD
    if (applicationCache.eventsHolidays && applicationCache.eventsHolidays.week.indexOf(day) >= 0)
      el.addClass("holy");
  }

});

function getEventById(eventId) {
  for (var m in applicationCache.events) {
    var evs=applicationCache.events[m];
    for (var i=0;i<evs.length;i++) {
      if (evs[i].id == eventId)
        return evs[i];
    }
  }
  return false;
}


</script>



