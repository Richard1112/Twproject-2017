<%@ page import="com.twproject.agenda.businessLogic.AgendaController, com.twproject.resource.Person, com.twproject.resource.Resource, com.twproject.resource.ResourceBricks, com.twproject.waf.TeamworkHBFScreen,
net.sf.json.JSONArray, net.sf.json.JSONObject, org.jblooming.operator.Operator, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea,
 org.jblooming.waf.constants.Fields, org.jblooming.waf.html.core.JST, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed,
  org.jblooming.waf.view.PageState, java.util.List" %><%

  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new AgendaController(), request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);
  } else {
    Operator logged = pageState.getLoggedOperator();
    Person loggedPerson = Person.getLoggedPerson(pageState);

    long focusMillis = pageState.getEntry("FOCUS_MILLIS").longValueNoErrorNoCatchedExc();
    focusMillis = focusMillis == 0 ? System.currentTimeMillis() : focusMillis;
    pageState.addClientEntry("FOCUS_MILLIS", focusMillis);

    String agendaType = pageState.getEntry("AGENDA_TYPE").stringValueNullIfEmpty();
    agendaType = agendaType != null ? agendaType : "WEEK";
    pageState.sessionState.setAttribute("AGENDA_TYPE", agendaType);


    PageSeed ps = pageState.thisPage(request);
    ps.addClientEntry("FOCUS_MILLIS", focusMillis);
    ps.addClientEntry("AGENDA_TYPE", agendaType);

    List<Resource> ress = ResourceBricks.fillWorkGroup(pageState);
    JSONArray pickedResources = new JSONArray();
    for (Resource res : ress) {
      JSONObject jres = new JSONObject();
      jres.element("resId", res.getId());
      jres.element("resName", res.getDisplayName());
      jres.element("resAvatarUrl", res.bricks.getAvatarImageUrl());
      pickedResources.add(jres);
    }

    ps.addClientEntry("WG_IDS", JSP.w(pageState.getEntry("WG_IDS").stringValueNullIfEmpty()));

    /*SerializedList<Permission> permissions = new SerializedList();
    permissions.add(TeamworkPermissions.resource_canRead);
    ps.addClientEntry("PERM_REQUIRED", permissions);
    */

    String filter = pageState.getEntry("FILTER").stringValueNullIfEmpty();
    if (filter == null)
      pageState.addClientEntry("FILTER", "NONE");


    Form form = new Form(ps);
    form.id = "AGWEDA";
    pageState.setForm(form);



%><div class="mainColumn">
  <%form.start(pageContext);%>

  <jsp:include page="partAgendaHeader.jsp"/><%

  form.end(pageContext);
%>
  <%-------------------------------------------------------------------------  MAIN CALENDAR START ----------------------------------------------------------------------------%>

  <div style="position:relative;" id="offScreenAlertBox">

    <div style="width:100%;" id="calendarContainer">
      <%if ("WEEK".equals(agendaType)) {%>
      <div style="width:100%;height:28px; z-index: 2" id="calendarHeader"></div>
      <%} else {%>
      <div class="taskIssueBox day"></div>
      <%} %>
      <div id="spanEvents" style="position: relative; min-height: 30px;"></div>
    </div>
    <div style="<%="DAY".equals(agendaType)?"top:0;":""%>" class="offTop offAlert">scroll up <span class="teamworkIcon" style="font-size: 100%; color:#fff">k</span></div>
    <div style="bottom:0;" class="offBottom offAlert">scroll down <span class="teamworkIcon" style="font-size: 100%; color:#fff">j</span></div>
  </div>
  <%-------------------------------------------------------------------------  MAIN CALENDAR END ----------------------------------------------------------------------------% >


  < %-------------------------------------------------------------------------  TEMPLATES START ----------------------------------------------------------------------------%>
  <div class="agendaTemplates" style="display:none">
    <%=JST.start("AGENDAEVENT")%>
    <div
        class='agendaEvent (#=!obj.icalId.startsWith("TW_")?"external":""#)(#=obj.isReminder?" isReminder":""#)(#=obj.isMeeting?" isMeeting":""#)(#=obj.isPersonal?" isPersonal":""#)(#=obj.isUnavailability?" isUnavailability":""#)'
        evId="(#=obj.id#)">
      <div class="eventBody">
        <span title="(#=obj.summary#)" class="hours">(#=obj.isReminder?"<span class='teamworkIcon'>b</span>":""#)(#=obj.isPersonal?"<span class='teamworkIcon'>o</span>":""#)(#=obj.schedule.type!="period"?"<span class='teamworkIcon'>h</span>":""#) (#=printPeriod(obj.startMillis,obj.endMillis)#)</span>

        <div class="location" style='(#=!obj.location?"display:none":""#)'>(#=obj.location#)</div>
        <div class="summary (#=!obj.location?"firstElement":""#)"><span class="eventAttendees"></span>(#=obj.summary#)</div>
        <div class="description linkEnabled">(#=obj.description#)</div>
      </div>
    </div>
    <%=JST.end()%>

    <%=JST.start("AGENDAEVENTCOMPACT")%>
    <div
        class='agendaEvent (#=!obj.icalId.startsWith("TW_")?"external":""#)(#=obj.isReminder?" isReminder":""#)(#=obj.isMeeting?" isMeeting":""#)(#=obj.isPersonal?" isPersonal":""#)(#=obj.isUnavailability?" isUnavailability":""#) compact'
        evId="(#=obj.id#)" title="(#=new Date(obj.startMillis).format('HH:mm')#) - (#=obj.summary#)">
      <div class="eventBody">
        <div class="eventAttendees"></div>
        <span class="hours">(#=new Date(obj.startMillis).format("HH:mm")#)</span>&nbsp;-
        (#=obj.summary#)

      </div>
    </div>
    <%=JST.end()%>

    <%=JST.start("AGENDAEVENTSPAN")%>
    <div class='agendaEventSpan (#=!obj.icalId.startsWith("TW_")?"external":""#)(#=obj.isReminder?" isReminder":""#)(#=obj.isMeeting?" isMeeting":""#)(#=obj.isPersonal?" isPersonal":""#)(#=obj.isUnavailability?" isUnavailability":""#)' evId="(#=obj.id#)">
      <div class="eventBody">
        <span title="(#=obj.summary#)" class="hours">(#=new Date(obj.startMillis).format("HH:mm")=="00:00"?"":"("+(new Date(obj.startMillis).format("HH:mm"))+")"#)</span>
        <span class="summary"><span class="eventAttendees"></span>(#=obj.summary#)</span>
      </div>
    </div>
    <%=JST.end()%>

    <%=JST.start("MICROEDIT")%>
    <div class='microEdit' evId="(#=obj.id#)" startMillis="(#=obj.startMillis#)" endMillis="(#=obj.endMillis#)">
      <div style="float:right"><span class="teamworkIcon meClose" title="<%=I18n.get("CLOSE")%>" onclick="closeMicroEditor($(this));" style="cursor: pointer; font-size: 15px">x</span></div>
      <span class="hours">(#=printEditorPeriod(obj)#)</span><br>
      <%--(#if(obj.id==-1){#)&nbsp;<input type="checkbox" name="isFullDay" onclick="clickIsFullDay($(this));"><%=I18n.get("ALL_DAY")%>(#}#)--%>
      <input class="formElements formElementsMedium" name="summary" required="true" type="text" style="width:100%;" value="(#=obj.summary#)" placeholder="<%=I18n.get("AGENDA_SUMMARY")%>">
      <input class="formElements formElementsMedium" name="location" type="text" style="width:100%;" value="(#=obj.location#)" placeholder="<%=I18n.get("LOCATION")%>">
      <textarea class="formElements formElementsMedium" name="description" rows="5" cols="5" style="width:100%;min-height:100px;" placeholder="<%=I18n.get("AGENDA_DESCRIPTION")%>">(#=obj.description#)</textarea>

      <div style="text-align:left;padding: 2px 0" class="textSmall">
        <input type="checkbox" (#=obj.isPersonal?"checked='yes'":""#)" name="isPersonal"><%=I18n.get("IS_PERSONAL")%>&nbsp;
        <input type="checkbox" (#=obj.isUnavailability?"checked='yes'":""#)" name="isUnavailable"><%=I18n.get("IS_UNAVAILABLE")%>&nbsp;
        <input type="checkbox" (#=obj.isReminder?"checked='yes'":""#)" name="isReminder"><%=I18n.get("IS_REMINDER")%>&nbsp;
      </div>
      <div style="text-align:left;padding: 10px 4px 0">
        <span class="button first" onclick="ajaxMicroSaveEvent($(this));"
              style="cursor: pointer;"><%=I18n.get("SAVE")%></span>
        <span class="button" onclick="fullEditEvent($(this));" style="cursor: pointer"><%=I18n.get("EDIT")%></span>
        (#if(obj.id!="-1"){#)
        <span class="button delete" onclick="ajaxDeleteEvent($(this));"
              style="cursor: pointer"><%=I18n.get("DELETE")%></span>
        &nbsp;&nbsp;
        (#}#)

      </div>
    </div>
    <%=JST.end()%>

    <%=JST.start("TASKBOX")%>
    <div class="tskIssBox linkEnabled" taskId="(#=obj.id#)" title="(#=obj.name#)">
      (#for (var g=0;g
      <obj.resources.length
      ;g++){#)
      <img src="(#=getResourceById(obj.resources[g]).resAvatarUrl#)"
           title="(#=getResourceById(obj.resources[g]).resName#)" align='absmiddle'>
      (#}#)
      <span class="teamworkIcon">t</span> T#(#=obj.mnemo#)# <span class="wl">(#=obj.msg#)</span>
    </div>
    <%=JST.end()%>
    <%=JST.start("ISSUEBOX")%>
    <div class="tskIssBox" resId="(#=obj[1]#)"
         title="<%=I18n.get("ISSUES_OF_THE_DAY")%>: (#=obj[2]#) (#=obj[3]>0?'- <%=I18n.get("ESTIMATED_WKL_REQ")%>'+getMillisInHoursMinutes(obj[3]):''#)">
      <img src="(#=getResourceById(obj[1]).resAvatarUrl#)" title="(#=getResourceById(obj[1]).resName#)"
           align='absmiddle'>
      (#=obj[2]#)<span class="teamworkIcon">i</span> <span
        class="wl">(#=obj[3]>0?getMillisInHoursMinutes(obj[3]):''#)</span>
    </div>
    <%=JST.end()%>

  </div>
  <%-------------------------------------------------------------------------  TEMPLATES END ----------------------------------------------------------------------------%>

  <div class="bottomBar" id="moveBar" onclick="moveBarClick($(this),event);"></div>

</div>

<script type="text/javascript">
var focusMillis =<%=focusMillis%>;
var showTaskIssues =<%=pageState.getEntryOrDefault("SHOW_TASK_ISSUES", Fields.FALSE).checkFieldValue()%>;
var minStep = 15 * 60000; //grid step
var firstMillisOfPeriod = 0;
var attendees =<%=pickedResources%>; // list of attendees involved in this period, used for legenda
var agendaType = "<%=agendaType%>";
var oneDayWidth = agendaType == "DAY" ? 700 : 100;

$(function () {
  //console.debug("startUp");
  startUp();
});

var folio;
var spanfolio;
var headFolio;

function startUp() {
  $(".agendaTemplates").loadTemplates().remove();
  //some decorators
  var eveDec = function (domEv, ev) {
    var lege = domEv.find(".eventAttendees");
    if (ev.targets.length > 1 || ev.targets[0].resId !=<%=loggedPerson.getId()%>) {
      for (var i = 0; i < ev.targets.length; i++) {
        var targ = ev.targets[i];
        var el = $("<img>").prop("src", targ.resAvatarUrl).addClass("face smaller").prop("title", targ.resName);
        el.on("dragstart", function (e) {return false});
        lege.append(el);
      }
    } else {
      lege.remove();
    }

    domEv.find(".linkEnabled").activateTeamworkLinks(true).emoticonize();
    if (!ev.icalId.startsWith("TW_"))
      domEv.prop("title", "<%=I18n.get("EXTERNAL_MANAGED_EVENT")%>")

  };
  $.JST.loadDecorator("AGENDAEVENT", eveDec);
  $.JST.loadDecorator("AGENDAEVENTCOMPACT", eveDec);
  $.JST.loadDecorator("AGENDAEVENTSPAN", eveDec);

  folio = createFolio();

  var spanEvents = $("#spanEvents").height(15);
  spanFolio = new Folio(spanEvents);

  $(window).resize(winResize).resize();

  //scroll to the working day start
  folio.scrollTop(new Date().getTime() - new Date().clearTime().getTime() - folio.height / 4);

  bindEventCreation(folio);
  bindSpanEventCreation(spanFolio);
  goToMillis(focusMillis);
}


function winResize() {
  //Set the height of the calendar area on window resize
  var calendarContainer = folio.element.parent();
  var fh = $(window).height() - calendarContainer.offset().top - 70 - $("#spanEvents").height();
  fh = fh < 200 ? 200 : fh;
  folio.resize({height: fh});
  spanFolio.resize();
  if(headFolio) headFolio.resize();

  $(".offAlert.offTop, .taskIssueBox").css({top:$("#spanEvents").height()+27})
}

function createFolio() {
  //create folio div
  var divFolio = $("<div>").prop("id", "calendar").addClass("calendar").css({width: "100%", height: 800, "z-index": 1});

  var container = $("#calendarContainer");
  container.append(divFolio);
  var folio = new Folio(divFolio);
  folio.inPercent = true;

  folio.createScrollbar(container);

  //set virtual dimensions
  folio.width = 720;
  folio.height = 14 * 3600000; // altezza in ore

  //draw day separators
  if (agendaType == "WEEK") {
    var date = new Date(focusMillis);
    date.clearTime();
    date.setFirstDayOfThisWeek();

    for (var i = 0; i < 7; i++) {
      var daySep = $("<div>").addClass("day").attr("day", date.getDay());
      folio.addElement(daySep, 0, 20 + i * 100, 100, 24 * 3600000);
      date.add("d", 1);
    }
  }

  //draw hours
  for (var i = 0; i < 24 * 3600000; i += 3600000) {
    var hSep = $("<div>").addClass("hourSep").html(getMillisInHoursMinutes(i));
    folio.addElement(hSep, i, 0, folio.width, 3600000);
  }

  folio.redraw();

  //bind check offScreen elements when scroll and
  divFolio.scroll(function () {
    $(this).stopTime("ckoffscrelmnt").oneTime(500, "ckoffscrelmnt", checkOffScreenElements);
  });

  return folio;
}


function deskRefill() {
  var date = new Date(focusMillis);
  date.clearTime();

  var now = new Date();
  $("#nowBar").remove();
  var nowBar = $("<div>").addClass("now").attr({id: "nowBar", title: "<%=I18n.get("NOW")%>"});

  if (agendaType == "WEEK") {
    date.setFirstDayOfThisWeek();
    drawWeekCalendaHeader(date.getTime());
    if (now.format("yyyyww") == date.format("yyyyww")) { // solo se è la stessa settimana
      folio.addElement(nowBar, now.getTime() - new Date().clearTime().getTime(), 20 + (now.getDay() - Date.firstDayOfWeek) * 100, 100, folio.getPixelHeight());
    }
  } else {
    if (now.format("yyyyMMdd") == date.format("yyyyMMdd")) { // solo se è lo stesso giorno
      folio.addElement(nowBar, new Date().getTime() - new Date().clearTime().getTime(), 0, folio.width, folio.getPixelHeight());
    }
  }

  firstMillisOfPeriod = date.getTime();
  ajaxGetEvents();

  if (showTaskIssues)
    ajaxGetTaskIssues();
  drawMoveBar();
}


function drawWeekCalendaHeader(firstMillisOfWeek) {
  var calHead = $("#calendarHeader");
  calHead.empty();
  headFolio = new Folio(calHead);
  headFolio.inPercent = true;
  headFolio.width = 720;
  headFolio.height = 50;
  var date = new Date(firstMillisOfWeek);
  var headLbl = $("<div>").addClass("dayHeader").html("&nbsp;");
  headFolio.addElement(headLbl, 0, 0, 20, 50);

  //clear holiday & today class
  var calendar = $("#calendar");
  calendar.find(".dayH,.dayT").removeClass("dayH dayT");
  for (var i = 0; i < 7; i++) {
    var headDay = $("<div>").addClass("dayHeader").css("cursor", "pointer").html(date.format("EE d MMM")).attr({"day": date.getDay(), "millis": date.getTime()});
    headFolio.addElement(headDay, 0, 20 + i * 100, 100, 50);
    if (date.isHoliday()) {
      headDay.addClass("dayHHeader");
      calendar.find("[day=" + date.getDay() + "]").addClass("dayH");
    }
    if (date.isToday()) {
      headDay.addClass("dayTHeader");
      calendar.find("[day=" + date.getDay() + "]").addClass("dayT");
    }
    headDay.click(goToDayView);
    headDay.append("<div class='taskIssueBox'></div>");
    date.add("d", 1);
  }
  headFolio.redraw();
}


function goToDayView() {
  var headDay = $(this);
  var form = $("#AGWEDA");
  form.find("[name=AGENDA_TYPE]").val("DAY");
  form.find("[name=FOCUS_MILLIS]").val(headDay.attr("millis"));
  form.submit();
}

function bindEventsOnEvent(evDom, ev, folio) {
  //bring in front when over
  evDom.mouseenter(function () {
    if(!$("body").is(".unselectable"))
      $(this).oneTime(400, "toFront", function () {$(this).css("z-index", 2000)}).stopTime("toBack");
  }).mouseleave(function () {
    if(!$("body").is(".unselectable"))
      $(this).oneTime(100, "toBack", function () {$(this).css("z-index", 0)}).stopTime("toFront");
  });

  //move and resize
  if (ev.canManage) {
    var focusedElement;
    var dragStarted = false;
    var dragHappened = false; //horrible hack to discriminate click on event from a click as consequence of drag

    var containment = folio.element;
    evDom.on("mousemove", function (e) {

      if (!focusedElement) {
        var element = $(this);
        var mousePos = e.pageY - element.offset().top;
        if (element.height() - mousePos < 15) {
          $("body").addClass("EVResize");
        } else {
          $("body").removeClass("EVResize");
        }
      }

    }).on("mouseout", function (e) {
      if (!focusedElement) {
        $("body").removeClass("EVResize");
      }

    }).on("mousedown", function (e) {
      if (e.which == 1) {
        e.stopPropagation();
        e.stopImmediatePropagation();

        folio.folioCanScroll = false;

        focusedElement = $(this);
        var mousePos = e.pageY - focusedElement.offset().top;
        var offsetTop = containment.offset().top;
        var vGrid = folio.getScaleHeight() * minStep;

        $("body").unselectable();

        //delayed resize -> in order to discriminate click from drag-resize
        focusedElement.oneTime(300, "manageResize", function () {
          if (focusedElement.height() - mousePos < 15) {
            dragHappened = false;
            //bind event for start resizing
            $(document).on("mousemove", function (e) {
              //manage resizing
              if (e.pageY - offsetTop < containment.height()) {
                var h = (e.pageY - focusedElement.offset().top);
                //grid
                h = (parseInt(h / vGrid)) * vGrid;
                h = h <= 0 ? vGrid : h;

                focusedElement.height(h);

                //upgrade hours during resize
                focusedElement.find(".hours").html(getMillisInHoursMinutes(Math.round(folio.getPixelHeight() * h)));
              }

              //bind mouse up on body to stop resizing
            }).on("mouseup", function (e) {
              //console.debug("stop resizing");
              $(this).off("mousemove").off("mouseup");
              $("body").removeClass("EVResize").clearUnselectable();

              folio.folioCanScroll = true;

              //upgrade hours when resize stops
              ajaxMoveResizeEvent(focusedElement, folio);
              focusedElement = undefined;
            });

            // drag
          } else {
            var elTop = parseInt(focusedElement.css("top"));
            var elLeft = parseInt(focusedElement.css("left"));
            var startY = e.pageY;
            var startX = e.pageX;
            $("body").addClass("EVDrag").unselectable();
            var $calendar = $("#calendar");
            $calendar.on("mousemove", function (e) {

              //remove current editor if any
              $(".microEdit .meClose").click();

              if (dragStarted || Math.abs(e.pageY - startY) > 10 || Math.abs(e.pageX - startX) > 50) {

                folio.folioCanScroll = false;

                dragStarted = true;
                var t = elTop + (e.pageY - startY);
                var l = elLeft + (e.pageX - startX);

                if (isSafari) {
                  t = elTop + (e.pageY + $calendar.scrollTop() - startY);
                  l = elLeft + (e.pageX + $calendar.scrollLeft() - startX);
                }

                if (t > 0 && (t + focusedElement.height() < containment.height() + containment.scrollTop())) {
                  //grid
                  t = (parseInt(t / vGrid)) * vGrid;
                  focusedElement.css("top", t).addClass("dragging");


                  //upgrade label
                  var newEv = getNewHours(focusedElement, folio);
                  focusedElement.find(".hours").html(printPeriod(newEv.start, newEv.end));

                }

                var originalEvent=focusedElement.data("originalEvent");
                //change day
                if (originalEvent.schedule.type=="period" && l > 0 && (l + focusedElement.width() < containment.width() + containment.scrollLeft())) {
                  //grid
                  l = folio.getScaleWidth() * 20 + Math.round((l - folio.getScaleWidth() * 20) / (folio.getScaleWidth() * 100)) * (folio.getScaleWidth() * 100);
                  focusedElement.css("left", l);
                }

              }

              focusedElement.css("z-index", 4000);


              //bind mouse up on body to stop resizing
            }).on("mouseup", function (e) {
              //console.debug("stop dragging");
              $(this).off("mousemove").off("mouseup");
              $("body").removeClass("EVDrag").clearUnselectable();
              dragStarted = false;
              ajaxMoveResizeEvent(focusedElement, folio);
              focusedElement.removeClass("dragging");
              focusedElement = undefined;
              dragHappened = true;
              folio.folioCanScroll = true;

            });
          }

        });
      }
      //quick edit event
    }).on("click",function (e) {
      folio.folioCanScroll = true;
      if (e.which == 1) {
        //remove delayed manage resize -> if you click you are not dragging
        var domEv = $(this);
        domEv.stopTime("manageResize");
        if (!dragHappened) {
          domEv.oneTime(300, "shmeaaw", function () {
            var ev = $(this).data("originalEvent");
            microEdit(ev, folio, $(this), e);
          });
        }
      }
      $("body").clearUnselectable();

    }).on("mouseup", function () {

      folio.folioCanScroll = true;

      $("body").unselectable();
    }).dblclick(function () {
      window.location.href = "agendaEditor.jsp?CM=ED&OBJID=" + $(this).attr("evid");
    });
  } else if (ev.isInvolved) { // in this case you can remove yourself -> go stright to the full edit
    evDom.mousedown(function (e) {
      e.stopImmediatePropagation();
      e.stopPropagation();
      fullEditEvent($(this));
      return false;
    });
  }
}

function bindEventsOnSpanEvent(evDom, ev, folio) {
  //move and resize
  if (ev.canManage) {
    evDom.click(function (e) {
      //console.debug("click, dragHappened:"+dragHappened)
      if (e.which == 1) {
        //remove delayed manage resize -> if you click you are not dragging
        var domEv = $(this);
        domEv.oneTime(300, "shmeaaw", function () {
          microEdit(ev, folio, $(this), e);
        });
      }
    }).dblclick(function () {
      window.location.href = "agendaEditor.jsp?CM=ED&OBJID=" + $(this).attr("evid");
    }).mousedown(function () {
      $("body").unselectable();
    }).mouseup(function () {
      $("body").clearUnselectable();
    });
  } else if (ev.isInvolved) { // in this case you can remove yourself -> go stright to the full edit
    evDom.mousedown(function (e) {
      e.stopImmediatePropagation();
      e.stopPropagation();
      fullEditEvent($(this));
      return false;
    });
  }
}

function bindEventCreation(folio) {
  //console.debug("bindEventCreation");

  // Todo: Add touch events to make it works on tablets
  folio.element.on("mousedown.creat", function (e) {

    if (e.which!=1)
      return;

    $(".microEdit .meClose").click();

    //if not a creation is still pending and is not ctrl/left etc
//    if ($(".microEdit .meClose").size() <= 0) {
    $("body").unselectable();

    var startX, startY;
    startX = e.pageX;
    startY = e.pageY;

    //start creating event after few millis
    folio.element.oneTime(200, "createCreator", function (e) {

      //create a placeholder div
      var creator = $("<div>").addClass("eventCreator agendaEvent");
      folio.element.append(creator);

      //microEdit({id: "-1", startMillis: firstMillisOfPeriod  , endMillis: firstMillisOfPeriod + 10000 }, folio, creator, e, false);

      var offsX = folio.element.offset().left;
      var offsY = folio.element.offset().top - folio.element.scrollTop();

      var moveCreator = function (creator, e) {
        //set coordinates
        if (!startX)
          startX = e.pageX;
        if (!startY)
          startY = e.pageY;

        var vGrid = folio.getScaleHeight() * minStep;

        var newDim = {
          left  : Math.min(startX, e.pageX) - offsX,
          top   : Math.min(startY, e.pageY) - offsY,
          width : (folio.getScaleWidth() * oneDayWidth),
          height: Math.abs(startY - e.pageY)
        };

        //set in grid
        newDim.left = folio.getScaleWidth() * 20 + Math.floor((newDim.left - folio.getScaleWidth() * 20) / (folio.getScaleWidth() * oneDayWidth)) * (folio.getScaleWidth() * oneDayWidth);
        newDim.top = (parseInt(newDim.top / vGrid)) * vGrid;
        newDim.height = (parseInt(newDim.height / vGrid)) * vGrid;
        newDim.height = newDim.height < vGrid ? vGrid : newDim.height;

        var creatorHtml = $("<div/>").html((Math.round(folio.getPixelHeight() * newDim.height / 60000) + " <%=I18n.get("MINUTES_SHORT")%>"));
        creatorHtml.addClass("creatorHtml");

        creator.css(newDim).html(creatorHtml);

      };

      //bind move
      folio.element.on("mousemove.creat", function (e) {
        //remove current editor if any
        moveCreator(creator, e);
      });

      //bind mouseUp on document

      $("body").one("mouseup.creat", function (e) {

        //stop timer
        folio.element.stopTime("createCreator");

        //unbind move
        folio.element.off("mousemove.creat");

        var creator = folio.element.find(".eventCreator:first");
        moveCreator(creator, e);

        //create a micro editor
        if (creator.size() > 0) {
          //get real millis
          var start = Math.round((folio.getPixelHeight() * parseInt(creator.css("top"))) / minStep) * minStep;
          var dur = Math.round(creator.outerHeight() * folio.getPixelHeight() / minStep) * minStep;
          var day = 0;
          if (agendaType == "WEEK")
            day = Math.round((parseInt(creator.css("left")) - folio.getScaleWidth() * 20) / (folio.getScaleWidth() * 100));

          microEdit({id: "-1", startMillis: firstMillisOfPeriod + start + day * 24 * 3600000, endMillis: firstMillisOfPeriod + start + day * 24 * 3600000 + dur}, folio, creator, e, false);
        }
      });
    });
//    }

  }).on("click.creat", function (e) {
    $("body").clearUnselectable();
    //stop timer
    folio.element.stopTime("createCreator");
  });
}


function bindSpanEventCreation(spanFolio) {
  //console.debug("bindEventCreation");
  spanFolio.element.on("mousedown.creat", function (e) {

    if (e.which!=1)
      return;

    //remove current editor if any
    $(".microEdit .meClose").click();

    //if not a creation is still pending and is not ctrl/left etc
    if ($(".microEdit").size() <= 0) {
      $("body").unselectable();

      var startX, startY;
      startX = e.pageX;
      startY = e.pageY;

      //start creating event after few millis
      spanFolio.element.oneTime(200, "createCreator", function (e) {

        //create a placeholder div
        var creator = $("<div>").addClass("eventCreator agendaEvent");
        spanFolio.element.append(creator);

        var offsX = spanFolio.element.offset().left;
        var offsY = spanFolio.element.offset().top - spanFolio.element.scrollTop();

        var moveCreator = function (creator, e) {
          //set coordinates
          if (!startX)
            startX = e.pageX;
          if (!startY)
            startY = e.pageY;

          var vGrid = spanFolio.getScaleHeight() * minStep;

          var newDim = {
            left  : 0,
            top   : 0,
            width : 0,
            height: $(".agendaSpanEvents").height()
//            height: spanFolio.height * spanFolio.getScaleHeight()
          };

          //$("h1:first").html(newDim.width+ " "+folio.getScaleWidth() * oneDayWidth);


          //set in grid
          var start=startX-offsX;
          var end= e.pageX-offsX;

          if (e.pageX<startX) {
            var oldStart=start;
            start=end;
            end=oldStart;
          }
          var startCol = (Math.floor((start - spanFolio.getScaleWidth() * 20) / (spanFolio.getScaleWidth() * oneDayWidth)));
          var numCols = (Math.floor((end - spanFolio.getScaleWidth() * 20) / (spanFolio.getScaleWidth() * oneDayWidth)) + 1);
          newDim.left= spanFolio.getScaleWidth() * 20 + startCol * (spanFolio.getScaleWidth() * oneDayWidth);
          newDim.width = spanFolio.getScaleWidth() * 20 + numCols * (spanFolio.getScaleWidth() * oneDayWidth)-newDim.left;

          var stDate=new Date(firstMillisOfPeriod);
          stDate.setDate(stDate.getDate()+startCol);
          var enDate=new Date(firstMillisOfPeriod);
          enDate.setDate(enDate.getDate()+numCols-1);

          var creatorHtml = $("<div/>").html(stDate.format("EEEE")+(startCol!=numCols-1? " - "+enDate.format("EEEE"):"" ));
          creatorHtml.addClass("creatorHtml");
          creator.css(newDim).html(creatorHtml);
        };


        //bind move
        spanFolio.element.on("mousemove.creat", function (e) {
          moveCreator(creator, e);
        });

        //bind mouseUp on document
        $("body").one("mouseup.creat", function (e) {
          //console.debug("body mouseup.creat");
          //stop timer
          spanFolio.element.stopTime("createCreator");

          //unbind move\
          spanFolio.element.off("mousemove.creat");

          var creator = spanFolio.element.find(".eventCreator:first");
          moveCreator(creator, e);

          //create a micro editor
          if (creator.size() > 0) {
            //get real millis
            var start = Math.round((parseInt(creator.css("left"))*spanFolio.getPixelWidth()-spanFolio.getPixelWidth()*20)/oneDayWidth);
            var dur = Math.round(creator.outerWidth() * spanFolio.getPixelWidth() / oneDayWidth);
            var stDate=new Date(firstMillisOfPeriod);
            var enDate=new Date(firstMillisOfPeriod);
            stDate.setDate(stDate.getDate()+start);
            enDate.setDate(enDate.getDate()+start+dur);

            microEdit({id: "-1", startMillis: stDate.getTime(), endMillis: enDate.getTime()-1000}, spanFolio, creator, e);
            $(".microEdit :input[name=isFullDay]").hide();
          }
        });
      });
    }

  }).on("mouseup.creat", function (e) {
    $("body").clearUnselectable();
    //stop timer
    spanFolio.element.stopTime("createCreator");
  });
}


function createBarFolio() {
  var moveBar = $("#moveBar");
  moveBar.empty();

  var date = new Date(focusMillis);
  date.clearTime();
  date.setDate(1);
  date.setMonth(date.getMonth() - 12);

  var startMillis = date.getTime();
  date.setMonth(date.getMonth() + 24);
  var endMillis = date.getTime();


  var barFolio = new Folio(moveBar);
  barFolio.width = endMillis - startMillis;
  barFolio.height = 50;
  barFolio.left = startMillis;
  barFolio.inPercent = true;


  var d = new Date(startMillis);
  while (d.getTime() < endMillis) {
    var headLbl = $("<span>").html(d.format("MM yyyy")).addClass("moveBarEl");
    barFolio.addElement(headLbl, 0, d.getTime(), 30 * 24 * 3600000, 50);
    d.setMonth(d.getMonth() + 1);
  }

  //today
  var today = $("<span>").prop("title", "<%=I18n.get("TODAY")%>").addClass("moveBarToday");
  barFolio.addElement(today, 0, new Date().getTime(), barFolio.getPixelWidth() * 2, 50);

  return barFolio;
}


function checkOffScreenElements() {
  var divFolio = $(this);
  var offTop = false;
  var offBottom = false;
  var h = divFolio.height();
  divFolio.find(".agendaEvent").each(function () {
    var el = $(this);
    if (el.position().top < 0) {
      offTop = true;
      return false;
    }
  }).each(function () {
    var el = $(this);
    if (el.position().top > h) {
      offBottom = true;
      return false;
    }
  });
  var offScreenAlertBox = $("#offScreenAlertBox");
  if (offTop)
    offScreenAlertBox.addClass("offTop");
  else
    offScreenAlertBox.removeClass("offTop");

  if (offBottom)
    offScreenAlertBox.addClass("offBottom");
  else
    offScreenAlertBox.removeClass("offBottom");
}


function goToMillis(newMillis) {
  //console.debug("goToMillis"+ (new Date(newMillis)).format());

  focusMillis = newMillis;

  $("[name=FOCUS_MILLIS]").val(focusMillis); // in order to add in the right time

  $("#topHeaderCentral").html(getTopCentralHeader(focusMillis));
  deskRefill();
}


function printEditorPeriod(ev) {
  //console.debug(ev)
  var st= new Date(ev.startMillis);
  var en= new Date(ev.endMillis);
  if (ev.schedule && ev.schedule.type=="period" ){
    st= new Date(ev.schedule.startMillis);
    en= new Date(ev.schedule.endMillis);
  }

  if (en.getTime() - st.getTime() < 3600000 * 23.8) {
    return st.format("EEEE")+"<span> "+ st.format("HH:mm") + " - " + en.format("HH:mm")+"</span>"
  } else if (st.getDay() == en.getDay()) {
    return st.format("EEEE dd");
  } else if (st.getMonth() == en.getMonth()) {
    return st.format("EEE dd") + " - " + en.format("EEE dd")
  } else {
    return st.format("EEE dd MMMM") + " - " + en.format("EEE dd MMMM")
  }
}


function printPeriod(startMillis, endMillis) {
  return new Date(startMillis).format("HH:mm") + " - " + new Date(endMillis).format("HH:mm");
}

function clickIsFullDay(el){
  if (el.is(":checked"))
    el.closest(".microEdit").find(".hours span").hide();
  else
    el.closest(".microEdit").find(".hours span").show();
}

function getNewHours(focusedElement, folio) {
  var day = Math.round((parseInt(focusedElement.css("left")) * folio.getPixelWidth() -20 )/100);
  var startDay = new Date(firstMillisOfPeriod);
  startDay.setDate(startDay.getDate()+day);
  var newStart = parseInt(focusedElement.css("top")) * folio.getPixelHeight() + folio.top;
  newStart = Math.round(newStart / minStep) * minStep + startDay.getTime();
  var newDur = Math.round((focusedElement.height() * folio.getPixelHeight()) / minStep) * minStep;
  return {start: newStart, duration: newDur, end: newStart + newDur};
}


function moveBarClick(el, event) {
  var pos = event.clientX - el.offset().left;
  var folio = el.data("folio");
  var millis = parseInt(folio.getVirtualLeft(pos));
  goToMillis(millis);
}

function microEdit(jsonData, folio, creator, event, oncursor) {
  //console.debug("microEdit",jsonData, folio, creator)

  if(typeof oncursor == "undefined")
    oncursor = true;

  //remove current editor if any
  $(".microEdit .meClose").click();

  folio.folioCanScroll = true;

  var miced = $.JST.createFromTemplate(jsonData, "MICROEDIT");

  var balloonOpener = creator.showBalloon(event, {balloon: miced, oncursor: oncursor}, true);

  var balloon = balloonOpener.getBalloon();
  if(balloon.length)
    balloon.on("closeBalloon", function(){
      $(".eventCreator").remove();
    });

  var ed = balloon.find(".microEdit");
  ed.data("balloonOpener", balloonOpener);

  if (creator.hasClass("eventCreator"))
    ed.data("creator", creator);

  ed.find("input:first").focus().select();

  //bind one click on folio to close editor
  folio.element.oneTime(100, "bctcaup", function () {
    folio.element.one("click.closeMicEd", function () {
      $(".microEdit .meClose").click();
    });
  }).off("click.closeMicEd");
}

function closeMicroEditor(el) {
  var miced = el.closest(".microEdit");
  var opener = miced.data("creator");

  var balloon = el.closest(".mbBalloon");
  var target = balloon.get(0).opener;

  $(target).hideBalloon();
}

function fullEditEvent(el) {
  var editor = el.closest("[evId]");
  var evId = editor.attr("evId");

  var request = getDefaultRequest();

  if (evId != "-1") {
    $.extend(request, {
      CM   : "ED",
      OBJID: evId
    });
  } else {
    $.extend(request, {
      CM         : "AD",
      OBJID      : evId,
      SCHEDULE   : JSON.stringify({type: "period", startMillis: editor.attr("startMillis"), endMillis: editor.attr("endMillis")}),
      summary    : editor.find("[name=summary]").val(),
      description: editor.find("[name=description]").val(),
      location   : editor.find("[name=location]").val(),
      IS_PERSONAL   : editor.find("[name=isPersonal]").is(":checked") ? "yes" : "no",
      IS_UNAVAILABLE: editor.find("[name=isUnavailable]").is(":checked") ? "yes" : "no",
      IS_REMINDER   : editor.find("[name=isReminder]").is(":checked") ? "yes" : "no"

    });

  }
  //console.debug($.param(request))
  window.location.href = "agendaEditor.jsp?" + $.param(request);
}


function refillWorkgroup() {
  var legenda = $("#workgroupLegenda").empty();
  for (var i = 0; i < attendees.length; i++) {
    legenda.append($.JST.createFromTemplate(attendees[i], "WORKGROUPELEMENT"));
  }
}


function getDefaultRequest() {
  return {
    type         : agendaType,
    focusMillis  : focusMillis,
    FILTER       : "<%=filter%>",
    WG_IDS       : "<%=JSP.w(pageState.getEntry("WG_IDS").stringValueNullIfEmpty())%>",
    PERM_REQUIRED: "<%=JSP.w(pageState.getEntry("PERM_REQUIRED").stringValueNullIfEmpty())%>"
  };
}


function ajaxMicroSaveEvent(el) {
  var editor = el.closest(".microEdit");
  if (editor.find(":input").isFullfilled()) {
    var request = getDefaultRequest();
    $.extend(request, {
      CM           : "MICSAVE",
      id           : editor.attr("evid"),
      summary      : editor.find("[name=summary]").val(),
      location     : editor.find("[name=location]").val(),
      description  : editor.find("[name=description]").val(),
      isPersonal   : editor.find("[name=isPersonal]").is(":checked") ? "yes" : "no",
      isUnavailable: editor.find("[name=isUnavailable]").is(":checked") ? "yes" : "no",
      isReminder   : editor.find("[name=isReminder]").is(":checked") ? "yes" : "no",
      isFullDay    : editor.find("[name=isFullDay]").is(":checked") ? "yes" : "no",
      startMillis  : editor.attr("startMillis"),
      endMillis    : editor.attr("endMillis")
    });
    showSavingMessage();

    $.getJSON("agendaAjaxController.jsp", request, function (response) {
      jsonResponseHandling(response);
      if (response.ok == true) {
        //remove editor
        closeMicroEditor(el);

        //clear all old events;
        drawEvents(response.events);
      }
      hideSavingMessage();
    });
  }
}

function ajaxMoveResizeEvent(focusedElement, folio) {
  var newEv = getNewHours(focusedElement, folio);
  var oldEv = focusedElement.data("originalEvent");

  //check if something changed
  if (oldEv.startMillis != newEv.start || oldEv.endMillis != newEv.end) {
    showSavingMessage();
    var request = getDefaultRequest();
    $.extend(request, {
      CM         : "MOVRESEV",
      id         : oldEv.id,
      startMillis: newEv.start,
      duration   : newEv.duration
    });

    $.getJSON("agendaAjaxController.jsp", request, function (response) {
      jsonResponseHandling(response);
      if (response.ok == true) {
        //clear all old events;
        drawEvents(response.events);
      }
      hideSavingMessage();
    });
  } else {
  }
}


function ajaxGetEvents() {
  //console.debug("ajaxGetEvents")
  var request = getDefaultRequest();
  $.extend(request, {
    CM: "GETEVENTS"
  });

  showSavingMessage();
  $.getJSON("agendaAjaxController.jsp", request, function (response) {
    jsonResponseHandling(response);
    if (response.ok == true) {
      refillWorkgroup();
      drawEvents(response.events);
    }
    hideSavingMessage();
  });
}


function ajaxGetTaskIssues() {
  var request = getDefaultRequest();
  $.extend(request, {
    CM: "GETTASKISS"
  });

  showSavingMessage();
  $.getJSON("agendaAjaxController.jsp", request, function (response) {
    jsonResponseHandling(response);
    if (response.ok == true) {
      drawTaskIssues(response);
    }
    hideSavingMessage();
  });
}

function ajaxDeleteEvent(el) {
  el.confirm(function () {
    var editor = el.closest(".microEdit");
    showSavingMessage();
    var request = getDefaultRequest();
    $.extend(request, {
      CM: "DELEV",
      id: editor.attr("evId"),
      startMillis:editor.attr("startMillis")
    });
    $.getJSON("agendaAjaxController.jsp", request, function (response) {
      jsonResponseHandling(response);
      if (response.ok) {

        closeMicroEditor(el);

        refillWorkgroup();
        drawEvents(response.events);
      }
      hideSavingMessage();
    });
  }, "<%=I18n.get("FLD_CONFIRM_DELETE")%>");
}


function drawMoveBar() {
  var barFolio = createBarFolio();
  //displayed period
  d = new Date(focusMillis);
  d.clearTime();
  if (agendaType == "DAY") {
    var st = new Date(d.getTime());
    d.add("d", 1);
  } else {
    d.setFirstDayOfThisWeek();
    var st = new Date(d.getTime());
    d.add("d", 6);
  }
  var highlight = $("<span>").addClass("moveBarHL");
  barFolio.addElement(highlight, 0, st.getTime(), d.getTime() - st.getTime(), 50);
  barFolio.redraw();
}


function getTopCentralHeader(millis) {
  var header;
  if (agendaType == "DAY") {
    header = new Date(millis).format("EEEE d MMMM yyyy") + "<sup>(" + new Date(millis).format("ww") + ")</sup>";
  } else {
    var date = new Date(millis);
    date.clearTime();
    date.setFirstDayOfThisWeek();
    var st = new Date(date.getTime());
    date.add("d", 6);
    header = st.format("dd" + (st.getMonth() != date.getMonth() ? " MMMM" : "")) + " - " + date.format("dd MMMM yyyy") + "<sup>(" + date.format("ww") + ")</sup>";
  }
  return header;
}


function drawTaskIssues(response) {
  var tasks = response.tasks;
  var issues = response.issues; //[[dayInt,resourceId,numOfIssues,totEstimated]]
  //clear
  $(".taskIssueBox").empty();

  //tasks
  if (tasks) {
    for (var i = 0; i < tasks.length; i++) {
      var data = tasks[i];

      var date = new Date(data.millis);
      var day = date.getDay();
      var taskIssueBox;
      if (agendaType == "WEEK") {
        taskIssueBox = $("[day=" + day + "] .taskIssueBox");
      } else {
        taskIssueBox = $(".taskIssueBox");
      }
      var ib = $.JST.createFromTemplate(data, "TASKBOX");
      ib.activateTeamworkLinks(false);
      taskIssueBox.append(ib);

    }
  }


  if (issues) {
    //console.debug(issues)
    for (var i = 0; i < issues.length; i++) {
      var data = issues[i];
      var date = Date.fromInt(data[0]);
      var day = date.getDay();
      var taskIssueBox;
      if (agendaType == "WEEK") {
        taskIssueBox = $("[day=" + day + "] .taskIssueBox");
      } else {
        taskIssueBox = $(".taskIssueBox");
      }
      var ib = $.JST.createFromTemplate(data, "ISSUEBOX");
      ib.attr("day", date.format())
      ib.click(function (ev) {
        ev.stopPropagation();
        ev.cancelBubble;
        var el = $(this);
        location.href = "../issue/issueList.jsp?CM=FN&FLT_ISSUE_ASSIGNED_TO=" + el.attr("resId") + "&FLT_ISSUE_DATE_CLOSE_BY=" + el.attr("day");

      });
      taskIssueBox.append(ib);
    }
  }
}


function drawEvents(events) {
//  console.debug("drawEvents",events)
  //sort events by start and endDate

  $("body").removeClass("EVResize");

  var spannedEvents=[];
  if (events) {
    // remove events spanning on more days
    var simpleEvents=[];
    for (var i = 0; i < events.length; i++) {
      var ev = events[i];
      if (!ev.trimmed && (ev.endMillis-ev.startMillis< 3600000*23) ){
        simpleEvents.push(ev);  // same day
      } else {
        spannedEvents.push(ev); //span day
      }
    }
    events=simpleEvents;


    events.sort(function (a, b) {
      if (a.startMillis < b.startMillis) {
        return -1;
      } else if (a.startMillis > b.startMillis) {
        return 1;
      } else {
        if (a.endMillis < b.endMillis) {
          return 1;
        } else if (a.endMillis > b.endMillis) {
          return -1;
        } else {
          return 0;
        }
      }
    });
  }

  //clear all old events;
  var calendar = $("#calendar");
  calendar.find(".agendaEvent").remove();
  var folio = calendar.data("folio");
  //remove current editor if any
  $(".microEdit .meClose").click();



  drawSpannedEvents(spannedEvents,agendaType);

  if (agendaType == "DAY") {
    drawOneColumnEventsOptimized(folio, oneDayWidth - 60, 40, events);

  } else { // WEEK
    // split event in days
    var ev4Days = {};
    for (var i = 0; i < events.length; i++) {
      var ev = events[i];
      var startDate = new Date(ev.startMillis);
      var day = (7 - Date.firstDayOfWeek + startDate.getDay()) % 7;  //column

      var evD = ev4Days[day];
      if (!evD)
        evD = [];

      evD.push(ev);
      ev4Days[day] = evD;
    }

    for (var day in ev4Days) {
      var evd = ev4Days[day];
      drawOneColumnEvents(folio, 100, 20 + day * 100, evd);
    }
  }
  winResize();
  folio.redraw();
  calendar.oneTime(500, "ckoffscrelmnt", checkOffScreenElements);
}


function drawSpannedEvents(spannedEvents,agendaType){

  var rowSize=0;
  if(agendaType == "DAY"){
    for (var i = 0; i < spannedEvents.length; i++) {
      var ev = spannedEvents[i];
      ev.startDay = (7 - Date.firstDayOfWeek + new Date(ev.startMillis).getDay()) % 7;  //column
      ev.endDay = (7 - Date.firstDayOfWeek + new Date(ev.endMillis).getDay()) % 7;  //column
      ev.row=i;
    }

    rowSize=spannedEvents.length;

  } else {
    var rows = [];

    for (var i = 0; i < spannedEvents.length; i++) {
      var ev = spannedEvents[i];
      ev.startDay = (7 - Date.firstDayOfWeek + new Date(ev.startMillis).getDay()) % 7;  //column
      ev.endDay = (7 - Date.firstDayOfWeek + new Date(ev.endMillis).getDay()) % 7;  //column

      var found = false;
      for (var r = 0; r < rows.length; r++) {
        var row = rows[r];
        var allFree = true;
        for (var c = ev.startDay; c <= ev.endDay; c++) {
          if (row[c] > 0) {
            allFree = false;
            break;
          }
        }
        if (allFree) {
          for (var c = ev.startDay; c <= ev.endDay; c++) {
            row[c] = 1;
          }
          found = true;
        }

        if (found) {
          ev.row = r;
          break;
        }
      }
      if (!found) {
        var row = [0, 0, 0, 0, 0, 0, 0];
        for (var c = ev.startDay; c <= ev.endDay; c++) {
          row[c] = 1;
        }
        rows.push(row);
        ev.row = rows.length - 1;

      }
    }
    rowSize=rows.length;
  }
  var rowH = 25;
  var spanEvents = spanFolio.element;
  spanEvents.height(rowH*rowSize);
  spanEvents.empty();
  spanFolio.width = 720;
  spanFolio.height = rowH * rowSize+15;
  spanFolio.inPercent = true;


  var bg = $("<div>").addClass("agendaSpanEvents");
  spanFolio.addElement(bg,0,20,700,spanFolio.height-5);

  for (var i = 0; i < spannedEvents.length; i++) {
    var ev = spannedEvents[i];
    var evDom = $.JST.createFromTemplate(ev, "AGENDAEVENTSPAN");

    evDom.data("originalEvent", ev);

    if(ev.schedule.startMillis<ev.startMillis)
      evDom.addClass("leftArrow");

    if(ev.schedule.endMillis>ev.endMillis)
      evDom.addClass("rightArrow");


    if (ev.isExternal)
      evDom.addClass("external");
    bindEventsOnSpanEvent(evDom, ev, folio);
    if(agendaType == "DAY") {
      spanFolio.addElement(evDom, ev.row * rowH, 20, 700, rowH);
    } else {
      spanFolio.addElement(evDom, ev.row * rowH, ev.startDay * 100 + 20, (ev.endDay - ev.startDay + 1) * 100, rowH);
    }
  }
  spanFolio.redraw();

}


function getOverlaps(ev, levels) {
  var over = 0;
  if (ev.level < levels.length - 1) {
    var levEv = levels[ev.level + 1];
    for (var i = 0; i < levEv.length; i++) {
      var ev2 = levEv[i];
      if (!((ev2.endMillis <= ev.startMillis) || (ev2.startMillis >= ev.endMillis))) {
        over = Math.max(getOverlaps(ev2, levels), over) + 1;
      }
    }
  }
  return over;
}


function drawOneColumnEventsOptimized(folio, colWidth, colOffset, events) {
  if (events) {

    //compute levels
    var levels = [];//è un array di eventi messi a strati come nel tetris
    for (var i = 0; i < events.length; i++) {
      var ev = events[i];
      //controllo
      var lastGoodLevel = -1;
      var collisionDetected = false;
      for (var lev = levels.length - 1; lev >= 0 && !collisionDetected; lev--) {
        for (var j = 0; j < levels[lev].length; j++) {
          var ev2 = levels[lev][j];
          if (!((ev2.endMillis <= ev.startMillis) || (ev2.startMillis >= ev.endMillis))) {
            //non entra su questo livello devo usare quello precedente
            collisionDetected = true;
            break;
          }
        }
        if (!collisionDetected)
          lastGoodLevel = lev;
      }
      if (lastGoodLevel == -1) {
        lastGoodLevel = levels.length;
      }
      if (!levels[lastGoodLevel])
        levels.push([]);
      ev.level = lastGoodLevel;
      levels[lastGoodLevel].push(ev);
    }
    //console.debug("levels", levels);


    //compute overlaps
    for (var lev = 0; lev < levels.length; lev++) {
      var levEv = levels[lev];
      for (var i = 0; i < levEv.length; i++) {
        var ev = levEv[i];
        ev.overlaps = getOverlaps(ev, levels) + ev.level + 1;
      }
    }

    for (var i = 0; i < events.length; i++) {
      var ev = events[i];

      var startDate = new Date(ev.startMillis);
      ev.offset = 0;
      for (var j = i - 1; j >= 0; j--) {
        var ev2 = events[j];
        if (!((ev2.endMillis <= ev.startMillis) || (ev.endMillis <= ev2.startMillis))) {
          ev.offset = ev2.offset + (colWidth / ev2.overlaps) * 0.9;
          break;
        }
      }
      var extSize = (ev.endMillis - ev.startMillis) * folio.getScaleHeight();
      var evDom = $.JST.createFromTemplate(ev, extSize < 32 ? "AGENDAEVENTCOMPACT" : "AGENDAEVENT");
      evDom.data("originalEvent", ev);

      if (ev.isExternal)
        evDom.addClass("external");
      bindEventsOnEvent(evDom, ev, folio);

      var w = colWidth / ev.overlaps;
      folio.addElement(evDom, ev.startMillis - startDate.clearTime().getTime(), colOffset + ev.offset, w, ev.endMillis - ev.startMillis);
    }
  }
}

function drawOneColumnEvents(folio, colWidth, colOffset, events) {
  if (events) {

    //compute levels
    var levels = [];//è un array di eventi messi a strati come nel tetris
    for (var i = 0; i < events.length; i++) {
      var ev = events[i];
      //controllo
      var lastGoodLevel = -1;
      var collisionDetected = false;
      for (var lev = levels.length - 1; lev >= 0 && !collisionDetected; lev--) {
        for (var j = 0; j < levels[lev].length; j++) {
          var ev2 = levels[lev][j];
          if (!((ev2.endMillis <= ev.startMillis) || (ev2.startMillis >= ev.endMillis))) {
            //non entra su questo livello devo usare quello precedente
            collisionDetected = true;
            break;
          }
        }
        if (!collisionDetected)
          lastGoodLevel = lev;
      }
      if (lastGoodLevel == -1) {
        lastGoodLevel = levels.length;
      }
      if (!levels[lastGoodLevel])
        levels.push([]);
      ev.level = lastGoodLevel;
      levels[lastGoodLevel].push(ev);
    }
    //console.debug("levels", levels);

    var levelIndent = 15;

    //draw events
    for (var lev = 0; lev < levels.length; lev++) {
      var levEv = levels[lev];
      for (var i = 0; i < levEv.length; i++) {
        var ev = levEv[i];
        var startDate = new Date(ev.startMillis);

        ev.offset = lev * levelIndent;
        var w = colWidth - ev.offset - 5;
        w = w < 30 ? 30 : w;

        var extSize = (ev.endMillis - ev.startMillis) * folio.getScaleHeight();
        var evDom = $.JST.createFromTemplate(ev, extSize < 32 ? "AGENDAEVENTCOMPACT" : "AGENDAEVENT");
        evDom.data("originalEvent", ev);
        if (ev.isExternal)
          evDom.addClass("external");
        bindEventsOnEvent(evDom, ev, folio);

        folio.addElement(evDom, ev.startMillis - startDate.clearTime().getTime(), colOffset + ev.offset, w, ev.endMillis - ev.startMillis);
      }
    }
  }
}

function getResourceById(resId) {
  for (var i = 0; i < attendees.length; i++) {
    if (resId == attendees[i].resId)
      return attendees[i];
  }
}

</script>

<%
  }
%>
