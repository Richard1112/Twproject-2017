<%@ page import="org.jblooming.waf.settings.I18n, org.jblooming.waf.html.input.DateField, com.twproject.worklog.Worklog" %>
<%----------------------------------------------------------  PAGES DEFINITION  ---------------------------------------------------------%>

<%-------------------------------------  worklog List page ---------------------------------------%>
<div data-role="page" id="worklogList" title="<%=I18n.get("WORKLOG")%>" class="worklogList">
  <table width="100%" cellspacing="0" cellpadding="0" class="worklogNav" data-position="fixed">
    <tr>
      <td width="10%"><div data-role="button" onclick="worklogMove(false);" class="prev full" title="<%=I18n.get("PREVIOUS")%>"></div></td>
      <td><div data-role="date"></div></td>
      <td width="10%"><div data-role="button" onclick="worklogMove(true);" class="next full" title="<%=I18n.get("NEXT")%>"></div></td>
    </tr>
  </table>

  <div data-role="content" class="scroll">
    <div id="worklogListPlace" width="100%"></div>
  </div>

  <div data-role="footer" data-position="fixed" class="groupRow">
    <div class="groupCell left inputBox col6">
      <div data-role="button" onmousedown="backPage();" class="back" title="<%=I18n.get("BACK")%>"></div>
    </div>
    <div class="groupCell right inputBox col6">
      <button onmousedown="worklogEditor();" class="first"><span class="teamworkIcon big">P</span></button>
    </div>
  </div>
</div>

<%-------------------------------------  worklog editor page --------------------------------------%>
<div data-role="page" class="editor" id="worklogEditor" title="<%=I18n.get("ADD_WORKLOG")%>">
  <div data-role="header" data-position="fixed">
    <div data-role="button" onmousedown="backPage();" class="close" ></div>
    <div data-role="title"></div>
  </div>

  <div data-role="content" class="scroll">
    <div id="worklogEditorPlace"></div>
  </div>
  <div data-role="footer" data-position="fixed" class="groupRow">
    <div class="groupCell left inputBox col6">
      <div data-role="button" onmousedown="backPage();" class="back" title="<%=I18n.get("BACK")%>"></div>
    </div>
    <div class="groupCell right inputBox col6">
      <button onmousedown="saveWorklog($(this));" class="save" ><%=I18n.get("SAVE")%></button>
    </div>
  </div>
</div>

<%----------------------------------------------------------  TEMPLATES  ---------------------------------------------------------%>
<div class="_mobileTemplates">

  <%-- ---------------------------------  WORKLOG_ROW TEMPLATE ------------------------------------------------------------------------------------------------------------------- --%>
    <%=JST.start("WORKLOG_ROW")%>
    <div data-role="swiper" class="listRow" wlId="(#=id#)" assId="(#=assId#)" taskId="(#=taskId#)" millis="(#=insertedMillis#)">
      <div taskId="(#=taskId#)" onclick="viewTask($(this));" style="width: 100%">
          <h2>(#=obj.taskName#)</h2>
          <div class="action">(#=obj.action#)</div>
        </div>
        <div style="width:80px; font-size:1.4em;text-align: right; vertical-align: baseline">(#=getMillisInHoursMinutes(obj.duration)#)</div>

      <%-- SWIPE BOX --%>
      <div class="swipeBox">

        <%--todo: add actions--%>
        <div class="swipeButton"><span class="teamworkIcon delete" msg="<%=I18n.get("FLD_CONFIRM_DELETE")%>" onclick="deleteWorklog($(this));">d</span></div>

      </div>

    </div>

    <%=JST.end()%>

  <%-- ---------------------------------  WORKLOG_TOTAL TEMPLATE ------------------------------------------------------------------------------------------------------------------- --%>
    <%=JST.start("WORKLOG_TOTAL")%>
    <div class="listRow">
      <div style="width: 100%"><i><%=I18n.get("WORKLOG_TOTAL_DAILY")%></i></div>
      <div style="width:80px; font-size:1.4em;text-align: right"><b>(#=wlTot#)</b></div>
    </div>
    <%=JST.end()%>

  <%-- ---------------------------------  WORKLOG_EDITOR TEMPLATE ------------------------------------------------------------------------------------------------------------------- --%>
  <%=JST.start("WORKLOG_EDITOR")%>
  <div class="editor" wlId="(#=obj.id#)">


    <div class="groupRow">
      <div class="groupCell inputBox col12">
        <%

          pageState.addClientEntry("wlAssId", "(#=obj.assId#)");
          pageState.addClientEntry("wlAssId_txt", "(#=obj.taskName#)");
          SmartCombo wlAssigs = TaskBricks.getAllAssignmentsOfCombo("wlAssId", logged.getPerson(), Period.getWeekPeriodInstance(new Date(),logged.getLocale()), false, false, false, false, null);
          wlAssigs.label = I18n.get("TASK");
          wlAssigs.separator = "";
          wlAssigs.fieldSize = 45;
          wlAssigs.required=true;
          wlAssigs.onValueSelectedScript="wlAssigSelected($(this));";
          wlAssigs.toHtml(pageContext);

          pageState.addClientEntry("ISSUEID", "(#=obj.issueId#)");
          TextField.hiddenInstanceToHtml("ISSUEID",pageContext);

        %>
      </div>
    </div>

    <div class="groupRow">
      <div class="groupCell inputBox col6">
        <label for="wlDoneOn"><%=I18n.get("WORKLOG_INSERTIONDATE")%></label>
        <input type="text" id="wlDoneOn" name="wlDoneOn" value="(#=currentDate.format()#)" style="width: 100%" readonly class="noFocus" onclick="this.blur(); wlOpenCalendar($(this))">
        <span class="teamworkIcon calendarOpener" onclick="wlOpenCalendar($(this))">m</span>
      </div>
      <div class="groupCell inputBox col6">
        <%

          pageState.addClientEntry("DURATION", "(#=obj.duration#)");
          TextField dur = TextField.getTimeInstance("DURATION");
          dur.label="DURATION";
          dur.fieldSize=30;
          dur.required=true;
          dur.separator="";
          dur.toHtmlI18n(pageContext);

        %>
      </div>
    </div>
<%          // questa e il path local senza contextpath a questa pagina che può essere sovrascritto con una custom label
              String descriptionFormUrl = ApplicationState.contextPath+"/applications/teamwork/task/worklog/worklogDescriptionForm.jsp";
              if (I18n.isActive("CUSTOM_FEATURE_WORKLOG_FORM")) {
                descriptionFormUrl = ApplicationState.contextPath+I18n.get("CUSTOM_FEATURE_WORKLOG_FORM");
              }
            %>
    <div class="groupRow">
      <div class="groupCell inputBox col12">
        <div id="worklogCustomFormPlace" class="wlCustom"></div>
      </div>
    </div>

  </div>
  <%=JST.end()%>

  <%-- ---------------------------------  NO_WORLOG FOUND ------------------------------------------------------------------------------------------------------------------- --%>
  <%=JST.start("NO_WORKLOGS")%>
  <div class="noElementFound">
    <%=I18n.get("NO_WORKLOGS")%>
  </div>
  <%=JST.end()%>

</div>

<%----------------------------------------------------------  DECORATORS  ---------------------------------------------------------%>
<script>


</script>


<%----------------------------------------------------------  WORKLOG PAGES FUNCTIONS  ---------------------------------------------------------%>
<script>

  var maxWorklogAgeInDays=<%=new ClientEntry("dummy",I18n.get("CUSTOM_FEATURE_DO_NOT_ACCEPT_OLD_WORLOG")).durationInWorkingDaysNoErrorNoCatchedExc(false)%>

  function worklogEditorEnter(event, data, fromPage, isBack){
    if(!data){
      data ={};
    }
    $(this).find("#worklogEditorPlace").empty().append($.JST.createFromTemplate(data, "WORKLOG_EDITOR"));
    if(data.assId){
      wlAssigSelected(currentPage.find("#wlAssId"))
    }
  }


  function worklogEditorLeave(event, data, fromPage, isBack){
    $(this).find("#worklogEditorPlace").empty();
  }

  function worklogListEnter(event, data, fromPage, isBack){
    fillWordlogDay();
  }


  function fillWordlogDay(){
    var page=$("#worklogList");
    currentDate=currentDate||new Date();

    var dateInt=currentDate.toInt();
    var monthWorklogs = applicationCache.worklogs[parseInt(dateInt / 100)];

    if (!monthWorklogs) {
      var filter = {"CM": "LOADWORKLOG", "DATEINT": dateInt};
      callController(filter, function(response) {

        //update cache
        var month = new Object();
        for (var day in response.worklogs) {
          var wls = response.worklogs[day];
          month[day] = wls;
        }
        applicationCache.worklogs[parseInt(dateInt / 100)] = month;

        //call itself
        fillWordlogDay();
      });

    } else {
      //set title
      page.find("[data-role=date]").html(currentDate.format(mobileFullDateFormat));

      //draw rows
      var dayWorklogs = monthWorklogs[dateInt];
      var ndo = $("#worklogList #worklogListPlace");
      ndo.empty();
      if (dayWorklogs && dayWorklogs.length > 0) {
        var wlTot = 0;
        for (var i in dayWorklogs) {
          var wl = dayWorklogs[i];
          wlTot += wl.duration;
          ndo.append($.JST.createFromTemplate(wl, "WORKLOG_ROW"));
        }

        ndo.append($.JST.createFromTemplate({wlTot:getMillisInHoursMinutes(wlTot)}, "WORKLOG_TOTAL"));

        //enable swipe action on list row
        enableSwipe();

      } else {
        ndo.append($.JST.createFromTemplate({}, "NO_WORKLOGS"));
      }
    }

    refreshIscroll();

  }


  function worklogMove(forward) {
    currentDate.setDate(currentDate.getDate() +(forward ? 1 : -1));
    fillWordlogDay();
  }


  function worklogEditor(el) {
    goToPage("worklogEditor", {});
  }


  function wlAssigSelected(el){
    var assId=el.val();

    var wlId=el.closest("[wlId]").attr("wlId");
    var box=$("#worklogCustomFormPlace");

    var request = {assId: assId};
    if (wlId)
      request.wlId=wlId;



    box.load("<%=descriptionFormUrl%>", request, function (html) {
    box.find("#descr").remove();

      if (currentPage)
        refreshIscroll();

    });
  }

  function saveWorklog(el) {
    var ed = currentPage.find("#worklogEditorPlace");

    if (ed.find(":input").isFullfilled()) {

      var request = {"CM": "SAVEWORKLOG"};
      ed.fillJsonWithInputValues(request);

      request.ASSID = request.wlAssId;

      request.DATEINT = Date.parseString(request.wlDoneOn).toInt();

      callController(request, function (response) {
        //update cache
        var worklog = response.worklog;

        currentDate = new Date(worklog.insertedMillis);
        var dateInt = currentDate.toInt();
        var monthWorklogs = applicationCache.worklogs[parseInt(dateInt / 100)];
        if (monthWorklogs) {
          var wlDay = monthWorklogs[dateInt];
          if (!wlDay) {
            wlDay = [];
            monthWorklogs[dateInt] = wlDay;
          }
          wlDay.push(worklog);
        }

        backPage();
        //fillWordlogDay();

      });
    }
  }


  function deleteWorklog(el){

    el.confirm(function(){

    var row = el.closest("[wlId]");

    var d= new Date(parseInt(row.attr("millis")));

    var wlId = row.attr("wlId");
    var filter = { "CM": "DLWL", "WLID":wlId};

    callController(filter, function(response) {
      //update cache

      var dateInt = d.toInt();
      var dayWorklogs = applicationCache.worklogs[parseInt(dateInt / 100)][dateInt];
      for  (var i in dayWorklogs) {
        if (dayWorklogs[i].id==wlId){
          dayWorklogs.splice(i,1);
          break;
        }
      }
      fillWordlogDay();
    });
    },"<%=I18n.get("FLD_CONFIRM_DELETE")%>");
  }


  function wlOpenCalendar(el){
    var inp = el.closest("[data-role=page]").find("#wlDoneOn");
    if (inp.is(".componentOverlayTF"))
      return;
    var config = {
      inputField:     inp,
      centerOnScreen:true,
      useYears:       1,
      useMonths:      3,
      showToday:      true,
      notAfterMillis: new Date().getTime() + 3600000 * 24,
      width:          "100%"
    };
    if (maxWorklogAgeInDays>0)
      config.notBeforeMillis=new Date()-3600000*24*maxWorklogAgeInDays;
    inp.dateField(config);
  }

</script>



