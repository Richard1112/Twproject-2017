<%@ page import="com.twproject.operator.TeamworkOperator,
com.twproject.security.TeamworkPermissions, com.twproject.task.*, com.twproject.waf.TeamworkPopUpScreen,
com.twproject.worklog.Worklog, com.twproject.worklog.businessLogic.WorklogBricks, com.twproject.worklog.businessLogic.WorklogController, org.jblooming.designer.DesignerField, org.jblooming.utilities.DateUtilities,
org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.input.ColorValueChooser, org.jblooming.waf.html.input.DateField, org.jblooming.waf.html.input.SmartCombo, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.ArrayList, java.util.HashSet, java.util.List, java.util.Set, org.jblooming.security.*"%><%


  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new WorklogController(),request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);
  } else {

    PageSeed self = pageState.thisPage(request);
    self.addClientEntry(pageState.getEntry("ISSUE_ID"));
    self.addClientEntry(pageState.getEntry("ASS_ID"));
    self.command=Commands.FIND;

    Form  f = new Form(self);
    f.id="WORKLOG";
    f.alertOnChange = false;
    pageState.setForm(f);
    f.start(pageContext);

    boolean taskManageWorklog=false;

    %><h1><%=I18n.get("HIS_WORKLOGLIST")%></h1><%


    Assignment assig = Assignment.load(pageState.getEntry("ASS_ID").stringValueNullIfEmpty()+"");
    Issue issue = Issue.load(pageState.getEntry("ISSUE_ID").stringValueNullIfEmpty() + "");

    if(issue!=null){
      assig = issue.getAssignmentOnTask(issue.getAssignedTo());
    }

    if (assig != null) {
      Task task=assig.getTask();
      taskManageWorklog = assig.hasPermissionFor(logged,TeamworkPermissions.worklog_manage);

      boolean canReadWorklog=assig.hasPermissionFor(logged, TeamworkPermissions.task_canRead)||assig.hasPermissionFor(logged,TeamworkPermissions.resource_manage)||taskManageWorklog;
      org.jblooming.security.SecurityException.riseExceptionIfNoPermission(canReadWorklog,TeamworkPermissions.task_canRead,assig);

      PageSeed tasked = pageState.pageFromRoot("task/taskOverview.jsp");
      tasked.mainObjectId = task.getId();
      tasked.command = Commands.EDIT;

      tasked.mainObjectId = task.getId();
      tasked.command = Commands.EDIT;
      ButtonLink taskbl = new ButtonLink(task.getDisplayName(), tasked);

      %><div class="pathToObject"><%taskbl.toHtmlInTextOnlyModality(pageContext);%></div><h2><%=assig.getDisplayNameWithResource()%></h2><%
    } else if (issue != null) {
      %><h2>I#<%=issue.getMnemonicCode()%>#</h2><%
    } else {
      %><h2><%=I18n.get("NO_ELEMENTS_FOUND")%></h2><%
      return;
    }
%>

<div id="bulkOp" style="display:none;">
    <div id="bulkRowSel"></div>
  <div>
    <%
      ButtonJS changeTaskAll = new ButtonJS("openBulkAction('moveToAssigAll');");
      changeTaskAll.label = I18n.get("WORKLOG_CHANGEASSIG_ALL");
      changeTaskAll.iconChar = " y";
      changeTaskAll.toHtmlInTextOnlyModality(pageContext);

    %></div><div><%

      ButtonJS changeStatusAll = new ButtonJS("openBulkAction('changeStatusAll');");
      changeStatusAll.label = I18n.get("WORKLOG_CHANGESTATUS_ASSIG");
      changeStatusAll.iconChar ="h";
      changeStatusAll.additionalCssClass="lreq20 lreqHide";
      if(logged.hasPermissionFor(TeamworkPermissions.worklog_manage))
        changeStatusAll.toHtmlInTextOnlyModality(pageContext);

   %></div><div><%

      ButtonJS removeAll = new ButtonJS("performBulkAction('WORKLOG_DELETE', $(this));");
      removeAll.confirmRequire=true;
      removeAll.confirmQuestion=I18n.get("WORKLOG_CONFIRM_REMOVE_ALL");
      removeAll.label = I18n.get("WORKLOG_REMOVE_ALL");
      removeAll.iconChar="&#xa2;";
      removeAll.toHtmlInTextOnlyModality(pageContext);


    %>
  </div>


</div>

<table class="table dataTable fixHead fixFoot" id="multi" >
  <thead class="dataTableHead" style="">
  <tr>
    <th class="tableHead" nowrap="" ><%if(taskManageWorklog){%><input type="checkbox" onclick="selUnselAll($(this));" title="<%=I18n.get("SELECT_DESELECT_ALL")%>"/><%}%></th>
    <th class="tableHead" nowrap=""><%=I18n.get("DATE")%></th>
    <th class="tableHead" nowrap=""><%=I18n.get("WORKLOG_DURATION")%></th>
    <th class="tableHead lreq20 lreqHide" nowrap="" ><%=I18n.get("STATUS")%></th>
    <th class="tableHead" nowrap=""><%=I18n.get("WORKLOG_ACTION")%></th>
    <th class="tableHead" nowrap="" ><%=I18n.get("ISSUES")%></th>
    <th class="tableHead" nowrap="" ><%=I18n.get("DELETE_SHORT")%></th>

  </thead>
    <%

    long totWL=0;
      List<Worklog> wklg= new ArrayList<Worklog>();
      if( pageState.getPage()!=null)
        wklg = pageState.getPage().getAllElements();

    if (wklg.size()>0) {
      // test security
      int fieldCount=0;
      for (Worklog workLog : wklg) {

        boolean canWrite=workLog.bricks.canWrite(logged);

        totWL=totWL+workLog.getDuration();

  %><tr class="data" wlId="<%=workLog.getId()%>" assId="<%=workLog.getAssig().getId()%>" millis="<%=workLog.getInserted().getTime()%>">
    <td width="1%" align="center"><%
         if (canWrite){
           %><input type="checkbox" onclick="refreshBulk($(this));" class="selector"><%
         } else {
           %>&nbsp;<%
         }

  %></td>

    <td width="1%" align="left" nowrap>
      <%

        pageState.addClientEntry("df_" + fieldCount,workLog.getInserted());
        DateField df= new DateField("df_" + fieldCount);
        df.labelstr="";
        df.separator="";
        df.onblurOnDateValid="updateDate('"+df.id+"');";
        df.readOnly=!canWrite;
        df.toHtml(pageContext);
      %>&nbsp;<%
        if (workLog.getInserted()!=null){
          PageSeed wlw=pageState.pageInThisFolder("worklogWeek.jsp",request);
          wlw.addClientEntry("FOCUS_MILLIS",""+workLog.getInserted().getTime());
          wlw.addClientEntry("RES_ID",workLog.getAssig().getResource());
          //ButtonLink wlwb=new ButtonLink(I18n.get("GO_TO_TIMESHEET"),wlw);
          ButtonLink wlwb=new ButtonLink("",wlw);
          wlwb.target="_top";
          wlwb.toolTip=I18n.get("GO_TO_TIMESHEET");
          wlwb.iconChar="8";
          wlwb.enabled=canWrite;
          wlwb.toHtmlInTextOnlyModality(pageContext);
        }
      %>
    </td>
    <td width="1%" nowrap><%

      fieldCount++;

      pageState.addClientEntry("wl_" + fieldCount,DateUtilities.getMillisInHoursMinutes(workLog.getDuration()));
      TextField tfDur = TextField.getDurationInMillisInstance("wl_" + fieldCount);
      tfDur.separator="";
      tfDur.label = "";
      tfDur.maxlength=5;
      tfDur.fieldSize=5;
      tfDur.minValue="0:01";
      //tfDur.maxValue="24:00";
      tfDur.fieldClass=tfDur.fieldClass+" wlTime";
      tfDur.readOnly = !canWrite;
      tfDur.script=" wlId=\""+workLog.getId()+"\" millis=\""+(workLog.getInserted()==null?"":workLog.getInserted().getTime())+"\" duration=\""+workLog.getDuration()+"\"";
      tfDur.toHtml(pageContext);

      %></td><td width="5%" align="center"  class="lreq20 lreqHide"><%

      ColorValueChooser stsCh = WorklogBricks.getStatusChooser("STSCH"+workLog.getId(), false, pageState);
      stsCh.showOpener=false;
      stsCh.displayValue=false;
      stsCh.disabled=!taskManageWorklog || !License.assertLevel(30);
      stsCh.onChangeScript="rowChangeStatus(hidden)";
      pageState.addClientEntry("STSCH"+workLog.getId(),workLog.getStatus());

      stsCh.toHtml(pageContext);


  %></td>
    <td width="60%" align="left"><%

      pageState.addClientEntry("d" + tfDur.id,workLog.getAction());
      TextField tfDescr = new TextField("d" + tfDur.id, "");
      tfDescr.label = "";
      tfDescr.fieldSize=80;
      tfDescr.fieldClass=tfDescr.fieldClass+" wlDescr";
      tfDescr.readOnly = !canWrite;
      tfDescr.innerLabel=I18n.get("WORKLOG_ACTION");
      tfDescr.toHtml(pageContext);
      %>

      <div style="font-size: 80%;"><%
        for (int i=1; i<5; i++) {
          DesignerField dfStr = DesignerField.getCustomFieldInstance( "WORKLOG_CUSTOM_FIELD_",i, workLog ,true,true, false, pageState);
          if (dfStr!=null){
            dfStr.separator="&nbsp;";
            dfStr.toHtml(pageContext);
            %>&nbsp;&nbsp;&nbsp;<%
          }
        }
      %></div>


    </td>

    <td width="1%"  align="center"><%
    if (workLog.getIssue() != null) {
      ButtonSupport iEdit = IssueBricks.getBlackEditor(workLog.getIssue().getId(),Commands.EDIT);
      iEdit.iconChar="i";
      iEdit.toolTip = JSP.encode(JSP.limWr(workLog.getIssue().getDescription(),30));
      iEdit.toHtmlInTextOnlyModality(pageContext);

    } else {

      %>&nbsp;<%
    }
    %></td>

    <td width="1%" align="center">
    <%     
      ButtonJS delData = new ButtonJS("deleteWl($(this));");
      delData.confirmRequire = true;
      delData.confirmQuestion = I18n.get("CONFIRM_DELETE_WKL");
      delData.enabled = canWrite;
      delData.label="";
      delData.iconChar="d";
      delData.additionalCssClass="delete";
      delData.toHtmlInTextOnlyModality(pageContext);
    %></td></tr><%

    }
    %>
    <tfoot class="totals">
    <tr><td id="numWl" colspan="2"><%=I18n.get("WORKLOG_%%_TOTAL_PERIOD",wklg.size()+"")%> </td>
    <td id="totWl"><%=DateUtilities.getMillisInHoursMinutes(totWL)%></td>
    <td colspan="5">&nbsp;</td>
    </tr>
    <tr><td id="bulkPlace" colspan="99"></td></tr>
    </tfoot><%
  } else {
  %> <tr><td><h2 class="hint"><%=I18n.get("NO_WORKLOGS")%></h2></td></tr><%
    }
  %></table>

<%
  f.end(pageContext);
%>

<div id="moveToAssigAll" style="display:none;" class="bulkData">
  <h1><%=I18n.get("WORKLOG_CHANGEASSIG_ALL")%></h1>
  <table><tr>
    <td><%

      List<String> myAss= new ArrayList<String>();
      if (assig!=null)
        myAss.add(assig.getId()+"");

      SmartCombo assigs = null;

      //if (logged.hasPermissionFor(TeamworkPermissions.worklog_manage)||logged.hasPermissionFor(TeamworkPermissions.resource_assignment_manage) || taskCanMoveWorklog) {
      if (taskManageWorklog) {
        Set<Permission> perms = new HashSet();
        //perms.add(TeamworkPermissions.assignment_manage);
        perms.add(TeamworkPermissions.worklog_manage);
        assigs = TaskBricks.getAssignmentCombo("NEW_ASSIG", null, perms, null, pageState);
      } else {
        //your dealing with your own
        assigs = TaskBricks.getAllMyAssignmentsCombo("NEW_ASSIG", null, pageState);
      }

      assigs.label = I18n.get("MOVE_WORKLOG_TO_ASSIG");
      assigs.fieldSize = 30;
      assigs.separator = "<br>";
      assigs.toHtml(pageContext);

    %></td></tr>
    <tr><td><br><%

    ButtonJS moveInstance = new ButtonJS("performBulkAction('WORKLOG_MOVE', $(this));");
    moveInstance.label=I18n.get("PROCEED");
    moveInstance.additionalCssClass="first";
    moveInstance.toHtml(pageContext);


  %></td>

  </tr></table>
</div>

<div id="changeStatusAll" style="display:none;" class="bulkData">
  <h1><%=I18n.get("WORKLOG_CHANGESTATUS_ASSIG")%></h1>
  <table><tr>
    <td><%
      ColorValueChooser chooser = WorklogBricks.getStatusChooser("WL_STATUS", false, pageState);
      chooser.setType("WLSTSBLK");
      chooser.label=I18n.get("WORKLOG_STATUS");
      chooser.separator="<br>";
      chooser.height=26;
      chooser.width=120;
      chooser.toHtml(pageContext);

    %></td></tr><tr>
    <td><br><%

      ButtonJS statusInstance = new ButtonJS("performBulkAction('WORKLOG_CHANGESTATUS', $(this));");
      statusInstance.label=I18n.get("PROCEED");
      statusInstance.additionalCssClass="first";
      statusInstance.toHtml(pageContext);

    %></td>

  </tr></table>
</div>


<script type="text/javascript">


  $(document).ready(function () {

  $("#multi").find(":text.wlTime").bind("blur", saveWL);
  $("#multi").find(":text.wlDescr").bind("blur", saveDescr);
});

  function saveWL() {
    var tf = $(this);
    //save only if something is changed
    if (tf.isValueChanged()) {
      showSavingMessage();
      var assId = tf.closest("assId").attr("assId");
      var wlId = tf.attr("wlId");
      var dur = tf.val();
      var millis = tf.attr("millis");
      var inserted = new Date(parseInt(millis)).format();
      var tfDescr = $("#d" + tf.prop("id"));
      var descr = tfDescr.val();

      showSavingMessage();
      var req={CM:"SV",
        assId:assId,
        wlId:wlId,
        WORKLOG_DURATION:dur,
        WORKLOG_INSERTIONDATE: inserted,
        WORKLOG_ACTION: descr
      };

      $.getJSON("worklogAjaxController.jsp",req,function(response){
        jsonResponseHandling(response);
        if (response.ok){
          tf.attr("wlId", response.wlId);
          tf.updateOldValue();
          tf.attr("duration",response.duration);
           updateCellTot();
        }
        hideSavingMessage();
      });
    }

  }


  function updateCellTot(){
    // update line counter
    $("#numWl").html("<%=I18n.get("WORKLOG_%%_TOTAL_PERIOD")%>".replace("%%",$(":text.wlTime").length));

    //update col total
    var totByCol=0
    $(":text.wlTime").each(function(index, domElement){
      totByCol=totByCol+  parseInt($(domElement).attr("duration"));
    });
    $("#totWl").html("<b>"+getMillisInHoursMinutes(totByCol)+"</b>");
  }


  function saveDescr() {
    var tfDescr = $(this);
    var wlId = tfDescr.parents("[wlId]:first").attr("wlId");
    //save only if something is changed and have a valid wl id
    if (tfDescr.isValueChanged() && wlId.trim() != "") {
      
          
      showSavingMessage();

      var req = {"<%=Commands.COMMAND%>":"UPDDESCR",
        wlId:wlId,
        descr:tfDescr.val()};

      $.getJSON("worklogAjaxController.jsp", req, function(response) {
        jsonResponseHandling(response);
        if (response.ok) {
          //reset variables
          tfDescr.updateOldValue();
        }

        hideSavingMessage();
      });

    }
  }

function updateDate(fieldId){
  var tfDate = $("#"+fieldId);
  var wlId = tfDate.parents("[wlId]:first").attr("wlId");
  //save only if something is changed and have a valid wl id
  if (tfDate.isValueChanged() && wlId.trim() != "") {
    showSavingMessage();
    var req = {"<%=Commands.COMMAND%>":"UPDDATE",
        wlId:encodeURIComponent(wlId),
        newdate:tfDate.val()
       };

      $.getJSON("worklogAjaxController.jsp", req, function(response) {
        jsonResponseHandling(response);
        if (response.ok) {
          //reset variables
           tfDate.updateOldValue();
        }
        hideSavingMessage();
      });
  }
}

  function performBulkAction(command, el) {
    var form = $("#WORKLOG");
    var ids = getCheckedIds();
    if (ids.length > 0) {
      //change command
      form.find("[name=CM]").val(command);

      // inject hidden ids field on form
      var hid = $("<input type='hidden' name='wlIds'>");
      hid.val(ids.join(","));
      form.append(hid);

      if (el) {
        // move div "moveToTaskAll" to form in order to submit ce
        form.append(el.closest(".bulkData").hide());
      }
      form.submit();
    }
  }

  function getCheckedIds() {
    var ret = [];
    $("#multi .selector:checked").each(function() {
      ret.push($(this).closest("[wlId]").attr("wlId"));
    });
    return ret;
  }


  function rowChangeStatus(el){
    showSavingMessage();
    var theRow = el.closest("tr[wlId]");
    var wlId = theRow.attr("wlId");
    //console.debug("rowChangeStatus",wlId,el.val());
    var req = {"CM":"WLCHSTS",wlId:wlId,stsId:el.val()};

    $.getJSON("worklogAjaxController.jsp", req, function(response) {
      jsonResponseHandling(response);
      if (response.ok) {
        //do nothing

      }
      hideSavingMessage();
    });

  }

  function deleteWl(el) {
    showSavingMessage();
    $.getJSON("worklogAjaxController.jsp", {CM: "DL", wlId: el.closest('[wlid]').attr("wlId")}, function (response) {
      jsonResponseHandling(response);
      if (response.ok) {
        el.closest('[wlid]').remove();
        updateCellTot();
      }
      hideSavingMessage();
    });
  }



</script>

<%
  }
%>
