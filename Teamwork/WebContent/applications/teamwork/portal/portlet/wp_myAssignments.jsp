<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.resource.Person, com.twproject.security.TeamworkPermissions, com.twproject.task.Assignment, com.twproject.utilities.TeamworkComparators, org.jblooming.agenda.CompanyCalendar,
                 org.jblooming.agenda.Period, org.jblooming.utilities.JSP, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.input.TextField,org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState,
                 java.util.Collections, java.util.Date, java.util.List, java.util.Set" %>
<div id="myAssigsExDiv"><%
    PageState pageState = PageState.getCurrentPageState(request);


    PageSeed self = pageState.pagePart(request);
    Form f = new Form(self);
    f.id="MYASSIGS_FORM";
    pageState.setForm(f);
    f.start(pageContext);


    PageSeed ps = new PageSeed(request.getContextPath() + "/applications/teamwork/task/taskList.jsp");
    ButtonLink taskList = new ButtonLink(ps);
    taskList.iconChar="t";
    taskList.label="";
    taskList.toolTip= I18n.get("MY_ASSIGNMENTS");

    taskList.label=I18n.get("MY_ASSIGNMENTS");
    taskList.iconChar="";

    ButtonJS bs = new ButtonJS();
    bs.onClickScript = "$('#myAssigs').toggle()";
    bs.iconChar="g";
    bs.additionalCssClass="ruzzol";
    bs.label="";
    bs.toolTip= I18n.get("FILTER");


    String bsa = ButtonSubmit.getAjaxButton(f, "myAssigsExDiv").generateJs().toString();

  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
  boolean createTask = logged.hasPermissionFor(TeamworkPermissions.project_canCreate);


%><div class="portletBox wp_myAssignemnt">
    <style type="text/css">

        .myAssigsEvidence .button.textual.bolder {
          color: #D30202;
        }

      .wp_myAssignemnt #wp_myAssig_table .facesBox {
        white-space: nowrap;
      }

    </style>

    <div style="float:right; padding-top: 5px">
        <%//taskList.toHtmlInTextOnlyModality(pageContext);%>
        &nbsp;<%bs.toHtmlInTextOnlyModality(pageContext);%>
    </div>
    <h1><%taskList.toHtmlInTextOnlyModality(pageContext);%></h1>

    <div class="portletParams" id="myAssigs" style="display:none;"><%
        boolean showNotActiveAssig = pageState.getEntryOrDefault("ASSIG_SHOW_NOTACTIVE", Fields.FALSE).checkFieldValue();
        CheckField cfMeeting = new CheckField("ASSIG_SHOW_NOTACTIVE","",false);
        cfMeeting.additionalOnclickScript=bsa;
        cfMeeting.toHtmlI18n(pageContext);

        boolean showNotActiveTasks = pageState.getEntryOrDefault("ASSIG_SHOW_NOTACTIVETASKS", Fields.FALSE).checkFieldValue();
        CheckField cfNotActTsk = new CheckField("ASSIG_SHOW_NOTACTIVETASKS","",false);
        cfNotActTsk.additionalOnclickScript=bsa;
        cfNotActTsk.toHtmlI18n(pageContext);

    %>&nbsp;&nbsp;&nbsp;&nbsp;<%
        int daysOnTheHorizon = pageState.getEntryOrDefault("ASSIG_SHOW_HORIZON", "0").intValue();

        TextField tf = new TextField("ASSIG_SHOW_HORIZON","&nbsp;");
        tf.fieldSize=4;
        tf.script=" onBlur=\""+bsa+"\" ";

        tf.toHtmlI18n(pageContext);


        //inhibit submit
    %><input type="text" style="display:none;"></div><%


%><table class="table" id="wp_myAssig_table">
  <thead class="dataTableHead" style=""><tr>
    <th class="tableHead" colspan="2" id="wpMAputFilerHere"><span class="tableHeadEl"><%=I18n.get("TASK")%></span></th>
    <th class="tableHead"><span class="tableHeadEl"><%=I18n.get("ROLE")%></span></th>
    <th class="tableHead"><span class="tableHeadEl lreq20 lreqLabel"><%=I18n.get("PRIORITY")%></span></th>
    <th class="tableHead"><span class="tableHeadEl"><%=I18n.get("ISSUES")%></span></th>
    <th class="tableHead"><span class="tableHeadEl"><%=I18n.get("WORKLOG_DONE_SHORT")%></span></th>
    <th class="tableHead lreq20 lreqHide">&nbsp;</th>
    <th class="tableHead"><span class="tableHeadEl"><%=I18n.get("WORKGROUP")%></span></th>
    <th class="tableHead" style="width: 5px;"><span class="tableHeadEl"><%=I18n.get("PROGRESS")%></span></th>
  </tr></thead>
  <%

    Person resource = Person.getLoggedPerson(pageState);

    boolean hasSomeAssig = false;
    boolean printedAssig = false;

    JspHelper assDrawer = new JspHelper("parts/partAssigLine.jsp");

    List<Assignment> expiredAssigs = resource.getExpiredAssignments(new Date(), !showNotActiveAssig,true);
    if (JSP.ex(expiredAssigs)){

        hasSomeAssig=true;
%><tbody class="myAssigsEvidence"><%

   Collections.sort(expiredAssigs, new TeamworkComparators.AssignmentComparatorByTaskEnd());

    java.util.Set addedexp = new java.util.HashSet();
    for (Assignment ass : expiredAssigs) {
        if (Assignment.ACTIVITY_REPEATED_IN_TIME.equals(ass.getActivity()))
            continue;
        if (!addedexp.contains(ass.getTask().getId())) {
            assDrawer.parameters.put("assignment", ass);
            assDrawer.toHtml(pageContext);
            printedAssig = true;
            addedexp.add(ass.getTask().getId());
        }
    }


%></tbody><%
    }

    List<Assignment> assigs = resource.getAssignmentsByPriority(new Period(System.currentTimeMillis(),System.currentTimeMillis()+10), !showNotActiveAssig, !showNotActiveTasks);

    if (daysOnTheHorizon>0) {
        CompanyCalendar cc = new CompanyCalendar();
        Date start = cc.setAndGetTimeToDayStart();
        cc.addWorkingDays(daysOnTheHorizon);
        List<Assignment> assignmentList = resource.getAssignmentsByPriority(new Period(start, cc.setAndGetTimeToDayEnd()), !showNotActiveAssig, !showNotActiveTasks);
        for (Assignment ass: assignmentList){
            if (!assigs.contains(ass))
                assigs.add(ass);
        }
    }


    Set added = new java.util.HashSet();
    for (Assignment ass : assigs) {
        hasSomeAssig = true;
        if (Assignment.ACTIVITY_REPEATED_IN_TIME.equals(ass.getActivity()))
            continue;
        if (!added.contains(ass.getTask().getId())) {
            assDrawer.parameters.put("assignment", ass);
            assDrawer.toHtml(pageContext);
            printedAssig = true;
            added.add(ass.getTask().getId());

        }
    }

%></table><%



if (!hasSomeAssig && !createTask) {
%><div class="hint"><%=I18n.get("ASK_PM_CREATE_ASSIG")%></div><%
} else if (!printedAssig) {
%><div class="hint"><%=I18n.get("NO_ASSIG_FOR_SETTINGS")%></div><%
    }

%></div><%

    f.end(pageContext);
%>


<script type="text/javascript">

  //bind for worklogSaved event
  registerEvent("worklogEvent.wpMyAssignment",function(ev,data){
    //console.debug("worklogEvent.wpMyAssignment",data);
    if (data.type=="save" && data.response&& data.response.worklog)
      $(".wp_myAssignemnt tr[assId="+data.response.worklog.assId+"] .wlg").html(getMillisInHoursMinutes(data.response.worklog.totalWl));
    else if (data.type=="delete")
      refreshPortlet($(".wp_myAssignemnt"));
  });


  //inject the table search
  createTableFilterElement($("#wpMAputFilerHere"),'#wp_myAssig_table .assigRow','.columnTaskName,.columnTaskCode');

</script>


</div>
