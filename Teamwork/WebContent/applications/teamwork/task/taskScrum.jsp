<%@ page import="com.twproject.security.TeamworkPermissions, com.twproject.task.Task, com.twproject.task.businessLogic.TaskController,
com.twproject.utilities.TeamworkComparators, com.twproject.waf.TeamworkHBFScreen, com.twproject.waf.html.StatusIcon,
org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands,
org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.display.PathToObject, org.jblooming.waf.html.display.PercentileDisplay,
org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed,
org.jblooming.waf.view.PageState" %><%
  PageState pageState = PageState.getCurrentPageState(request);
  pageState.setPopup(true);
  if (!pageState.screenRunning) {

    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(new TaskController(), request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);

  } else {

    Task task = (Task) pageState.getMainObject();


    // Form -----------------------------------------------------------------------------------------------

    PageSeed self = pageState.thisPage(request);
    self.mainObjectId = task.getId();
    self.setCommand(Commands.EDIT);
    String parId = pageState.getEntry(Fields.PARENT_ID).stringValueNullIfEmpty();
    self.addClientEntry(Fields.PARENT_ID, JSP.w(parId));
    Form f = new Form(self);
    f.encType = Form.MULTIPART_FORM_DATA;  //do not remove or the upload will not work
    f.alertOnChange = true;
    pageState.setForm(f);
     f.start(pageContext);


    %><script>
      $(function(){
        $("#TASK_MENU").addClass('selected');
      });
    </script><%

   /*
________________________________________________________________________________________________________________________________________________________________________


      path to object

________________________________________________________________________________________________________________________________________________________________________

*/


        PathToObject pto = new PathToObject(task);
        pto.comparator = new TeamworkComparators.TaskManualOrderComparator();
        pto.canClick = TeamworkPermissions.task_canRead;

        PageSeed back = new PageSeed("taskList.jsp");
        ButtonLink taskList = new ButtonLink(I18n.get("TASKS"), back);
        pto.rootDestination = taskList;

        pto.destination = pageState.thisPage(request);
        pto.destination.setCommand(Commands.EDIT);        
        pto.toHtml(pageContext);


  PercentileDisplay progressBar = task.bricks.getProgressBar(false);
  progressBar.width="100px";
  progressBar.height="15px";

  StatusIcon statusIcon = task.bricks.getStatusIcon(10, pageState);

      %>
<div class="summary">
  <div style="width:100px;float:left;margin-right:20px"><%progressBar.toHtml(pageContext);%></div>
  <div class="pathCodeWrapper"><%statusIcon.toHtml(pageContext);%><span class='pathCode' title='<%=I18n.get("REFERENCE_CODE")%>'><%=(task.isNew() ? "" : "T#" + task.getMnemonicCode())%>#</span></div>
</div>


<jsp:include page="../issue/partIssueGraphs.jsp"/><%


f.end(pageContext);

    }
%>