<%@ page import="com.twproject.task.Assignment, com.twproject.task.AssignmentPriority, com.twproject.task.Task, com.twproject.task.TaskBricks,
org.jblooming.operator.Operator, org.jblooming.utilities.DateUtilities, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink,
org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.input.ColorValueChooser, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed,
org.jblooming.waf.view.PageState, java.util.Date, com.twproject.security.TeamworkPermissions" %><%

  PageState pageState = PageState.getCurrentPageState(request);

  Operator logged= pageState.getLoggedOperator();
  JspHelper includer = (JspHelper) JspHelper.getCurrentInstance(request);
  Assignment ass = (Assignment) includer.parameters.get("ass");
  Task task = (Task) includer.parameters.get("task");
  long time=(Long)  includer.parameters.get("time");

  boolean canManage=ass.hasPermissionFor(logged, TeamworkPermissions.resource_manage);


AssignmentPriority ap = ass.getAssignmentPriorityAtTime(time);

int actualPriority = ass.getPriorityAtTime(time);
  PageSeed psEdTask = pageState.pageFromRoot("task/taskAssignmentList.jsp");
  psEdTask.command = Commands.EDIT;
  psEdTask.mainObjectId = ass.getId();
  psEdTask.addClientEntry("TASK_ID",task);

%>


<td class="assPriorityCell" style="height: 50px; vertical-align: top">


  <table class="table" border="0">
    <tr>

      <td width="25" valign="top" valign="top"><%
        pageState.addClientEntry("AP_" + ass.getId()+""+time,actualPriority);
        ColorValueChooser chooser = TaskBricks.getAssignmentPriorityCombo("AP_" + ass.getId()+""+time, 19, pageState);
        chooser.readOnly=!canManage;
        chooser.displayValue=false;
        //chooser.showOpener=true;
        chooser.script="ass=\""+ass.getId()+"\" time=\""+time+"\"";
        chooser.onChangeScript="priorityChanged(hidden)";
        chooser.toHtml(pageContext);%>
        <div style="width: 100%">
          <%

            if (ap != null && ap.getCutPoint() != time) {
              String txt = I18n.get("LAST_PRIORITY_CHANGE_ON") + " " + DateUtilities.dateToString(new Date(ap.getCutPoint()));
              ButtonJS gocT = new ButtonJS("goToMillis("+ap.getCutPoint() +");");
              gocT.toolTip = txt;
              gocT.iconChar="v";
              gocT.additionalCssClass="small";
              gocT.toHtmlInTextOnlyModality(pageContext);
            }

            if (ap != null && ap.getCutPoint() == time) {
              ButtonJS goc = new ButtonJS("clearFromNowOn(this,"+ass.getId()+","+ap.getCutPoint() +");");
              goc.toolTip = I18n.get("PRIORITY_RESET_FROM_NOW");
              goc.iconChar="h";
              goc.enabled=canManage;
              goc.additionalCssClass="small";
              goc.toHtmlInTextOnlyModality(pageContext);
            }
          %></div>
      </td>
      <td valign="top" class="linkCell">
        <%ButtonLink taskLink = ButtonLink.getTextualInstance(task.bricks.getDisplayName(), psEdTask);
          taskLink.enabled=task.hasPermissionFor(logged,TeamworkPermissions.task_canRead);
          taskLink.toHtml(pageContext);%>
      </td>



      </tr>
  </table>

</td>


