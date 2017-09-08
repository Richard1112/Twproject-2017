<%@ page import="com.twproject.operator.TeamworkOperator,
                 com.twproject.resource.ResourceBricks,
                 com.twproject.security.RoleTeamwork,
                 com.twproject.security.TeamworkPermissions,
                 com.twproject.task.Assignment,
                 com.twproject.task.Task,
                 com.twproject.task.TaskStatus,
                 com.twproject.waf.TeamworkPopUpScreen,
                 org.jblooming.agenda.CompanyCalendar,
                 org.jblooming.agenda.Period,
                 org.jblooming.operator.Operator,
                 org.jblooming.oql.OqlQuery,
                 org.jblooming.security.Area,
                 org.jblooming.waf.ActionUtilities,
                 org.jblooming.waf.PagePlugin,
                 org.jblooming.waf.PluginBricks,
                 org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.button.ButtonSubmit,
                 org.jblooming.waf.html.container.ButtonBar,
                 org.jblooming.waf.html.container.Container,
                 org.jblooming.waf.html.core.JspIncluder, org.jblooming.waf.html.input.SmartCombo, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.Date, com.twproject.task.TaskBricks" %>
<%@ page contentType="text/html; charset=utf-8" pageEncoding="UTF-8" %><%!
  public class CreaTaskWiz extends PagePlugin {

    public boolean isVisibleInThisContext(PageState pagestate) {
      Operator logged = pagestate.getLoggedOperator();
      boolean ret=false;
      //visible if you have rights
      if (logged!=null)
        ret = logged.hasPermissionFor(TeamworkPermissions.project_canCreate);

      //visible int the task list page
      ret=ret && pagestate.href.indexOf("taskList")>=0;
      
      return ret;
    }


  }

%>
<%

  // PARAMETERS ---- START ----------------------------------------------------------------------
  String projectManager = "Project manager";
  // PARAMETERS ---- END ----------------------------------------------------------------------


  if (JspIncluder.INITIALIZE.equals(request.getParameter(Commands.COMMAND))) {
    CreaTaskWiz pp = new CreaTaskWiz();
    //pp.setPixelHeight(1024);
    //pp.setPixelWidth(1280);
    PluginBricks.getPagePluginInstance("WIZARD", pp, request);


  } else {

    PageState pageState = PageState.getCurrentPageState(request);

    if (!pageState.screenRunning) {

      pageState.screenRunning = true;
      final ScreenArea body = new ScreenArea(request);
      TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
      lw.register(pageState);
      pageState.perform(request, response).toHtml(pageContext);

    } else {
      TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();
      logged.testPermission(TeamworkPermissions.project_canCreate);

      /**
       *   CONTROLLER
       */
      if (Commands.SAVE.equals(pageState.command)) {

        String code = pageState.getEntry("TASK_CODE").stringValueNullIfEmpty();
        String name = pageState.getEntry("TASK_NAME").stringValue();
        Area area = logged.getDefaultAreaForPermission(TeamworkPermissions.project_canCreate);
        Task task = new Task();
        task.setIdAsNew();
        task.setCode(code);
        task.setName(name);
        task.setArea(area);
        Period week = Period.getWeekPeriodInstance(new Date(), logged.getLocale());
        week.store();
        task.setSchedule(week);
        task.setDuration(CompanyCalendar.getDistanceInWorkingDays(week.getStartDate(),week.getEndDate()));
        task.setStatus(TaskStatus.STATUS_ACTIVE);

        task.store();


        //create assigs
        String hql = "select r from " + RoleTeamwork.class.getName() + " as r where r.area = :area and r.name = :name";

        String pm = pageState.getEntry("PRJMGR").stringValueNullIfEmpty();
        if (pm!=null) {
          Assignment assig = new Assignment();
          assig.setIdAsNew();
          ActionUtilities.setIdentifiable(pageState.getEntryAndSetRequired("PRJMGR"), assig, "resource");
          OqlQuery oql = new OqlQuery(hql);
          oql.getQuery().setEntity("area", area);
          oql.getQuery().setString("name", projectManager);
          RoleTeamwork r = (RoleTeamwork) oql.uniqueResult();
          assig.setRole(r);
          assig.setActivity(Assignment.ACTIVITY_ALL_IN_ONE);
          assig.setEnabled(true);
          assig.setTask(task);
          assig.store();

//        // -------------------- START the trick --------------------
//        task.setOwner(assig.getResource().getMyself());
//        task.store();
//        // -------------------- END the trick --------------------

        }


        // close and redirect
        PageSeed editTask = pageState.pageFromRoot("task/taskEditor.jsp");
        editTask.command = Commands.EDIT;
        editTask.mainObjectId = task.getId();

%><script type="text/javascript">
          var targetWin=getBlackPopupOpener();
          targetWin.location.href="<%=editTask.toLinkToHref()%>";
          targetWin.closeBlackPopup();
        </script><%



      } else {
        Container cont = new Container();
        cont.title = "Task creation wizard";
        cont.start(pageContext);

        PageSeed pageSeed = pageState.thisPage(request);
        pageSeed.command = "TASK_SAVE";
        Form form = new Form(pageSeed);
        form.start(pageContext);



%>
<table class="table" cellpadding="15" cellspacing="0">
  <tr>
    <td style="border-bottom:1px solid #e5e5e5"><%
      TextField code = new TextField("<b>2.</b>&nbsp;&nbsp; task code", "TASK_CODE", "</td><td style=\"border-bottom:1px solid #e5e5e5\">", 5, false);
      code.required = true;
      code.toHtml(pageContext);
    %></td>
  </tr>
  <tr>
    <td style="border-bottom:1px solid #e5e5e5"><%
      TextField name = new TextField("<b>3.</b>&nbsp;&nbsp; task name", "TASK_NAME", "</td><td style=\"border-bottom:1px solid #e5e5e5\">", 40, false);
      name.required = true;
      name.toHtml(pageContext);
    %></td>
  </tr>
  <tr>
    <td><%
      //SmartCombo sc = ResourceBricks.getPersonCombo("PRJMGR", TeamworkPermissions.assignment_manage, true, "", pageState);
      SmartCombo sc = TaskBricks.getAssignableResourceCombo(null, "PRJMGR", false, pageState);
      sc.label = "<b>5.</b>&nbsp;&nbsp; select project manager";
      sc.separator = "</td><td>";
      //sc.required = true;
      sc.toHtml(pageContext);
    %></td>
  </tr>
</table>
<%     cont.end(pageContext);

        ButtonBar bb = new ButtonBar();
        ButtonSubmit saveInstance = ButtonSubmit.getSaveInstance(form, "create");
        saveInstance.variationsFromForm.command = Commands.SAVE;
        bb.addButton(saveInstance);
        bb.toHtml(pageContext);



        form.end(pageContext);
      }
    }

  }

%>