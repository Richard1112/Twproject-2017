<%@ page import="org.jblooming.persistence.PersistenceHome, org.jblooming.persistence.objectEditor.FieldDrawer, org.jblooming.persistence.objectEditor.FieldFeature, org.jblooming.scheduler.Executable, org.jblooming.scheduler.Job,
                 org.jblooming.scheduler.Parameter, org.jblooming.scheduler.businessLogic.JobController, org.jblooming.utilities.ReflectionUtilities, org.jblooming.utilities.StringUtilities, org.jblooming.waf.ScreenBasic, org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.display.DeletePreviewer, org.jblooming.waf.html.input.CheckField,
                 org.jblooming.waf.html.input.ScheduleComposer, org.jblooming.waf.html.input.TextArea, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.ClientEntry, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.lang.reflect.Field, java.util.List, org.jblooming.waf.html.button.ButtonSupport" %><%

  PageState pageState = PageState.getCurrentPageState(request);
  pageState.getLoggedOperator().testIsAdministrator();

  if (!pageState.screenRunning) {
    ScreenBasic.preparePage(new JobController(), pageContext);
    pageState.perform(request, response).toHtml(pageContext);
  } else {

    Job job = (Job) pageState.getMainObject();
    PageSeed ps = pageState.thisPage(request);
    ps.setCommand(Commands.SAVE);
    if (job == null)
      ps.setMainObjectId(PersistenceHome.NEW_EMPTY_ID);
    else
      ps.setMainObjectId(job.getId());
    Form form = new Form(ps);
    form.alertOnChange=true;
    form.start(pageContext);

%><script>$("#ADMINISTRATION_ROOT_MENU").addClass('selected');</script>
<h1><a href="scheduleManager.jsp">jobs</a> /  <%= (job.getName() != null ? job.getName() : I18n.get("NEW"))%></h1>
<div class="container level_2">
<table class="table" cellpadding="0" cellspacing="0" border="0">
  <tr>
    <td >
      <table cellpadding="5" cellspacing="0">
        <tr><% TextField tf = new TextField(I18n.get("NAME"), "NAME", "<br>", 40, false);
            tf.required=true;
        %> <td><%tf.toHtml(pageContext);%></td></tr>
        <tr>
          <%
            TextArea ta = new TextArea(I18n.get("DESCRIPTION"), "DESCRIPTION", "<br>", 40, 2, null);
          %> <td><%ta.toHtml(pageContext);%></td></tr>
        <tr><td>
          <% tf = new TextField(I18n.get("AGE_ESTIMATED_DURATION"), "ESTIMATED_DURATION", "<br>", 5, false); %> <%tf.toHtml(pageContext);%>&nbsp;&nbsp;&nbsp;
        </td></tr><tr><td>
          <% tf = new TextField(I18n.get("timeoutTime"), "timeoutTime", "<br>", 5, false);%> <%tf.toHtml(pageContext);%>
        </td></tr>

        <tr><td colspan="2">
          <%new CheckField(I18n.get("ENABLED"), "enabled", "&nbsp;", false).toHtml(pageContext);%>
          <%new CheckField(I18n.get("onErrorRetryNow"), "onErrorRetryNow", "&nbsp;", false).toHtml(pageContext);%>
          <%new CheckField(I18n.get("onErrorSuspendScheduling"), "onErrorSuspendScheduling", "&nbsp;", false).toHtml(pageContext);%>
        </td>
        </tr>

        <tr><td>&nbsp;</td></tr>
        <tr><% tf = new TextField(I18n.get("LAUNCHER_CLASS"), "LAUNCHER_CLASS", "<br>", 50, false);
        %> <td><%tf.toHtml(pageContext);%></td></tr>


        <tr><td colspan="4"><%=I18n.get("SCHEDULER_CLASS_HELP")%>
        </td></tr>
        <%
          if (!(PersistenceHome.NEW_EMPTY_ID.equals(job.getId()))) {

            ClientEntry clazz = pageState.getEntry("LAUNCHER_CLASS");

            if (clazz.stringValueNullIfEmpty() != null) {

              try {
                Class theClass = Class.forName(clazz.stringValue());
                if (ReflectionUtilities.getInheritedClasses(theClass).contains(Executable.class)) {
                  List<Field> flds = ReflectionUtilities.getDeclaredInheritedParameterFields(theClass, Parameter.class);

                  for (Field field : flds) {

                    String defaultValue = ((Parameter) field.getAnnotation(Parameter.class)).value();
                    //should not do make for existing but empty ces
                    if (pageState.getEntry(field.getName()).name==null && pageState.getEntry(field.getName()).stringValueNullIfEmpty() == null && defaultValue != null) {
                      pageState.addClientEntry(field.getName(), defaultValue);
                    }

                    FieldFeature fieldFeature = new FieldFeature(field.getName(), StringUtilities.deCamel(field.getName()));
                    FieldDrawer fd = new FieldDrawer(fieldFeature, theClass, form);
                    fd.autoSize = true;

        %>
        <tr><%fd.toHtml(pageContext);%></tr>
        <%
          }

        } else {
        %>
        <tr><td colspan="4"> <span class="warning"><%=I18n.get("CLASS_MUST_IMPLEMENT_EXECUTABLE")%></span> </td></tr>
        <%
          }
        } catch (ClassNotFoundException e) {
        %>
        <tr><td colspan="4"> <span class="warning"><%=I18n.get("CLASS_NOT_FOUND_IN_SERVER_PATH")%></span> </td></tr>
        <%
              }

            }
          }%>

      </table>
    </td>


    <td valign="top" rowspan="2" width="730">
      <%
        ScheduleComposer c1 = new ScheduleComposer("schedule");
        c1.isMinute = true;
        c1.toHtml(pageContext);
      %>
    </td>
  </tr>
</table>

<%
  ButtonBar bb = new ButtonBar();

  PageSeed sm = new PageSeed("scheduleManager.jsp");
  ButtonLink smlist = new ButtonLink(sm);
  smlist.label = I18n.get("SCHEDULER");
  bb.addButton(smlist);

  PageSeed ls = pageState.pageInThisFolder("jobList.jsp", request);
  ls.setCommand(Commands.FIND);
  ButtonLink list = new ButtonLink(ls);
  list.label = I18n.get("RETURN_TO_LIST");
  bb.addButton(list);

  if (job != null && !PersistenceHome.NEW_EMPTY_ID.equals(job.getId())) {
    DeletePreviewer deletePreviewer = new DeletePreviewer("JOB_DEL", JobController.class, pageState);
    ButtonSupport delPrev = deletePreviewer.getDeleteButton(I18n.get("DELETE"), job.getId() );
    delPrev.additionalCssClass="big delete";
    delPrev.additionalCssClass="lreq30 lreqLabel";
    bb.addButton(delPrev);

    ButtonSubmit bs = new ButtonSubmit(form);
    bs.variationsFromForm.setCommand("RUN_NOW");
    bs.label = I18n.get("RUN_NOW");
    bb.addButton(bs);

  }

  ButtonJS breset = new ButtonJS();
  breset.label = I18n.get("RESET");
  breset.onClickScript = form.getUniqueName() + ".reset();";
  bb.addButton(breset);

  ButtonSubmit bs = ButtonSubmit.getSaveInstance(form, I18n.get("SAVE"));
  bb.addButton(bs);

  bb.toHtml(pageContext);


%></div>
<%
    form.end(pageContext);

  }
%>