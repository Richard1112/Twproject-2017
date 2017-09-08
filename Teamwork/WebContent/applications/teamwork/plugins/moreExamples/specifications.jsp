<%@ page import=" com.twproject.resource.Person,
                  com.twproject.task.Task,
                  org.jblooming.designer.Designer,
                  org.jblooming.designer.DesignerField,
                  org.jblooming.persistence.PersistenceHome,
                  org.jblooming.waf.PagePlugin,
                  org.jblooming.waf.PluginBricks,
                  org.jblooming.waf.SessionState,
                  org.jblooming.waf.constants.Commands,
org.jblooming.waf.constants.Fields,
org.jblooming.waf.html.container.Container, org.jblooming.waf.html.core.JspIncluder, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.view.PageState,
java.util.Map, java.util.TreeMap, com.twproject.security.TeamworkPermissions"%>
<%@ page contentType="text/html; charset=utf-8" pageEncoding="UTF-8"%><%!

  public class PagePluginExt extends PagePlugin {

    public boolean isVisibleInThisContext(PageState pagestate) {

      boolean ret = true;

      if (pagestate.getMainObject() != null && pagestate.getMainObject().getClass().equals(Task.class)) {

        Task task = (Task) pagestate.getMainObject();
        ret = task.getType() != null && "PRODUCTION".equalsIgnoreCase(task.getType().getDescription());
      }

      return ret;
    }
  }
%>
<%

  if (JspIncluder.INITIALIZE.equals(request.getParameter(Commands.COMMAND))) {

    PluginBricks.getPagePluginInstance("TASK_FORMS", new PagePluginExt(), request);

  } else {

    PageState pageState = PageState.getCurrentPageState(request);

    Task task = (Task) PersistenceHome.findByPrimaryKey(Task.class, pageState.mainObjectId);
    Designer designer = (Designer) JspIncluderSupport.getCurrentInstance(request);

    designer.readOnly = !task.hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.task_canWrite);

    Map<String, String> questions = new TreeMap();

    //setup
    questions.put("AS1", "Setup no Tomcat");
    questions.put("AS2", "Setup on port 81");
    //usage
    questions.put("B1", "Add second AD on resource");
    questions.put("B2", "Test interface in all languages");
    questions.put("B3", "Find issues without task or assignee ");
    //security
    questions.put("C1", "Test a user with no roles, only PM ");
    questions.put("C2", "Create objects with logged with no area");
    //layout
    questions.put("D1", "Smart combo reset a field");
    questions.put("D2", "Date in US and UK formats");


    if (Designer.DRAW_FORM.equals(request.getAttribute(JspIncluder.ACTION))) {
      if (designer.fieldsConfig) {

        DesignerField df = DesignerField.getIdentifiableInstance("TESTER", "tester", Person.class, new String[]{"personName", "personSurname"});
        df.separator = "&nbsp;";
        designer.add(df);

        for (String code : questions.keySet()) {
          df = new DesignerField(Boolean.class.getName(), "IE6_" + code, "IE6", false, false, Fields.FALSE);
          df.label = "";
          df.separator = "";
          designer.add(df);
          df = new DesignerField(Boolean.class.getName(), "IE7_" + code, "IE6", false, false, Fields.FALSE);
          df.label = "";
          df.separator = "";
          designer.add(df);
          df = new DesignerField(Boolean.class.getName(), "F1.5_" + code, "F1.5", false, false, Fields.FALSE);
          df.label = "";
          df.separator = "";
          designer.add(df);
          df = new DesignerField(Boolean.class.getName(), "Mac_" + code, "Mac", false, false, Fields.FALSE);
          df.label = "";
          df.separator = "";
          designer.add(df);
          df = new DesignerField(Boolean.class.getName(), "Lnx_" + code, "Lnx", false, false, Fields.FALSE);
          df.label = "";
          df.separator = "";
          designer.add(df);
        }

      } else {

        // layout
        Container c = new Container();
        c.title = "Test suite: " + task.getDisplayName();
        c.start(pageContext);

        designer.draw("TESTER", pageContext);

%> <table border="0" class="table">

      <tr><td colspan="2"><b><big>Tests</big></b></td></tr>

      <tr><th>test</th><th>IE6</th><th>IE7</th><th>F1.5</th><th>Mac</th><th>Lnx</th></tr><%

      for (String code : questions.keySet()) {
         %><tr class="alternate">
            <td><%=questions.get(code)%></td>
            <td align="center"><%designer.draw("IE6_"+code,pageContext);%></td>
            <td align="center"><%designer.draw("IE7_"+code,pageContext);%></td>
            <td align="center"><%designer.draw("F1.5_"+code,pageContext);%></td>
            <td align="center"><%designer.draw("Mac_"+code,pageContext);%></td>
            <td align="center"><%designer.draw("Lnx_"+code,pageContext);%></td>
         </tr><%
      }

    %> </table> <%

    c.end(pageContext);

  }
  }
  }
%>