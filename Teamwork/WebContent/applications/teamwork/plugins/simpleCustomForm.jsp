<%@ page import=" com.twproject.resource.Person,
                 com.twproject.task.Task,
                 org.jblooming.designer.Designer,
                 org.jblooming.designer.DesignerField,
                 org.jblooming.designer.Detail,
                 org.jblooming.ontology.PersistentFile,
                 org.jblooming.persistence.PersistenceHome,
                 org.jblooming.utilities.CodeValue,
                 org.jblooming.utilities.CodeValueList,
                 org.jblooming.utilities.DateUtilities,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.PagePlugin,
                 org.jblooming.waf.PluginBricks,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.container.Container,
                 org.jblooming.waf.html.core.JspIncluder,
                 org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.html.display.PercentileDisplay,
                 org.jblooming.waf.view.PageState,
                 java.util.Date, org.jblooming.waf.html.input.SQLCombo, com.twproject.security.TeamworkPermissions" %><%@ page contentType="text/html; charset=utf-8" pageEncoding="UTF-8" %><%!

  /**
   * This inner class is used by Twproject to know if this form applies to current context.
   * PagePlugin classes are loaded at startup (or by hand) in memory to be performant.
   *
   */
  public class PagePluginExt extends PagePlugin {
    public boolean isVisibleInThisContext(PageState pagestate) {
      boolean ret = false;
      if (pagestate.getMainObject() != null && pagestate.getMainObject().getClass().equals(Task.class)) {

        Task task = (Task) pagestate.getMainObject();
        
        ret = "Twproject test form".equalsIgnoreCase(task.getName());

        // ----- begin test condition on task -----------------
        // this form will be visible only on root tasks
        //ret = task.getType() != null && task.getType().getDescription().equalsIgnoreCase("SEDE");
        // ----- end test condition on task -----------------
      }
      return ret;
    }
  }
%><%
  /*
  * Each custom form is composed by two parts called in different application life-cycle.
  *
  * The first part is the initialization. This part is called at startup and injects PagePlugin class in the system.
  * The PagePluginExt.isVisibleInThisContext method is called every time Teamworks is creating links for plugins.
  *
  * The second part is the definition of the form. Definition is composed of two parts: form data definition and form html layout.
  */
  // #########################################################################  BEGIN INITIALIZE  #########################################################################
  if (JspIncluder.INITIALIZE.equals(request.getParameter(Commands.COMMAND))) {
    PluginBricks.getPagePluginInstance("TASK_FORMS", new PagePluginExt(), request);

    // #########################################################################  END INITIALIZE  #########################################################################
  } else if (Designer.DRAW_FORM.equals(request.getAttribute(JspIncluder.ACTION))) {
    // ------- recover page model and objects -----  BEGIN DO NOT MOFIFY --------------
    PageState pageState = PageState.getCurrentPageState(request);
    Task task = (Task) PersistenceHome.findByPrimaryKey(Task.class, pageState.mainObjectId);
    Designer designer = (Designer) JspIncluderSupport.getCurrentInstance(request);
    // ------- recover page model and objects -----  END DO NOT MOFIFY --------------

    // check security and set read_only modality
    designer.readOnly = !task.hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.task_canWrite);

    // #########################################################################  BEGIN FORM DATA DEFINITION  #########################################################################
    if (designer.fieldsConfig) {

      CodeValueList cvl = new CodeValueList();
      cvl.add("0", "list value 0");
      cvl.add("1", "list value 1");
      cvl.add("2", "list value 2");
      cvl.add("3", "list value 3");
      cvl.add("4", "list value 4");
      DesignerField dfr = new DesignerField(CodeValue.class.getName(), "RADIO", "Checklist Example as radio", false, false, null);
      dfr.separator = "<br>";
      dfr.cvl = cvl;
      dfr.displayAsCombo = false;
      designer.add(dfr);

      DesignerField dfl = new DesignerField(CodeValue.class.getName(), "COMBO", "Checklist Example as list", false, false, null);
      dfl.separator = "<br>";
      dfl.cvl = cvl;
      dfl.displayAsCombo = true;
      designer.add(dfl);


      DesignerField dfStr = new DesignerField(String.class.getName(), "STRING", "String example", false, false, "preloaded value");
      dfStr.separator = "<br>";
      dfStr.fieldSize = 20;
      designer.add(dfStr);

      DesignerField dfNote = new DesignerField(String.class.getName(), "NOTES", "Text example (limited to 2000)", false, false, "");
      dfNote.fieldSize = 80;
      dfNote.rowsLength = 5;
      dfNote.separator = "<br>";
      designer.add(dfNote);

      DesignerField dfInt = new DesignerField(Integer.class.getName(), "INTEGER", "Integer example", false, false, "");
      dfInt.separator = "<br>";
      dfInt.fieldSize = 4;
      designer.add(dfInt);

      DesignerField dfdouble = new DesignerField(Double.class.getName(), "DOUBLE", "Double example", false, false, "");
      dfdouble.separator = "<br>";
      dfdouble.fieldSize = 4;
      designer.add(dfdouble);


      DesignerField dfdate = new DesignerField(Date.class.getName(), "DATE", "Date example", false, false, null);
      dfdate.separator = "<br>";
      designer.add(dfdate);

      DesignerField dffile = new DesignerField(PersistentFile.class.getName(), "FILE", "Upload example", false, false, null);
      dffile.fieldSize = 40;
      dffile.separator = "<br>";
      designer.add(dffile);

      DesignerField dfperson = new DesignerField(Person.class.getName(), "PERSON", "Any persistent (Identifiable) object example, here Person", false, false, null);
      dfperson.separator = "<br>";
      dfperson.fieldSize = 40;
      designer.add(dfperson);


      DesignerField dfbool = new DesignerField(Boolean.class.getName(), "BOOLEAN", "Check if agree", false, false, "");
      dfbool.separator="&nbsp;";
      designer.add(dfbool);


      DesignerField sqlcombo = DesignerField.getSQLComboInstance("SQLCOMBO", "Sql combo", "select id,loginName from olpl_operator", "where loginName like ? order by loginName", "where id = ?");
      sqlcombo.separator="<br>";
      sqlcombo.label="Sql combo <small>select id,loginName from olpl_operator</small>";
      designer.add(sqlcombo);


      // Master Detail example. You can add a detail to the form and then add field to detail.
      Detail detail = designer.addDetail("DETAIL");
      detail.label = "Master-Detail example";
      DesignerField dfitem = new DesignerField(String.class.getName(), "ITEM", "Item", false, false, "");
      dfitem.fieldSize=55;
      detail.add(dfitem);


      DesignerField dftask = new DesignerField(Task.class.getName(), "TASK", "Task", false, false, null);
      dftask.separator = "</td><td>";
      dftask.fieldSize = 50;
      detail.add(dftask);


      DesignerField dfqty = new DesignerField(Integer.class.getName(), "QTY", "Qty", false, false, "");
      dfqty.fieldSize = 4;
      detail.add(dfqty);

      detail.add(dfl);


      // #########################################################################  END FORM DATA DEFINITION  ###################################################################


    } else {
      // #########################################################################  BEGIN FORM LAYOUT DEFINITION  ###############################################################



      // you can extract data to enrich your form using data from current task.
      // In this case we will extract missing days from current task
      String daysMissing = pageState.getI18n("UNSPECIFIED");
      if (task.getSchedule() != null && task.getSchedule().getEndDate() != null) {
        if (task.getSchedule().getValidityEndTime() > new Date().getTime()) {
          long missing = task.getSchedule().getValidityEndTime() - new Date().getTime();
          daysMissing = DateUtilities.getMillisInDaysHoursMinutes(missing);
        } else
          daysMissing = "<b>" + pageState.getI18n("OVERDUE") + "</b>";
      }
%>


<h1><Custom form DEMO for task: <%=task.getDisplayName()%></h1>
<%-- -------------------  BEGIN TASK DATA ----------------- You can use the task recovered before to display cue data   --%>
<br>
<table border="1" class="table">
  <tr>
    <th colspan="4" class="tableHead"> Some data from current task:</th>
  </tr>
  <tr>
    <td ><label><%=pageState.getI18n("RELEVANCE")%>:</label> <%=task.getRelevance()%></td>
    <td><label><%=pageState.getI18n("TASK_END")%>:</label> <%=task.getSchedule() != null && task.getSchedule().getEndDate() != null ? JSP.w(task.getSchedule().getEndDate()) : "&nbsp;-&nbsp;"%></td>
    <td><label><%=pageState.getI18n("TASK_REMAINING")%>:</label> <%=daysMissing%></td>
    <td><label><%=pageState.getI18n("PROGRESS")%>:</label> <%
      PercentileDisplay pd = task.bricks.getProgressBar();
      pd.toHtml(pageContext);
    %>
    </td>
  </tr>
</table>
<%-- -------------------  END TASK DATA -----------------  --%>
<hr><br>
  <%-- -------------------  BEGIN HTML GRID ----------------- --%>
    <table border="0"  cellpadding="5" cellspacing="0" class="table">

      <tr>
        <td colspan="4"><%designer.draw("RADIO", pageContext);%></td>
      </tr>

      <tr>
        <td><%designer.draw("COMBO", pageContext);%></td>
        <td><%designer.draw("STRING", pageContext);%></td>
      </tr>

      <tr>
        <td colspan="4"><%designer.draw("NOTES", pageContext);%>
          <br>
          <%designer.draw("BOOLEAN", pageContext);%>
        </td>
      </tr>

      <tr>
        <td><%designer.draw("INTEGER", pageContext);%></td>
        <td><%designer.draw("DOUBLE", pageContext);%></td>
      </tr>

      <tr>
        <td><%designer.draw("DATE", pageContext);%></td>
        <td><%designer.draw("PERSON", pageContext);%></td>
      </tr>

      <tr>
        <td><%designer.draw("FILE", pageContext);%></td>
        <td><%designer.draw("SQLCOMBO", pageContext);%></td>
      </tr>
    </table>

    <table><tr><td><%designer.draw("DETAIL", pageContext);%></td></tr></table>

  <%-- -------------------  END HTML GRID ----------------- --%>

      <%
        double testUseValues = 0;

        //sum of weights
        testUseValues  += designer.getEntry("INTEGER", pageState).intValueNoErrorCodeNoExc();
        testUseValues  += designer.getEntry("DOUBLE", pageState).doubleValueNoErrorNoCatchedExc();

      %>
      <hr>
        <b><big>Test of sum of stored values:&nbsp;<%=JSP.w(testUseValues)%></big></b>
      <%
    }
   // #########################################################################  END FORM LAYOUT DEFINITION  ####################################################################
  }
%>