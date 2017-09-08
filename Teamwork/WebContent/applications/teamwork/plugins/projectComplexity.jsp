<%@ page import=" com.twproject.task.Task,
                 org.jblooming.designer.Designer,
                 org.jblooming.designer.DesignerField,
                 org.jblooming.persistence.PersistenceHome,
                 org.jblooming.utilities.CodeValue,
                 org.jblooming.utilities.CodeValueList,
                 org.jblooming.utilities.DateUtilities,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.PagePlugin,
                 org.jblooming.waf.PluginBricks,
                 org.jblooming.waf.SessionState,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.container.Container,
                 org.jblooming.waf.html.core.JspIncluder,
                 org.jblooming.waf.html.core.JspIncluderSupport,
                 org.jblooming.waf.html.display.PercentileDisplay,
                 org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState,
                 java.util.Date, com.twproject.security.TeamworkPermissions"
        %><%@ page contentType="text/html; charset=utf-8" pageEncoding="UTF-8" %><%!

  public class ProjectComplexity extends PagePlugin {

    public boolean isVisibleInThisContext(PageState pagestate) {
      boolean ret = false;
      if (pagestate.getMainObject() != null && pagestate.getMainObject().getClass().equals(Task.class)) {
        Task task = (Task) pagestate.getMainObject();
        ret = "Twproject test form".equalsIgnoreCase(task.getName());
      }
      return ret;
    }
  }
%><%

  if (JspIncluder.INITIALIZE.equals(request.getParameter(Commands.COMMAND))) {

    PluginBricks.getPagePluginInstance("TASK_FORMS", new ProjectComplexity(), request);

  } else {

    PageState pageState = PageState.getCurrentPageState(request);

    Task task = (Task) PersistenceHome.findByPrimaryKey(Task.class, pageState.mainObjectId);
    Designer designer = (Designer) JspIncluderSupport.getCurrentInstance(request);
    designer.readOnly = !task.hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.task_canWrite);
    if (Designer.DRAW_FORM.equals(request.getAttribute(JspIncluder.ACTION))) {
      if (designer.fieldsConfig) {

        CodeValueList cvl = new CodeValueList();
        cvl.add("20", "Mono directional or mono site");
        cvl.add("40", "Multi directional or multi site");
        cvl.add("60", "Internal company");
        cvl.add("80", "Field-force");
        cvl.add("100", "External company");
        DesignerField df = new DesignerField(CodeValue.class.getName(), "AREAS", "<b><big>1. Areas and/or managers involved</big></b>", false, false, null);
        df.separator = "<br>";
        df.cvl = cvl;
        designer.add(df);

        DesignerField dfROI = new DesignerField(Double.class.getName(), "AREAS_WEIGHT", "Weight", false, false, "10");
        dfROI.fieldSize = 4;
        designer.add(dfROI);

        cvl = new CodeValueList();
        cvl.add("50", "Internal");
        cvl.add("100", "External");
        df = new DesignerField(CodeValue.class.getName(), "SPEC_COMP", "<b><big>2. There are specialized competences</big></b>", false, false, null);
        df.separator = "<br>";
        df.cvl = cvl;
        designer.add(df);

        dfROI = new DesignerField(Double.class.getName(), "SPEC_COMP_WEIGHT", "Weight", false, false, "10");
        dfROI.fieldSize = 4;
        designer.add(dfROI);

        cvl = new CodeValueList();
        cvl.add("20", "Less than 20");
        cvl.add("40", "Between 20 and 50");
        cvl.add("60", "Between 50 and 100");
        cvl.add("80", "Between 100 and 400");
        cvl.add("100", "More than 400");
        df = new DesignerField(CodeValue.class.getName(), "STAFF", "<b><big>3. Staff involved (internal or external)</big></b>", false, false, null);
        df.separator = "<br>";
        df.cvl = cvl;
        designer.add(df);

        dfROI = new DesignerField(Double.class.getName(), "STAFF_WEIGHT", "Weight", false, false, "10");
        dfROI.fieldSize = 4;
        designer.add(dfROI);

        cvl = new CodeValueList();
        cvl.add("0", "Not applicable");
        cvl.add("20", "Less than 10");
        cvl.add("40", "Between 20 and 30");
        cvl.add("60", "Between 30 and 100");
        cvl.add("100", "More than 100");
        df = new DesignerField(CodeValue.class.getName(), "TARGET_PERSON", "<b><big>4. Target scope</big></b>", false, false, null);
        df.separator = "<br>";
        df.cvl = cvl;
        designer.add(df);

        dfROI = new DesignerField(Double.class.getName(), "TARGET_PERSON_WEIGHT", "Weight", false, false, "10");
        dfROI.fieldSize = 4;
        designer.add(dfROI);

        cvl = new CodeValueList();
        cvl.add("0", "Does not apply");
        cvl.add("25", "low");
        cvl.add("50", "medium");
        cvl.add("100", "high");
        df = new DesignerField(CodeValue.class.getName(), "QUALITY_REQUIRED", "<b><big>5. Quality required</big></b>", false, false, null);
        df.separator = "<br>";
        df.cvl = cvl;
        designer.add(df);

        dfROI = new DesignerField(Double.class.getName(), "QUALITY_REQUIRED_WEIGHT", "Weight", false, false, "10");
        dfROI.fieldSize = 4;
        designer.add(dfROI);

        cvl = new CodeValueList();
        cvl.add("0", "Does not apply");
        cvl.add("50", "short");
        cvl.add("100", "long");
        df = new DesignerField(CodeValue.class.getName(), "DURATION", "<b><big>6. Duration</big></b>", false, false, null);
        df.separator = "<br>";
        df.cvl = cvl;
        designer.add(df);

        dfROI = new DesignerField(Double.class.getName(), "DURATION_WEIGHT", "Weight", false, false, "10");
        dfROI.fieldSize = 4;
        designer.add(dfROI);

        cvl = new CodeValueList();
        cvl.add("0", "Not applicable");
        cvl.add("20", "Less than 25.000");
        cvl.add("40", "Between 25.000 and 100.000");
        cvl.add("60", "Between 100.000 and 200.000");
        cvl.add("80", "Between 200.000 and 500.000");
        cvl.add("100", "Above 500.000");
        df = new DesignerField(CodeValue.class.getName(), "TOTAL_COSTS", "<b><big>7. Total costs</big></b>", false, false, null);
        df.separator = "<br>";
        df.cvl = cvl;
        designer.add(df);

        dfROI = new DesignerField(Double.class.getName(), "TOTAL_COSTS_WEIGHT", "Weight", false, false, "30");
        dfROI.fieldSize = 4;
        designer.add(dfROI);

        cvl = new CodeValueList();
        cvl.add("0", "Does not apply");
        cvl.add("50", "locally");
        cvl.add("100", "internationally");
        df = new DesignerField(CodeValue.class.getName(), "DEPENDENCIES", "<b><big>8. Interdependencies with other projects</big></b>", false, false, null);
        df.separator = "<br>";
        df.cvl = cvl;
        designer.add(df);

        dfROI = new DesignerField(Double.class.getName(), "DEPENDENCIES_WEIGHT", "Weight", false, false, "10");
        dfROI.fieldSize = 4;
        designer.add(dfROI);


      } else {

        //days missing
        String daysMissing = I18n.get("UNSPECIFIED");
        if (task.getSchedule() != null && task.getSchedule().getEndDate() != null) {
          if (task.getSchedule().getValidityEndTime() > new Date().getTime()) {
            long missing = task.getSchedule().getValidityEndTime() - new Date().getTime();
            daysMissing = DateUtilities.getMillisInDaysHoursMinutes(missing);
          } else
            daysMissing = "<b>" + I18n.get("OVERDUE") + "</b>";
        }


%>
<h1>Project Complexity: <%=task.getDisplayName()%></h1>
<table border="0" class="table" cellpadding="3">
 <thead>
  <tr>
    <th class="tableHead"><%=I18n.get("RELEVANCE")%>
    </th>
    <th class="tableHead"><%=I18n.get("TASK_END")%>
    </th>
    <th class="tableHead"><%=I18n.get("TASK_REMAINING")%>
    </th>
    <th class="tableHead"><%=I18n.get("PROGRESS")%>
    </th>
  </tr>
 </thead>
  <tr class="alternate">
    <td align="center"><%=task.getRelevance()%>
    </td>
    <td align="center"><%=task.getSchedule() != null && task.getSchedule().getEndDate() != null ? JSP.w(task.getSchedule().getEndDate()) : "&nbsp;-&nbsp;"%>
    </td>
    <td align="center"><%=daysMissing%>
    </td>
    <td align="center"><%
      PercentileDisplay pd = task.bricks.getProgressBar();
      pd.width="100px";
      pd.toHtml(pageContext);
    %>
    </td>
  </tr>

</table>
<br>
<table class="table">

  <tr class="alternate">
    <td><%designer.draw("AREAS", pageContext);%></td>
    <td><%designer.draw("AREAS_WEIGHT", pageContext);%></td>
  </tr>
  <tr class="alternate">
    <td><%designer.draw("SPEC_COMP", pageContext);%></td>
    <td><%designer.draw("SPEC_COMP_WEIGHT", pageContext);%></td>
  </tr>

  <tr class="alternate">
    <td><%designer.draw("STAFF", pageContext);%></td>
    <td><%designer.draw("STAFF_WEIGHT", pageContext);%></td>
  </tr>

  <tr class="alternate">
    <td><%designer.draw("TARGET_PERSON", pageContext);%></td>
    <td><%designer.draw("TARGET_PERSON_WEIGHT", pageContext);%></td>
  </tr>
  <tr class="alternate">
    <td><%designer.draw("QUALITY_REQUIRED", pageContext);%></td>
    <td><%designer.draw("QUALITY_REQUIRED_WEIGHT", pageContext);%></td>
  </tr>

  <tr class="alternate">
    <td><%designer.draw("DURATION", pageContext);%></td>
    <td><%designer.draw("DURATION_WEIGHT", pageContext);%></td>
  </tr>

  <tr class="alternate">
    <td><%designer.draw("TOTAL_COSTS", pageContext);%></td>
    <td><%designer.draw("TOTAL_COSTS_WEIGHT", pageContext);%></td>
  </tr>

  <tr class="alternate">
    <td><%designer.draw("DEPENDENCIES", pageContext);%></td>
    <td><%designer.draw("DEPENDENCIES_WEIGHT", pageContext);%></td>
  </tr>


  <%

    double projectValue = 0;

    //sum of weights
    double w = designer.getEntry("AREAS_WEIGHT", pageState).doubleValueNoErrorNoCatchedExc();
    w = w + designer.getEntry("SPEC_COMP_WEIGHT", pageState).doubleValueNoErrorNoCatchedExc();
    w = w + designer.getEntry("TARGET_PERSON_WEIGHT", pageState).doubleValueNoErrorNoCatchedExc();
    w = w + designer.getEntry("QUALITY_REQUIRED_WEIGHT", pageState).doubleValueNoErrorNoCatchedExc();
    w = w + designer.getEntry("DURATION_WEIGHT", pageState).doubleValueNoErrorNoCatchedExc();
    w = w + designer.getEntry("STAFF_WEIGHT", pageState).doubleValueNoErrorNoCatchedExc();
    w = w + designer.getEntry("TOTAL_COSTS_WEIGHT", pageState).doubleValueNoErrorNoCatchedExc();
    w = w + designer.getEntry("DEPENDENCIES_WEIGHT", pageState).doubleValueNoErrorNoCatchedExc();

    double pickedValue = designer.getEntry("AREAS", pageState).intValueNoErrorCodeNoExc() * designer.getEntry("AREAS_WEIGHT", pageState).doubleValueNoErrorNoCatchedExc();
    pickedValue = pickedValue + designer.getEntry("SPEC_COMP", pageState).intValueNoErrorCodeNoExc() * designer.getEntry("SPEC_COMP_WEIGHT", pageState).doubleValueNoErrorNoCatchedExc();
    pickedValue = pickedValue + designer.getEntry("TARGET_PERSON", pageState).intValueNoErrorCodeNoExc() * designer.getEntry("TARGET_PERSON_WEIGHT", pageState).doubleValueNoErrorNoCatchedExc();
    pickedValue = pickedValue + designer.getEntry("QUALITY_REQUIRED", pageState).intValueNoErrorCodeNoExc() * designer.getEntry("QUALITY_REQUIRED_WEIGHT", pageState).doubleValueNoErrorNoCatchedExc();
    pickedValue = pickedValue + designer.getEntry("DURATION", pageState).intValueNoErrorCodeNoExc() * designer.getEntry("DURATION_WEIGHT", pageState).doubleValueNoErrorNoCatchedExc();
    pickedValue = pickedValue + designer.getEntry("STAFF", pageState).intValueNoErrorCodeNoExc() * designer.getEntry("STAFF_WEIGHT", pageState).doubleValueNoErrorNoCatchedExc();
    pickedValue = pickedValue + designer.getEntry("TOTAL_COSTS", pageState).intValueNoErrorCodeNoExc() * designer.getEntry("TOTAL_COSTS_WEIGHT", pageState).doubleValueNoErrorNoCatchedExc();
    pickedValue = pickedValue + designer.getEntry("DEPENDENCIES", pageState).intValueNoErrorCodeNoExc() * designer.getEntry("DEPENDENCIES_WEIGHT", pageState).doubleValueNoErrorNoCatchedExc();

    pickedValue = (pickedValue) / w;


  %>
  <tr>
    <td colspan="3" align="right"><b><big>Project value:&nbsp;<%=JSP.perc(pickedValue)%>
    </big></b></td>
  </tr>
</table>
<%

    }
  }
  }
%>