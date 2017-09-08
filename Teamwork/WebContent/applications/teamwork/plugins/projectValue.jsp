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
                 org.jblooming.waf.view.PageState, java.util.Date, org.jblooming.waf.settings.I18n, com.twproject.security.TeamworkPermissions" %><%@ page contentType="text/html; charset=utf-8" pageEncoding="UTF-8" %><%!

  public class PagePluginExt extends PagePlugin {

    public boolean isVisibleInThisContext(PageState pagestate) {

      boolean ret = false;

      if (pagestate.getMainObject() != null && pagestate.getMainObject().getClass().equals(Task.class)) {
        Task task = (Task) pagestate.getMainObject();
        ret = "Twproject test form".equalsIgnoreCase(task.getName());
        //ret = task.getParent() == null && task.getRelevance()>=80;
      }

      return ret;
    }
  }
%><%

  if (JspIncluder.INITIALIZE.equals(request.getParameter(Commands.COMMAND))) {

    PluginBricks.getPagePluginInstance("TASK_FORMS", new PagePluginExt(), request);

  } else {

  PageState pageState = PageState.getCurrentPageState(request);

  Task task = (Task) PersistenceHome.findByPrimaryKey(Task.class, pageState.mainObjectId);
  Designer designer = (Designer) JspIncluderSupport.getCurrentInstance(request);

  designer.readOnly = !task.hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.task_canWrite);

  if (Designer.DRAW_FORM.equals(request.getAttribute(JspIncluder.ACTION))) {
    if (designer.fieldsConfig) {

      CodeValueList cvl = new CodeValueList();
      cvl.add("0", "Not calculated or does not apply");
      cvl.add("1", "less than 15%");
      cvl.add("2", "between 16% and 30%");
      cvl.add("3", "greater than 30%");
      DesignerField df = new DesignerField(CodeValue.class.getName(), "ROI", "<b><big>1. Return on investment (ROI)</big></b>", false, false, null);
      df.separator = "<br>";
      df.cvl = cvl;
      designer.add(df);

      DesignerField dfROI = new DesignerField(Double.class.getName(), "ROI_WEIGHT", "Weight", false, false, "20");
      dfROI.fieldSize = 4;
      designer.add(dfROI);

      cvl = new CodeValueList();
      cvl.add("0", "Does not apply");
      cvl.add("1", "low");
      cvl.add("2", "medium");
      cvl.add("3", "high");
      df = new DesignerField(CodeValue.class.getName(), "COST_CUTS", "<b><big>2. Cost cuts</big></b>", false, false, null);
      df.separator = "<br>";
      df.cvl = cvl;
      designer.add(df);

      dfROI = new DesignerField(Double.class.getName(), "COST_CUTS_WEIGHT", "Weight", false, false, "20");
      dfROI.fieldSize = 4;
      designer.add(dfROI);

      cvl = new CodeValueList();
      cvl.add("0", "Does not apply");
      cvl.add("1", "low");
      cvl.add("2", "medium");
      cvl.add("3", "high");
      df = new DesignerField(CodeValue.class.getName(), "SALES_GROWTH", "<b><big>3. Sales growth</big></b>", false, false, null);
      df.separator = "<br>";
      df.cvl = cvl;
      designer.add(df);

      dfROI = new DesignerField(Double.class.getName(), "SALES_GROWTH_WEIGHT", "Weight", false, false, "20");
      dfROI.fieldSize = 4;
      designer.add(dfROI);

      cvl = new CodeValueList();
      cvl.add("0", "Does not apply");
      cvl.add("1", "low");
      cvl.add("2", "medium");
      cvl.add("3", "high");
      df = new DesignerField(CodeValue.class.getName(), "MARKET_SHARE_GROWTH", "<b><big>4. Market share growth</big></b>", false, false, null);
      df.separator = "<br>";
      df.cvl = cvl;
      designer.add(df);

      dfROI = new DesignerField(Double.class.getName(), "MARKET_SHARE_GROWTH_WEIGHT", "Weight", false, false, "10");
      dfROI.fieldSize = 4;
      designer.add(dfROI);

      cvl = new CodeValueList();
      cvl.add("0", "Does not apply");
      cvl.add("1", "low");
      cvl.add("2", "medium");
      cvl.add("3", "high");
      df = new DesignerField(CodeValue.class.getName(), "PROCESS_OPTIMIZATION", "<b><big>5. Processes optimization</big></b>", false, false, null);
      df.separator = "<br>";
      df.cvl = cvl;
      designer.add(df);

      dfROI = new DesignerField(Double.class.getName(), "PROCESS_OPTIMIZATION_WEIGHT", "Weight", false, false, "10");
      dfROI.fieldSize = 4;
      designer.add(dfROI);

      cvl = new CodeValueList();
      cvl.add("0", "Does not apply");
      cvl.add("1", "low");
      cvl.add("2", "medium");
      cvl.add("3", "high");
      df = new DesignerField(CodeValue.class.getName(), "INNOVATION", "<b><big>6. Innovation</big></b>", false, false, null);
      df.separator = "<br>";
      df.cvl = cvl;
      designer.add(df);

      dfROI = new DesignerField(Double.class.getName(), "INNOVATION_WEIGHT", "Weight", false, false, "10");
      dfROI.fieldSize = 4;
      designer.add(dfROI);

      cvl = new CodeValueList();
      cvl.add("0", "Does not apply");
      cvl.add("1", "low");
      cvl.add("2", "medium");
      cvl.add("3", "high");
      df = new DesignerField(CodeValue.class.getName(), "PERCEIVED_VALUE", "<b><big>7. Perceived value</big></b>", false, false, null);
      df.separator = "<br>";
      df.cvl = cvl;
      designer.add(df);

      dfROI = new DesignerField(Double.class.getName(), "PERCEIVED_VALUE_WEIGHT", "Weight", false, false, "10");
      dfROI.fieldSize = 4;
      designer.add(dfROI);

    } else {


      
      //days missing
      String daysMissing = pageState.getI18n("UNSPECIFIED");
      if (task.getSchedule() != null && task.getSchedule().getEndDate() != null) {
        if (task.getSchedule().getValidityEndTime() > new Date().getTime()) {
          long missing = task.getSchedule().getValidityEndTime() - new Date().getTime();
          daysMissing = DateUtilities.getMillisInDaysHoursMinutes(missing);
        } else
          daysMissing = "<b>" + pageState.getI18n("OVERDUE") + "</b>";
      }


%>
<h1>Project Value: <%=task.getDisplayName()%></h1>
<table border="0" class="table" cellpadding="3">
 <thead class="dataTableHead">
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
 </thead>
</table>
<br>
<table border="0" class="table">

  <tr class="alternate">
    <td><%designer.draw("ROI", pageContext);%></td>
    <td><%designer.draw("ROI_WEIGHT", pageContext);%></td>
  </tr>

  <tr class="alternate">
    <td><%designer.draw("COST_CUTS", pageContext);%></td>
    <td><%designer.draw("COST_CUTS_WEIGHT", pageContext);%></td>
  </tr>

  <tr class="alternate">
    <td><%designer.draw("SALES_GROWTH", pageContext);%></td>
    <td><%designer.draw("SALES_GROWTH_WEIGHT", pageContext);%></td>
  </tr>

  <tr class="alternate">
    <td><%designer.draw("MARKET_SHARE_GROWTH", pageContext);%></td>
    <td><%designer.draw("MARKET_SHARE_GROWTH_WEIGHT", pageContext);%></td>
  </tr>

  <tr class="alternate">
    <td><%designer.draw("PROCESS_OPTIMIZATION", pageContext);%></td>
    <td><%designer.draw("PROCESS_OPTIMIZATION_WEIGHT", pageContext);%></td>
  </tr>

  <tr class="alternate">
    <td><%designer.draw("INNOVATION", pageContext);%></td>
    <td><%designer.draw("INNOVATION_WEIGHT", pageContext);%></td>
  </tr>

  <tr class="alternate">
    <td><%designer.draw("PERCEIVED_VALUE", pageContext);%></td>
    <td><%designer.draw("PERCEIVED_VALUE_WEIGHT", pageContext);%></td>
  </tr>
  <%

    double projectValue = 0;

    //sum of weights
    double w = designer.getEntry("ROI_WEIGHT", pageState).doubleValueNoErrorNoCatchedExc();
    w = w + designer.getEntry("COST_CUTS_WEIGHT", pageState).doubleValueNoErrorNoCatchedExc();
    w = w + designer.getEntry("SALES_GROWTH_WEIGHT", pageState).doubleValueNoErrorNoCatchedExc();
    w = w + designer.getEntry("MARKET_SHARE_GROWTH_WEIGHT", pageState).doubleValueNoErrorNoCatchedExc();
    w = w + designer.getEntry("PROCESS_OPTIMIZATION_WEIGHT", pageState).doubleValueNoErrorNoCatchedExc();
    w = w + designer.getEntry("INNOVATION_WEIGHT", pageState).doubleValueNoErrorNoCatchedExc();
    w = w + designer.getEntry("PERCEIVED_VALUE_WEIGHT", pageState).doubleValueNoErrorNoCatchedExc();

    double pickedValue = designer.getEntry("ROI", pageState).intValueNoErrorCodeNoExc() * designer.getEntry("ROI_WEIGHT", pageState).doubleValueNoErrorNoCatchedExc();
    pickedValue = pickedValue + designer.getEntry("COST_CUTS", pageState).intValueNoErrorCodeNoExc() * designer.getEntry("COST_CUTS_WEIGHT", pageState).doubleValueNoErrorNoCatchedExc();
    pickedValue = pickedValue + designer.getEntry("SALES_GROWTH", pageState).intValueNoErrorCodeNoExc() * designer.getEntry("SALES_GROWTH_WEIGHT", pageState).doubleValueNoErrorNoCatchedExc();
    pickedValue = pickedValue + designer.getEntry("MARKET_SHARE_GROWTH", pageState).intValueNoErrorCodeNoExc() * designer.getEntry("MARKET_SHARE_GROWTH_WEIGHT", pageState).doubleValueNoErrorNoCatchedExc();
    pickedValue = pickedValue + designer.getEntry("PROCESS_OPTIMIZATION", pageState).intValueNoErrorCodeNoExc() * designer.getEntry("PROCESS_OPTIMIZATION_WEIGHT", pageState).doubleValueNoErrorNoCatchedExc();
    pickedValue = pickedValue + designer.getEntry("INNOVATION", pageState).intValueNoErrorCodeNoExc() * designer.getEntry("INNOVATION_WEIGHT", pageState).doubleValueNoErrorNoCatchedExc();
    pickedValue = pickedValue + designer.getEntry("PERCEIVED_VALUE", pageState).intValueNoErrorCodeNoExc() * designer.getEntry("PERCEIVED_VALUE_WEIGHT", pageState).doubleValueNoErrorNoCatchedExc();

    pickedValue = (pickedValue * 33.33) / w;


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