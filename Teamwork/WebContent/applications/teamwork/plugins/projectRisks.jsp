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
org.jblooming.waf.constants.Commands, org.jblooming.waf.html.core.JspIncluder,
org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.html.display.PercentileDisplay, org.jblooming.waf.settings.I18n,
org.jblooming.waf.view.PageState, java.util.Date, com.twproject.security.TeamworkPermissions"%><%@ page contentType="text/html; charset=utf-8" pageEncoding="UTF-8"%><%!

  public class ProjectRisks extends PagePlugin {


    public boolean isVisibleInThisContext(PageState pagestate) {

      boolean ret = true;

      if (pagestate.getMainObject() != null && pagestate.getMainObject().getClass().equals(Task.class)) {
        Task task = (Task) pagestate.getMainObject();
        //ret = task.getType() != null && "PRODUCTION".equalsIgnoreCase(task.getType().getDescription())&& task.getParent() == null && task.getRelevance()>=80;
        ret = "Twproject test form".equalsIgnoreCase(task.getName());

      }

      return ret;
    }
  }
%><%

  if (JspIncluder.INITIALIZE.equals(request.getParameter(Commands.COMMAND))) {

    PluginBricks.getPagePluginInstance("TASK_FORMS", new ProjectRisks(), request);

  } else {

    PageState pageState = PageState.getCurrentPageState(request);

    Task task = (Task) PersistenceHome.findByPrimaryKey(Task.class, pageState.mainObjectId);
    Designer designer = (Designer) JspIncluderSupport.getCurrentInstance(request);

    designer.readOnly = !task.hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.task_canWrite);

    if (Designer.DRAW_FORM.equals(request.getAttribute(JspIncluder.ACTION))) {
      if (designer.fieldsConfig) {

        CodeValueList cvl = new CodeValueList();
        cvl.add("20", "Clear");
        cvl.add("40", "Partially clear");
        cvl.add("60", "Not clear");
        DesignerField df = new DesignerField(CodeValue.class.getName(), "CONTEXTUAL_1", "1. Clarity of project targets:", false, false, null);
        //df.separator = "&nbsp;";
        df.cvl = cvl;
        designer.add(df);

        cvl = new CodeValueList();
        cvl.add("50", "Yes");
        cvl.add("100", "No");
        df = new DesignerField(CodeValue.class.getName(), "CONTEXTUAL_2", "2. Uncomplete scope definition:", false, false, null);
        //df.separator = "&nbsp;";
        df.cvl = cvl;
        designer.add(df);

        cvl = new CodeValueList();
        cvl.add("20", "Yes");
        cvl.add("40", "No");
        df = new DesignerField(CodeValue.class.getName(), "CONTEXTUAL_3", "3. Additional requests:", false, false, null);
        //df.separator = "&nbsp;";
        df.cvl = cvl;
        designer.add(df);

        cvl = new CodeValueList();
        cvl.add("20", "High");
        cvl.add("40", "Low");
        df = new DesignerField(CodeValue.class.getName(), "CONTEXTUAL_4", "4. Company sponsorship:", false, false, null);
        //df.separator = "&nbsp;";
        df.cvl = cvl;
        designer.add(df);

        cvl = new CodeValueList();
        cvl.add("20", "Yes");
        cvl.add("40", "No");
        df = new DesignerField(CodeValue.class.getName(), "CONTEXTUAL_5", "5. External dependencies:", false, false, null);
        //df.separator = "&nbsp;";
        df.cvl = cvl;
        designer.add(df);

        DesignerField dfROI = new DesignerField(Double.class.getName(), "CONTEXTUAL_WEIGHT", "Weight", false, false, "30");
        dfROI.separator = "&nbsp;";
        dfROI.fieldSize = 4;
        designer.add(dfROI);


        cvl = new CodeValueList();
        cvl.add("20", "Yes");
        cvl.add("40", "No");
        df = new DesignerField(CodeValue.class.getName(), "MANAGERIAL_1", "1. Resource scarcity dependencies:", false, false, null);
        df.cvl = cvl;
        designer.add(df);

        cvl = new CodeValueList();
        cvl.add("20", "Yes");
        cvl.add("40", "No");
        df = new DesignerField(CodeValue.class.getName(), "MANAGERIAL_2", "2. Critical project end:", false, false, null);
        df.cvl = cvl;
        designer.add(df);

        cvl = new CodeValueList();
        cvl.add("20", "from 0,3 to 0,5");
        cvl.add("40", "from 0,5 to 0,8");
        cvl.add("60", "from 0,8 to 1");
        df = new DesignerField(CodeValue.class.getName(), "MANAGERIAL_3", "3. Relationship between effective time and calendar:", false, false, null);
        df.cvl = cvl;
        designer.add(df);

        cvl = new CodeValueList();
        cvl.add("20", "High");
        cvl.add("40", "Medium");
        cvl.add("60", "Low");
        cvl.add("60", "None");
        df = new DesignerField(CodeValue.class.getName(), "MANAGERIAL_4", "4. Project management' experience:", false, false, null);
        df.cvl = cvl;
        designer.add(df);

        cvl = new CodeValueList();
        cvl.add("20", "Yes");
        cvl.add("40", "No");
        df = new DesignerField(CodeValue.class.getName(), "MANAGERIAL_5", "5. Effective resource availability:", false, false, null);
        df.cvl = cvl;
        designer.add(df);

        cvl = new CodeValueList();
        cvl.add("20", "Yes");
        cvl.add("40", "No");
        df = new DesignerField(CodeValue.class.getName(), "MANAGERIAL_6", "6. Key-users availability:", false, false, null);
        df.cvl = cvl;
        designer.add(df);

        df = new DesignerField(Double.class.getName(), "MANAGERIAL_WEIGHT", "Weight", false, false, "40");
        df.separator = "&nbsp;";
        df.fieldSize = 4;
        designer.add(df);


        cvl = new CodeValueList();
        cvl.add("20", "Yes");
        cvl.add("40", "No");
        df = new DesignerField(CodeValue.class.getName(), "TECHNICAL_1", "1. New or unfamiliar technology:", false, false, null);
        df.cvl = cvl;
        designer.add(df);

        cvl = new CodeValueList();
        cvl.add("20", "High");
        cvl.add("40", "Medium");
        cvl.add("60", "Low");
        df = new DesignerField(CodeValue.class.getName(), "TECHNICAL_2", "2. Complex technology:", false, false, null);
        df.cvl = cvl;
        designer.add(df);

        cvl = new CodeValueList();
        cvl.add("20", "High");
        cvl.add("40", "Medium");
        cvl.add("60", "Low");
        df = new DesignerField(CodeValue.class.getName(), "TECHNICAL_3", "3. Integration level:", false, false, null);
        df.cvl = cvl;
        designer.add(df);

        cvl = new CodeValueList();
        cvl.add("20", "Yes");
        cvl.add("40", "No");
        df = new DesignerField(CodeValue.class.getName(), "TECHNICAL_4", "4. Technology availability:", false, false, null);
        df.cvl = cvl;
        designer.add(df);

        cvl = new CodeValueList();
        cvl.add("20", "0-5%");
        cvl.add("40", "5-15%");
        cvl.add("60", ">15%");
        df = new DesignerField(CodeValue.class.getName(), "TECHNICAL_5", "5. Changes on an existing technology:", false, false, null);
        df.cvl = cvl;
        designer.add(df);

        dfROI = new DesignerField(Double.class.getName(), "TECHNICAL_WEIGHT", "Weight", false, false, "30");
        dfROI.separator = "&nbsp;";
        dfROI.fieldSize = 4;
        designer.add(dfROI);

      } else {
        // layout


        //days missing
        String daysMissing = pageState.getI18n("UNSPECIFIED");
        if (task.getSchedule() != null && task.getSchedule().getEndDate() != null) {
          if (task.getSchedule().getValidityEndTime() > new Date().getTime()) {
            long missing = task.getSchedule().getValidityEndTime() - new Date().getTime();
            daysMissing = DateUtilities.getMillisInDaysHoursMinutes(missing);
          } else
            daysMissing = "<b>" + pageState.getI18n("OVERDUE") + "</b>";
        }


%><h1>Project Risks: <%=task.getDisplayName()%></h1>
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
<table border="0" class="table">
  <tr><td colspan="2"><b><big>Contextual factors</big></b></td></tr>
  <tr class="alternate">
    <td><%designer.draw("CONTEXTUAL_1",pageContext);%></td>
  </tr>
  <tr class="alternate">
    <td><%designer.draw("CONTEXTUAL_2",pageContext);%></td>
  </tr>
  <tr class="alternate">
    <td><%designer.draw("CONTEXTUAL_3",pageContext);%></td>
  </tr>
  <tr class="alternate">
    <td><%designer.draw("CONTEXTUAL_4",pageContext);%></td>
  </tr>
  <tr class="alternate">
    <td><%designer.draw("CONTEXTUAL_5",pageContext);%></td>
  </tr>
  <tr><td colspan="3" style="border-top:1px solid">&nbsp;</td></tr>
 <tr><td>&nbsp;</td>
    <td align="right"><%designer.draw("CONTEXTUAL_WEIGHT",pageContext);%></td>
  </tr>
  <tr><td colspan="3" style="border-top:1px solid">&nbsp;</td></tr>

  <tr><td colspan="2"><b><big>Management/organizational factors</big></b></td></tr>
  <tr class="alternate">
    <td><%designer.draw("MANAGERIAL_1",pageContext);%></td>
  </tr>
  <tr class="alternate">
    <td><%designer.draw("MANAGERIAL_2",pageContext);%></td>
  </tr>
  <tr  class="alternate">
    <td><%designer.draw("MANAGERIAL_3",pageContext);%></td>
  </tr>
  <tr class="alternate">
    <td><%designer.draw("MANAGERIAL_4",pageContext);%></td>
  </tr>
  <tr class="alternate">
    <td><%designer.draw("MANAGERIAL_5",pageContext);%></td>
  </tr>
  <tr class="alternate">
    <td><%designer.draw("MANAGERIAL_6",pageContext);%></td>
  </tr>
  <tr><td colspan="3" style="border-top:1px solid">&nbsp;</td></tr>
  <%

  %><tr><td>&nbsp;</td>
    <td align="right"><%designer.draw("MANAGERIAL_WEIGHT",pageContext);%></td>
  </tr>
  <tr><td colspan="3" style="border-top:1px solid">&nbsp;</td></tr>

  <tr><td colspan="2"><b><big>Technological factors</big></b></td></tr>
  <tr  class="alternate">
    <td><%designer.draw("TECHNICAL_1",pageContext);%></td>
  </tr>
  <tr class="alternate">
    <td><%designer.draw("TECHNICAL_2",pageContext);%></td>
  </tr>
  <tr class="alternate">
    <td><%designer.draw("TECHNICAL_3",pageContext);%></td>
  </tr>
  <tr class="alternate">
    <td><%designer.draw("TECHNICAL_4",pageContext);%></td>
  </tr>
  <tr  class="alternate">
    <td><%designer.draw("TECHNICAL_5",pageContext);%></td>
  </tr>
  <tr><td colspan="3" style="border-top:1px solid">&nbsp;</td></tr>

  <tr><td>&nbsp;</td>
    <td align="right"><%designer.draw("TECHNICAL_WEIGHT",pageContext);%></td>
  </tr>
  <tr><td colspan="3" style="border-top:1px solid">&nbsp;</td></tr>


<%
  double projectValue = 0;

  //sum of weights
  double w = designer.getEntry("CONTEXTUAL_WEIGHT",pageState).doubleValueNoErrorNoCatchedExc();
  w = w + designer.getEntry("MANAGERIAL_WEIGHT",pageState).doubleValueNoErrorNoCatchedExc();
  w = w + designer.getEntry("TECHNICAL_WEIGHT",pageState).doubleValueNoErrorNoCatchedExc();

  double pickedValue = designer.getEntry("CONTEXTUAL_1",pageState).intValueNoErrorCodeNoExc()*designer.getEntry("CONTEXTUAL_WEIGHT",pageState).doubleValueNoErrorNoCatchedExc();
  pickedValue = pickedValue + designer.getEntry("CONTEXTUAL_2",pageState).intValueNoErrorCodeNoExc()*designer.getEntry("CONTEXTUAL_WEIGHT",pageState).doubleValueNoErrorNoCatchedExc();
  pickedValue = pickedValue + designer.getEntry("CONTEXTUAL_3",pageState).intValueNoErrorCodeNoExc()*designer.getEntry("CONTEXTUAL_WEIGHT",pageState).doubleValueNoErrorNoCatchedExc();
  pickedValue = pickedValue + designer.getEntry("CONTEXTUAL_4",pageState).intValueNoErrorCodeNoExc()*designer.getEntry("CONTEXTUAL_WEIGHT",pageState).doubleValueNoErrorNoCatchedExc();
  pickedValue = pickedValue + designer.getEntry("CONTEXTUAL_5",pageState).intValueNoErrorCodeNoExc()*designer.getEntry("CONTEXTUAL_WEIGHT",pageState).doubleValueNoErrorNoCatchedExc();

  pickedValue = pickedValue + designer.getEntry("MANAGERIAL_1",pageState).intValueNoErrorCodeNoExc()*designer.getEntry("MANAGERIAL_WEIGHT",pageState).doubleValueNoErrorNoCatchedExc();
  pickedValue = pickedValue + designer.getEntry("MANAGERIAL_2",pageState).intValueNoErrorCodeNoExc()*designer.getEntry("MANAGERIAL_WEIGHT",pageState).doubleValueNoErrorNoCatchedExc();
  pickedValue = pickedValue + designer.getEntry("MANAGERIAL_3",pageState).intValueNoErrorCodeNoExc()*designer.getEntry("MANAGERIAL_WEIGHT",pageState).doubleValueNoErrorNoCatchedExc();
  pickedValue = pickedValue + designer.getEntry("MANAGERIAL_4",pageState).intValueNoErrorCodeNoExc()*designer.getEntry("MANAGERIAL_WEIGHT",pageState).doubleValueNoErrorNoCatchedExc();
  pickedValue = pickedValue + designer.getEntry("MANAGERIAL_5",pageState).intValueNoErrorCodeNoExc()*designer.getEntry("MANAGERIAL_WEIGHT",pageState).doubleValueNoErrorNoCatchedExc();
  pickedValue = pickedValue + designer.getEntry("MANAGERIAL_6",pageState).intValueNoErrorCodeNoExc()*designer.getEntry("MANAGERIAL_WEIGHT",pageState).doubleValueNoErrorNoCatchedExc();

  pickedValue = pickedValue + designer.getEntry("TECHNICAL_1",pageState).intValueNoErrorCodeNoExc()*designer.getEntry("TECHNICAL_WEIGHT",pageState).doubleValueNoErrorNoCatchedExc();
  pickedValue = pickedValue + designer.getEntry("TECHNICAL_2",pageState).intValueNoErrorCodeNoExc()*designer.getEntry("TECHNICAL_WEIGHT",pageState).doubleValueNoErrorNoCatchedExc();
  pickedValue = pickedValue + designer.getEntry("TECHNICAL_3",pageState).intValueNoErrorCodeNoExc()*designer.getEntry("TECHNICAL_WEIGHT",pageState).doubleValueNoErrorNoCatchedExc();
  pickedValue = pickedValue + designer.getEntry("TECHNICAL_4",pageState).intValueNoErrorCodeNoExc()*designer.getEntry("TECHNICAL_WEIGHT",pageState).doubleValueNoErrorNoCatchedExc();
  pickedValue = pickedValue + designer.getEntry("TECHNICAL_5",pageState).intValueNoErrorCodeNoExc()*designer.getEntry("TECHNICAL_WEIGHT",pageState).doubleValueNoErrorNoCatchedExc();

  pickedValue = (pickedValue)/w;

  %><tr>
    <td colspan="3" align="right"><b><big>Project risk:&nbsp;<%=JSP.perc(pickedValue)%></big></b></td>
  </tr>
   </table><%   


  }
  }
  }
%>