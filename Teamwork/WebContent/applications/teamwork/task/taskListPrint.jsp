<%@ page contentType="text/html;charset=UTF-8" language="java" %><%@ page import="com.twproject.task.Assignment,com.twproject.task.Task,com.twproject.task.businessLogic.TaskController,com.twproject.waf.TeamworkPopUpScreen,org.jblooming.system.SystemConstants,
org.jblooming.utilities.DateUtilities, org.jblooming.utilities.HashTable, org.jblooming.utilities.JSP,
 org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.ButtonJS,
  org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.html.display.Img, org.jblooming.waf.html.input.CheckField,
   org.jblooming.waf.html.state.Form, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.ArrayList, java.util.Date, java.util.List, java.util.Map, com.twproject.task.TaskBricks, org.jblooming.designer.DesignerField, org.jblooming.utilities.ReflectionUtilities"%>
<%
  Map<String,Number> twd = new HashTable<String,Number>();

  PageState pageState = PageState.getCurrentPageState(request);


  if (!pageState.screenRunning) {
    pageState.screenRunning = true;

    final ScreenArea body = new ScreenArea(new TaskController(), request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);
  } else {

    PageSeed self = pageState.thisPage(request);
    self.setCommand(Commands.FIND);
    self.addClientEntries(pageState.getClientEntries());    
    self.removeEntriesMatching("PRINT_");

    Form f = new Form(self);
    pageState.setForm(f);
    f.start(pageContext);


    // I need to preserve the damn filter
    boolean printWk = pageState.getEntryOrDefault("PRINT_WITH_TOTAL_WKL", Fields.FALSE).checkFieldValue();
    boolean printEST = pageState.getEntryOrDefault("PRINT_WITH_WKL_ESTIM", Fields.FALSE).checkFieldValue();
    boolean printCost = pageState.getEntryOrDefault("PRINT_WITH_WKL_COST", Fields.FALSE).checkFieldValue();

    List<Task> tasks = new ArrayList();
    List<Object[]> tasksSched =  pageState.getPage().getAllElements();


    for (Object[] taskSch : tasksSched) {
      Task ntask = ((Task) taskSch[0]);
      tasks.add(ntask);
      if (printWk)
        twd.put(ntask.getId()+"WD", new Long(ntask.getTotalWorklogDone()));
      if(printEST)
        twd.put(ntask.getId()+"WE", new Long(ntask.getTotalWorklogEstimated()));
      if(printCost)
        twd.put(ntask.getId()+"WC", new Double(ntask.getTotalCostsDone()));
    }

    Img logo = new Img(ApplicationState.getApplicationSetting(SystemConstants.PRINT_LOGO), "");
    %><table border="0" width="100%" align="center" cellpadding="5" cellspacing="0" >
    <tr>
      <td align="left" width="90%"><%logo.toHtml(pageContext);%></td>
      <td align="right" class="noprint"><%

          ButtonJS print = new ButtonJS("window.print();");
          print.label = "";
          print.toolTip = I18n.get("PRINT_PAGE");
          print.iconChar = "p";
          print.toHtmlInTextOnlyModality(pageContext);

        %></td>
    </tr>
</table>

<div id="printFilter" style="visibility:visible;" class="noprint">
  <%
  //filters

  CheckField wkt = new CheckField("PRINT_WITH_TOTAL_WKL", "&nbsp;", false);
  wkt.preserveOldValue = false;
  ButtonSubmit scwkt = new ButtonSubmit(f);
  wkt.toHtmlI18n(pageContext);

  CheckField wkEstim = new CheckField("PRINT_WITH_WKL_ESTIM", "&nbsp;", false);
  wkEstim.label = I18n.get("PRINT") + " " + I18n.get("ESTIMATE_WORKLOG");
  wkEstim.preserveOldValue = false;
  wkEstim.toHtmlI18n(pageContext);

  CheckField wkCost = new CheckField("PRINT_WITH_WKL_COST", "&nbsp;", false);
  wkCost.label = I18n.get("PRINT") + " " + I18n.get("COST_ESTIMATED");
  wkCost.preserveOldValue = false;
  wkCost.toHtmlI18n(pageContext);

  %>&nbsp;&nbsp;&nbsp;<%
  ButtonSubmit sc = new ButtonSubmit(f);
  sc.additionalCssClass="small";
  sc.label = I18n.get("REFRESH");
  sc.toHtml(pageContext);


  %></div>
        <table border="0"  align="center" cellpadding="5" cellspacing="0" class="table dataTable">
         <%
  boolean isFirst = true;
           long twe = 0;
           double tc = 0;
            long twdd = 0;
      for (Task task : tasks) {

         if (isFirst ) {
        %>
          <thead>
           <tr>
              <th class="tableHead"><%=I18n.get("NAME")%></th>
              <th nowrap class="tableHead"><%=I18n.get("CODE")%></th>
              <th class="tableHead"><%=I18n.get("TYPE")%></th>
              <th class="tableHead"><%=I18n.get("RELEVANCE")%></th>
              <th class="tableHead"><%=I18n.get("PROGRESS")%></th>
              <th class="tableHead"><%=I18n.get("START")%></th>
              <th class="tableHead"><%=I18n.get("END")%></th>
              <th class="tableHead"><%=I18n.get("ASSIGNEE")%></th>
            <% if (printWk) { %>
              <th class="tableHead"><%=I18n.get("TOTAL_WORKLOG_DONE_HM")%></th>
            <%}
              if (printEST) { %>
              <th class="tableHead"><%=I18n.get("ESTIMATE_WORKLOG")%></th>
            <%}
              if (printCost) { %>
              <th class="tableHead"><%=I18n.get("COST_ESTIMATED")%></th>
            <%}
            %>
          </tr></thead><%
        }

        isFirst = false;

        %> <tr class="alternate">
            <td valign="top"><%=JSP.w(task.getName())%></td>
            <td valign="top"><%=JSP.w(task.getCode())%>&nbsp;</td>
            <td valign="top"><%=JSP.w(task.getType() != null ? task.getType().getDescription() : "-")%></td>

            <td valign="top"><%=JSP.w(task.getRelevance())%></td>
            <td valign="top"><%=JSP.w(task.getProgress())%>%</td>
            <td valign="top"><%=task.getSchedule() != null ? JSP.w(task.getSchedule().getStartDate()) : "-"%></td>
            <td valign="top"><%=task.getSchedule() != null ? JSP.w(task.getSchedule().getEndDate()) : "-"%></td>
            <td valign="top" nowrap><%
              for (Assignment a: task.getAssignments()) {
                %><%=a.getResource().getDisplayName()%><br><%
              }
              %>&nbsp;</td>

          <% if (printWk) { %>
            <td valign="top" style="text-align:right;"><b><%=DateUtilities.getMillisInHoursMinutes(twd.get(task.getId() + "WD"))%></b></td>
          <%
            twdd += (Long)twd.get(task.getId()+"WD");
            }  if (printEST) { %>
            <td valign="top" style="text-align:right;"><b><%=DateUtilities.getMillisInHoursMinutes(twd.get(task.getId()+"WE"))%>&nbsp;</b></td>
          <%
            twe += (Long)twd.get(task.getId()+"WE");
            } if (printCost) { %>
            <td valign="top" style="text-align:right;"><b><%=JSP.currency(twd.get(task.getId()+"WC"))%>&nbsp;</b></td>
          <%
            tc +=(Double)twd.get(task.getId()+"WC");
            }

          %>
          </tr><%

          //---------------------------------------------------------- CUSTOM FIELDS ----------------------------------------------------------
          if (TaskBricks.hasCustomField()) {
            %><tr><td colspan="99" valign="middle"><%
            for (int i=1; i<7; i++) {
              if (!JSP.ex((String) ReflectionUtilities.getFieldValue("customField" + i, task)))
                continue;
              DesignerField dfStr = DesignerField.getCustomFieldInstance("TASK_CUSTOM_FIELD_", i, task, true, true, false, pageState);
              if (dfStr!=null){
                dfStr.separator=": ";
                %><div style="float:left;margin-right: 10px"><%dfStr.toHtml(pageContext);%></div><%
              }
            }
            %></td></tr><%
          }


          } %>
          <tr><td>&nbsp;</td></tr>
          <tr class="alternate"><td colspan="8">&nbsp;</td>
            <%if(printWk){%><td align="right"><%=I18n.get("TOTAL")%><br><b><%=DateUtilities.getMillisInHoursMinutes(twdd)%></b></td><%}%>
            <%if(printEST){%><td align="right"><%=I18n.get("TOTAL")%><br><b><%=DateUtilities.getMillisInHoursMinutes(twe)%></b></td><%}%>
            <%if(printCost){%><td align="right"><%=I18n.get("TOTAL")%><br><b><%=JSP.currency(tc)%></b></td><%}%></tr>
            
   </table>

<br><p align="right"><i><%=I18n.get("PRINTED_ON", JSP.timeStamp(new Date()))%></i></p>

<%
  f.end(pageContext);
  }
%>