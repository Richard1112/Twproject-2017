<%@page import="com.teamwork.expand.TaskServiceBricks"%>
<%@page import="com.teamwork.expand.TaskReportBricks"%>
<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.resource.ResourceBricks, com.twproject.security.SecurityBricks, com.twproject.security.TeamworkPermissions, com.twproject.task.Issue, com.twproject.task.IssueBricks, com.twproject.task.TaskBricks, org.jblooming.designer.DesignerField, org.jblooming.security.Permission, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.input.*, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, java.util.HashSet, java.util.Set, com.twproject.resource.Resource, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.PluginBricks, org.jblooming.waf.html.container.DivOnMouseover, java.util.List, org.jblooming.utilities.JSP, org.jblooming.utilities.CollectionUtilities" %>
<%
   PageState pageState = PageState.getCurrentPageState(request);
   SmartCombo reportTypeCombo = TaskReportBricks.getReportTypeCombo("REPORT_TYPE_ID", pageState);
   reportTypeCombo.fieldSize = 18;
   reportTypeCombo.linkToEntity=null;
   reportTypeCombo.separator="";
   reportTypeCombo.required=false;
   reportTypeCombo.label="";
   SmartCombo supCombo = TaskServiceBricks.getDepartCombo("REPORT_DEPART_ID", pageState, "DEPARTMENT");
   supCombo.fieldSize = 18;
   supCombo.linkToEntity=null;
   supCombo.separator="";
   supCombo.required=false;
   supCombo.label="";
  // supCombo.toHtml(pageContext);
   String taskId = pageState.getEntry("TASK_ID").stringValueNullIfEmpty();
   
   SmartCombo yearCombo = TaskReportBricks.getReportYearCombo("REPORT_YEAR", pageState);
   yearCombo.fieldSize = 18;
   yearCombo.linkToEntity=null;
   yearCombo.separator="";
   yearCombo.required=false;
   yearCombo.label="";
   SmartCombo dicCombo = TaskReportBricks.getReportDictionNameCombo("REPORT_DIC_NAME", pageState);
   dicCombo.fieldSize = 18;
   dicCombo.linkToEntity=null;
   dicCombo.separator="";
   dicCombo.required=false;
   dicCombo.label="";
   
   DateField startDate = new DateField("UPLOAD_START_DATE");
   startDate.labelstr="";
   startDate.separator="";
   startDate.required=false;
   startDate.dateFormat="yyyy-MM-dd";
   
   DateField endDate = new DateField("UPLOAD_END_DATE");
   endDate.labelstr="";
   endDate.separator="";
   endDate.required=false;

%>
  <div class="filterActiveElements"></div>
  <div class="filterInactiveElements">
     <div class="filterElement filterDefault">
        <label><%=I18n.get("Department")%></label><%supCombo.toHtml(pageContext);%>
     </div>
     <div class="filterElement filterDefault">
      <label><%=I18n.get("REPORT_YEAR")%></label> &nbsp;&nbsp;&nbsp;&nbsp;<%yearCombo.toHtml(pageContext);%>
    </div> 
    <div class="filterElement">
      <label><%=I18n.get("REPORT_DIC_NAME")%></label> &nbsp;&nbsp;&nbsp;&nbsp;<%dicCombo.toHtml(pageContext);%>
    </div> 
    <div class="filterElement">
      <label><%=I18n.get("REPORT_TYPE")%></label> &nbsp;&nbsp;&nbsp;&nbsp;<%reportTypeCombo.toHtml(pageContext);%>
    </div>
    <div class="filterElement">
      <label><%=I18n.get("REPORT_UPLOAD_DATE")%></label> &nbsp;&nbsp;&nbsp;&nbsp;<%startDate.toHtml(pageContext);%>
    </div>
  
     <div class="filterElement hiddenFilterElement ">
        <input type="hidden" name="TASK_ID" value="<%=taskId%>">
     </div>
  </div>



