<%@ page
        import="com.twproject.task.Task, org.jblooming.persistence.PersistenceHome, org.jblooming.utilities.JSP,
        org.jblooming.waf.view.PageState" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  String taskId = pageState.getEntry("taskId").stringValueNullIfEmpty();
  if (taskId != null) {
    Task task = (Task) PersistenceHome.findByPrimaryKey(Task.class, taskId);

    %>T#<%=task.getMnemonicCode()%># <%=task.getDisplayName()%>  (<%=pageState.getI18n(task.getStatus())%>):
<%=JSP.w(task.getSchedule().getStartDate())%> - <%=JSP.w(task.getSchedule().getEndDate())%>
<%
  }
%>