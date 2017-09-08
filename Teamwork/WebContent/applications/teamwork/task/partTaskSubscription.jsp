<%@ page import="com.twproject.operator.TeamworkOperator,
                 com.twproject.resource.Resource,
                 com.twproject.security.RoleTeamwork,
                 com.twproject.security.TeamworkPermissions,
                 com.twproject.task.Task,
                 org.jblooming.messaging.Listener,
                 org.jblooming.messaging.MessagingSystem,
                 org.jblooming.ontology.SerializedMap,
                 org.jblooming.utilities.StringUtilities,
                 org.jblooming.waf.constants.Fields,
                 org.jblooming.waf.html.container.Container,
                 org.jblooming.waf.html.input.CheckField,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageState,
                 java.util.ArrayList,
                 java.util.List" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  Task task=Task.load(pageState.getEntry("taskId").intValueNoErrorCodeNoExc()+"");
  Resource assignee = Resource.load(pageState.getEntry("resId").intValueNoErrorCodeNoExc()+"");
  RoleTeamwork role= RoleTeamwork.load(pageState.getEntry("roleId").intValueNoErrorCodeNoExc()+"");




  if (assignee== null || (assignee.getMyself() == null &&(assignee.getMyManager()==null || assignee.getMyManager().getMyself()==null))) {
    %><%=I18n.get("RESOURCE_NO_LOGIN_IS_UNSUBSCRIBABLE")%><%
    // PEAK OF ELEGANCE: LEAVING!!!!!!!!!!!!!!!!
    return;
  }


  boolean notifyingManager=false; // if you are assigning a department you can notify the manager for the assignment creation ONLY (not for events) so we hide event related rows

  // I can subscribe if I have permisson or if I am on my subscriptions
  //boolean canSubscribe=task.hasPermissionFor(logged, TeamworkPermissions.assignment_manage);
  boolean canSubscribe=task.hasPermissionFor(logged, TeamworkPermissions.assignment_canCRW);
  canSubscribe= canSubscribe || assignee.equals(logged.getPerson());


  TeamworkOperator focusedOperator=assignee.getMyself();
  if (focusedOperator==null && assignee.getMyManager()!=null )
    focusedOperator=assignee.getMyManager().getMyself();

  List<Listener> myListeners = task.bricks.getListeners(focusedOperator);
  boolean isSomeAncestorListeningAndPropagating = task.bricks.isSomeAncestorListeningAndPropagating(focusedOperator);

  boolean assIsNew=pageState.getEntry("assIsNew").intValueNoErrorCodeNoExc()>0;

  //make: uses default from role if there is no assig for the resource
  if (role!=null && (assIsNew || task.getFirstAssignmentsForResource(assignee)==null ) ){
    SerializedMap<String, String> subm = role.getDefaultSubscriptions();
    if (subm != null) {
      for (String k : subm.keySet()) {
        pageState.addClientEntry(k, subm.get(k));
      }
    }

  } else {
    for (Listener l : myListeners) {
      pageState.addClientEntry((l.getEventType()), Fields.TRUE);
      pageState.addClientEntry("TASK_NOTIFY_DESC", l.isListenDescendants() ? Fields.TRUE : Fields.FALSE);
      List<String> medias = StringUtilities.splitToList(l.getMedia(), ",");
      String type = l.getEventType();
      //if (type.startsWith("TASK_ISSUE_"))
      //  type = "TASK_ISSUE_ADDED_CLOSED";
      for (String media : medias) {
        pageState.addClientEntry(type + "_" + media, Fields.TRUE);
      }
    }
  }

  if(!notifyingManager){
    CheckField propDesc = new CheckField("TASK_NOTIFY_DESC", "&nbsp;", false);
    propDesc.label = "<small>"+I18n.get("TASK_NOTIFY_DESC")+"</small>";
    propDesc.disabled=!canSubscribe;
%><div style="float: right"><% propDesc.toHtml(pageContext);%></div><%
  }

  if (focusedOperator==null){
    focusedOperator=assignee.getMyManager().getMyself(); //notify the manager instead
    notifyingManager=true;
%><h4><%=I18n.get("SUBSCRIBE_%%_TO",  assignee.getDisplayName()+" ("+assignee.getMyManager().getDisplayName()+") ")+":"%></h4><%
} else {
%><h4><%=(assignee != null ? I18n.get("SUBSCRIBE_%%_TO", assignee.getDisplayName()) + ":" : I18n.get("SUBSCRIBE_ASSIGNEE_TO"))%></h4><%
  }

  //listenToContainer.start(pageContext);


%>
<table class="table dataTable">
<%
  if (!notifyingManager && isSomeAncestorListeningAndPropagating) {
    %><tr><td colspan="99" align="left"><%=I18n.get("SOME_ANCESTOR_LISTENING_AND_PROPAG")%></td></tr><%
  }
%>

<tr>
  <th class="tableHead"><%=I18n.get("EVENT_TYPE")%></th>
  <%

    for (MessagingSystem.Media media : MessagingSystem.activeMedia) {

      %> <th style="text-align: right" class="tableHead"><%=I18n.get(media.toString().toUpperCase())%> <%
      List<String> cks=new ArrayList<String>();
      for (Task.Event event: Task.Event.values() ){
        cks.add(event+"_" + media.toString().toUpperCase());
      }
      cks.add("ASSIGNEE_NOTIFY_"+ media.toString().toUpperCase()+"");
      CheckField cf = CheckField.getMasterCheckField("SELECT_ALL", cks);
      cf.disabled=!canSubscribe;
      cf.preserveOldValue=false;
      cf.label = "";
      cf.toHtml(pageContext);
      %></th> <%
    }
    %></tr>
  
  <tr class="alternate" >
  <td><%=I18n.get("ASSIGNMENT_CREATION")%></td>
  <%

    if (assIsNew){
      pageState.addClientEntry("ASSIGNEE_NOTIFY_"+MessagingSystem.Media.EMAIL,Fields.TRUE);
      pageState.addClientEntry("ASSIGNEE_NOTIFY_"+MessagingSystem.Media.STICKY,Fields.TRUE);
    }


    for (MessagingSystem.Media media : MessagingSystem.activeMedia) {

      %><td align="right"><%

      String subscrField = "ASSIGNEE_NOTIFY_" + media.toString().toUpperCase();
      CheckField mediaCf = new CheckField(subscrField, "", false);
      mediaCf.disabled = !canSubscribe;
      mediaCf.label = "";//I18n.get(media.toString().toUpperCase());
      mediaCf.toHtml(pageContext);

      %></td><%
    }
  %></tr><%


  if (!notifyingManager){
        for (Task.Event event: Task.Event.values() ){

      // hack to check permission on costs for TASK_BUDGET_OVERFLOW
      if (Task.Event.TASK_BUDGET_OVERFLOW.equals(event)){
        if (!task.hasPermissionFor(focusedOperator, TeamworkPermissions.task_cost_canRead))
          continue;
      }

      boolean subscribedOnParent=task.bricks.isSomeAncestorListeningAndPropagatingForTheEvent(focusedOperator,event);

      %>
        <tr class="alternate <%=subscribedOnParent?"assDisabled":""%>" title="<%=subscribedOnParent?I18n.get("SOME_ANCESTOR_LISTENING_AND_PROPAG"):""%>">
        <td><%=I18n.get(event.toString())%></td>
        <%


          for (MessagingSystem.Media media : MessagingSystem.activeMedia) {
            %><td align="right"><%
            String subscrField = event+"_" + media.toString().toUpperCase();

            CheckField cf = new CheckField(subscrField, "", true);
            cf.label = "";
            cf.disabled=!canSubscribe || subscribedOnParent ;
            cf.toHtml(pageContext);
            %></td><%
          }
      %></tr><%
    }
  }
  %></table><%
%>