<%@ page import=" com.twproject.operator.TeamworkOperator,
                  com.twproject.resource.Person,
                  com.twproject.resource.Resource,
                  com.twproject.resource.ResourceBricks,
                  com.twproject.security.TeamworkPermissions,
                  com.twproject.task.financial.FinancialBricks,
                  org.jblooming.designer.DesignerField,
                  org.jblooming.ontology.PerformantNode,
                  org.jblooming.security.Area,
                  org.jblooming.utilities.JSP,
                  org.jblooming.utilities.StringUtilities,
                  org.jblooming.waf.SessionState,
                  org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.input.*, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.ArrayList, java.util.Collections, java.util.List, java.util.Set, com.twproject.security.SecurityBricks, org.jblooming.waf.constants.Fields"%>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  Resource resource = (Resource) pageState.getMainObject();                                                       
  boolean isPerson = (resource instanceof Person);
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  boolean canWrite =resource.hasPermissionFor(logged,TeamworkPermissions.resource_canWrite);
%>
<table border="0" cellspacing="0" cellpadding="3" class="table">
  <tr>

    <td nowrap style="width: 20%">
      <%
        SmartCombo sb = ResourceBricks.getCompanyCombo(Fields.PARENT_ID,TeamworkPermissions.resource_canRead,null,pageState);

        PageSeed edit = pageState.pageFromRoot("resource/resourceEditor.jsp");
        edit.command= Commands.EDIT;
        sb.label = I18n.get("RESOURCE_OF");
        sb.fieldSize =25;
        sb.separator="<br>";
        sb.fieldClass = "formElements bold";
        sb.readOnly = !canWrite;
        sb.toHtml(pageContext);
      %>
    </td>
    <td style="width: 20%"><%

      sb = null;
      if (!resource.isNew() && isPerson) {
        sb = ResourceBricks.getPersonCombo("BOSS", TeamworkPermissions.resource_canRead, false, "resource!=:purelyMyself", pageState);
        sb.fixedParams.put("purelyMyself", resource);
      } else {
        sb = ResourceBricks.getPersonCombo("BOSS", TeamworkPermissions.resource_canRead, false, "", pageState);
      }
      sb.label = I18n.get("BOSS");
      sb.fieldSize = 25;
      //sb.useTableForResults = false;
      sb.separator = "<br>";
      sb.readOnly = !canWrite;

      //PageSeed edit = pageState.pageFromRoot("resource/resourceEditor.jsp");
      //edit.command= Commands.EDIT;
      //sb.addLinkToEntity(edit,I18n.get("HIS_RESOURCEEDITOR"));
      sb.toHtml(pageContext);

    %></td>


    <%

      if(isPerson) {
    %><td valign="top" nowrap width="20%"><%
    DateField df = new DateField("HIRING_DATE");
    df.labelstr = I18n.get("HIRING_DATE");
    df.readOnly = !canWrite;
    df.separator="<br>";
    df.toHtml(pageContext);
  %></td><%
    } %>

    <td class="<%=SecurityBricks.isSingleArea()?"displayNone":""%>">
      <%
        Set<Area> areas=  SecurityBricks.getAreasForLogged(resource.isNew() ? TeamworkPermissions.resource_canCreate : TeamworkPermissions.resource_canWrite,pageState);
        if (canWrite && !SecurityBricks.isSingleArea() && ((resource.getArea()!=null && areas.contains(resource.getArea()) || resource.bricks.logged.hasPermissionAsAdmin())) ) {
          Combo cb = SecurityBricks.getAreaCombo("AREA", resource.isNew() ? TeamworkPermissions.resource_canCreate : TeamworkPermissions.resource_canWrite, pageState);
          cb.readOnly = !canWrite;
          cb.separator="<br>";
          cb.toHtmlI18n(pageContext);
        } else {
          TextField.hiddenInstanceToHtml("AREA",pageContext);
      %><label><%=I18n.get("AREA")%></label><br><%=resource.getArea()!=null ? JSP.w(resource.getArea().getName()) : "-"%><%
      }


    %>


    </td>

  </tr>

  <%
    if (JSP.ex(resource.getMyManagerIds())){
      List<String> managerIds = StringUtilities.splitToList(resource.getMyManagerIds(), PerformantNode.SEPARATOR);
      if (managerIds.size()>1){
  %><tr><td colspan="4"><div class="box"><label><%=I18n.get("PATH_TO_MANAGER")%>:</label>&nbsp;<%
  for (int i=0;i<managerIds.size()-1;i++){ //last element is always empty
    Person man= Person.load(managerIds.get(i));
    if (man!=null){
      ButtonLink bl = ButtonLink.getEditInstance("resourceEditor.jsp",man,request);
      bl.label="/"+man.getDisplayName()+" ";
      bl.toHtmlInTextOnlyModality(pageContext);
    }
  }
%></div></td></tr><%
    }
  }
  %>
<tr>



<tr><%
  if (resource.hasPermissionFor(logged,TeamworkPermissions.resource_cost_canRead)) {
    %><td nowrap><%
    TextField hc = TextField.getCurrencyInstance("HOURLY_COST");
    hc.separator="<br>";
    hc.fieldSize=7;
    hc.readOnly = !canWrite;
    hc.toHtmlI18n(pageContext);
    %></td><%
  }



%><td nowrap><%
  ComboBox wc = ComboBox.getTimeInstance("WORK_DAILY_CAPACITY","WORKING_HOUR_TOTAL", pageState);
  wc.fieldSize = 5;
  wc.separator = "<br>";
  wc.readOnly = !canWrite;
  wc.toHtmlI18n(pageContext);

%></td>
  <td valign="top" colspan="2"> <%
    //todo che strano controllo di sicurezza è questo?
  //Set<Area> areas = logged.getAreasForPermission(TeamworkPermissions.resource_canRead);
  if (areas.size()>0) {
    SmartCombo ccs = FinancialBricks.getCostAggregatorCombo("COST_CENTER",areas,null,null, pageState);
    ccs.readOnly = !canWrite;
    ccs.fieldSize = 25;
    ccs.separator="<br>";
    ccs.toHtmlI18n(pageContext);
  } else {
    TextField.hiddenInstanceToHtml("COST_CENTER",pageContext);
  }

%></td></tr>
<tr><td valign="top" nowrap colspan="2"><%
TextArea ta = new TextArea(I18n.get("JOB_DESCRIPTION"),"JOB_DESCRIPTION","<br>",20,3,"");
ta.script = "style='width:100%;height:80px'";
ta.maxlength=2000;
ta.readOnly = !canWrite;
ta.toHtml(pageContext);
%>
</td><td valign="top" width="40%" colspan="2">
<label><%=I18n.get("NOTES")%></label>

  <div class=" formElements" style="-moz-border-radius:3px 3px 3px 3px; width:100%; height:80px; overflow-y:auto;" onclick="$(this).hide().next().show().focus()"><%=JSP.encode(resource.getNotes())%></div><%

ta = new TextArea("","NOTE","",20,3,"");
ta.script = "style='width:100%;height:80px;display:none;'";
ta.readOnly = !canWrite;
ta.maxlength=2000;
ta.toHtml(pageContext);
%>
</td>
  </tr>
  <tr>
  <td colspan="4">
    <%
      TagBox tags= new TagBox("RESOURCE_TAGS",Resource.class,resource.getArea());
      tags.label= I18n.get("TAGS");
      tags.separator="<br>";
      tags.fieldSize=50;
      tags.script = "style='width:100%;'";
      tags.toHtml(pageContext);

    %>
  </td></tr><%
     //try to know if there is custom forms or custom field
     if (ResourceBricks.hasCustomField() ){



       //---------------------------------------------------- CUSTOM FIELDS ------------------------------------------------------------------------
   %><tr><td colspan="3"><%

     for (int i=1; i<7; i++) {
       DesignerField dfStr = DesignerField.getCustomFieldInstance( "RESOURCE_CUSTOM_FIELD_",i, resource,!canWrite, false, false, pageState);
       if (dfStr!=null){
         dfStr.separator="</td><td>";
          %><table style="float:left;"><tr><td><%dfStr.toHtml(pageContext);%></td></tr></table><%
       }
     }
   %></td></tr><%


     }

   %>
</table>
