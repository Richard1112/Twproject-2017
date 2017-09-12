<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.resource.Person, com.twproject.resource.Resource, com.twproject.utilities.TeamworkComparators, org.jblooming.anagraphicalData.AnagraphicalData, org.jblooming.utilities.JSP, org.jblooming.utilities.StringUtilities, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.ArrayList, java.util.Collections, java.util.List" %><%

  JspHelper rowDrawer = (JspHelper) JspIncluderSupport.getCurrentInstance(request);
  Resource resource = (Resource) rowDrawer.parameters.get("ROW_OBJ");

  PageState pageState = PageState.getCurrentPageState(request);

  TeamworkOperator logged= (TeamworkOperator) pageState.getLoggedOperator();


  PageSeed e = new PageSeed("resourceEditor.jsp");
  e.setCommand(Commands.EDIT);


  ButtonLink bEdit = ButtonLink.getEditInstance("resourceEditor.jsp", resource, request);
  bEdit.additionalCssClass="edit";
  e.setMainObjectId(resource.getId());


  String resName;
  if (resource instanceof Person)
    resName= JSP.w(((Person) resource).getPersonName()+ " " + JSP.w(((Person) resource).getPersonSurname()));
  else
    resName=resource.getName();


  int rowspan=JSP.ex(resource.getAnagraphicalDatas())?resource.getAnagraphicalDatas().size():1;

%><tr class="alternate first resourceList <%=resource.getAnagraphicalDatas().size()<=1 ? "last" : ""%>" >

  <td  width="1%" valign="top" nowrap rowspan="<%=rowspan%>">
    <%  if (resource.getMyself() != null) {  %>
    <%=resource.bricks.getAvatarImage("").toHtmlStringBuffer()%>
    <%}else{%>
     <div style="height: 35px; width: 35px"></div>
    <%}%>
  </td>
  <td  valign="top" rowspan="<%=rowspan%>"><a href="<%=e.toLinkToHref()%>" class="button textual bolder"  style="white-space:normal;"><%=resName%></a>
  <%
    if (JSP.ex(resource.getTags())) {
      PageSeed search = pageState.thisPage(request);
      search.command = Commands.FIND;
      %><br><%
      List<String> tags = StringUtilities.splitToList(resource.getTags(), ",");
      for (String tag : tags) {
        search.addClientEntry("RESOURCE_TAGS", tag);
        ButtonLink bl = new ButtonLink(tag, search);
        bl.additionalCssClass = "tag";
        bl.toHtmlInTextOnlyModality(pageContext);
      }
    }

  %>

  </td>
  <td  valign="top" rowspan="<%=rowspan%>"><%=JSP.ex(resource.getCode())?"&nbsp;("+resource.getCode()+")":""%></td>
  <td  valign="top" rowspan="<%=rowspan%>"><%if (resource instanceof Person && ((Person) resource).getMyself() != null) resource.bricks.getPopupMessage(pageState).toHtml(pageContext);%></td>
  <td  valign="top" rowspan="<%=rowspan%>"><%  if (resource.getMyself() != null) {  %>
    <span class="score" title="<%=I18n.get("SCORE")%>"><span class="teamworkIcon">1</span> <%=resource.getMyself().getScore()%></span>
    <%}%></td>

  <%
    ArrayList<AnagraphicalData> orderAnagraphicalData = new ArrayList<AnagraphicalData>(resource.getAnagraphicalDatas());
    if (orderAnagraphicalData!=null && orderAnagraphicalData.size()>0){
      Collections.sort(orderAnagraphicalData, new TeamworkComparators.AnagraphicalDataComparator());

      int counter = 1;

      for (AnagraphicalData data : orderAnagraphicalData) {
  %>
  <td ><%=JSP.w(data.getLocationDescription())%></td>
  <td ><%=JSP.w(data.getTelephone())%></td>
  <td ><%=JSP.w(data.getMobile())%></td>
  <td ><a class="button textual" href="mailto:<%=JSP.w(data.getEmail())%>"><%=JSP.w(data.getEmail())%></a></td>
</tr>
<%
  if (resource.getAnagraphicalDatas().size()>1 && counter < resource.getAnagraphicalDatas().size()) {

%><tr class="alternate resourceList <%= counter+1 == resource.getAnagraphicalDatas().size() ? "last" : ""%>"><%
      counter++;
    }
  }
} else {
%><td colspan="4">&nbsp;</td></tr><%
  }

%>
