<%@ page import="com.twproject.resource.Person, com.twproject.resource.Resource, com.twproject.task.financial.CostAggregator, com.twproject.utilities.TeamworkComparators, org.jblooming.anagraphicalData.AnagraphicalData, org.jblooming.utilities.JSP, org.jblooming.waf.html.core.JspHelper, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, java.util.ArrayList, java.util.Collections" %>
<%
 PageState pageState = PageState.getCurrentPageState(request);
 JspHelper dDrawer = (JspHelper) JspIncluderSupport.getCurrentInstance(request);
 Resource resource = (Resource)dDrawer.parameters.get("resource");

 boolean isPerson = (resource instanceof Person);

%><table border="0" width="99%" align="center" cellpadding="3" cellspacing="0">
        <tr>
           <td colspan="3"><h1><%=resource.getDisplayName()%></h1></td>
        </tr>

        <tr>
          <td valign="top"><%=I18n.get("CODE")%>: <b><%=JSP.w(resource.getCode() != null ? resource.getCode() : "-")%></b></td>
          <td valign="top"><%=I18n.get("PARENT")%>: <b><%=JSP.w(resource.getParent() != null ? resource.getParent().getDisplayName() : "-")%></b></td>
          <td valign="top"><%=I18n.get("AREA")%>: <b><%=JSP.w(resource.getArea() != null ? resource.getArea().getName() : "-")%></b></td>
        </tr>
        <tr>
          <td colspan="3">
             <table class="table" border="0" cellpadding="2"><%
                      ArrayList<AnagraphicalData> orderAnagraphicalData = new ArrayList<AnagraphicalData>(((Resource)resource).getAnagraphicalDatas());
                      Collections.sort(orderAnagraphicalData,new TeamworkComparators.AnagraphicalDataComparator());

                    if(orderAnagraphicalData != null && orderAnagraphicalData.size()>0) {
                        for (AnagraphicalData data : orderAnagraphicalData) {
                  %>
                  <tr>
                  <th colspan="5" nowrap style="height: 30px" class="tableHead">
                    <%=JSP.w(data.getLocationDescription())%>
                  </th>
                  </tr>
                 <tr>
                   <td width="70%">

                     <table width="100%"  cellpadding="2" cellspacing="0" border="0" class="edged">
                  <tr height="25">
                    <td colspan="2"><%=I18n.get("ADDRESS")%>: <b><%=JSP.w(data.getAddress())%></b></td>
                    <td colspan="2"><%=I18n.get("CITY")%>: <b><%=(data.getCity()!=null ? data.getCity() : "")%></b></td>
                   </tr><tr height="25">
                    <td><%=I18n.get("PROVINCE")%>: <b><%=(data.getProvince()!=null ? data.getProvince() : "")%></b></td>
                    <td><%=I18n.get("ZIP")%>: <b><%=(data.getZip()!=null ? data.getZip() : "")%></b></td>
                    <td  colspan="2"><%=I18n.get("COUNTRY")%>: <b><%=(data.getCountry()!=null ? data.getCountry() : "")%></b></td>
                  </tr>
                  <tr height="25">
                  <td valign="middle">
                  t. <%=(data.getTelephone()!=null ? data.getTelephone() : "")%>
                  </td>
                  <td valign="middle">
                  f. <%=(data.getFax()!=null ? data.getFax() : "")%>
                  </td>
                  <td valign="middle">
                  m. <%=(data.getMobile()!=null ? data.getMobile() : "")%>
                  </td>
                    <td valign="middle">
                    <%=(data.getEmail()!=null ? data.getEmail() : "")%>
                    </td>
                  </tr></table>
               <td style="background-color: #EEEEEE;padding: 10px;font-weight: bold; vertical-align: top">
               <%=resource.getDisplayName()%><br>
                   <%=(data.getAddress()!=null ? data.getAddress() +"<br>" : "")%>
                   <%=(data.getZip()!=null ? data.getZip() +"&nbsp;" : "")%>
                   <%=(data.getCity()!=null ? data.getCity() +"<br>" : "")%>
                   <%=(data.getProvince()!=null ? data.getProvince() +" - " : "")%>
                   <%=(data.getCountry()!=null ? data.getCountry() : "")%>
               </td></tr>
               <tr><td colspan="4">&nbsp;</td></tr>
               <%
                   }
                  }
               %></table>
            </td></tr>
          <tr><td colspan="3">
          <table class="table" cellpadding="2" cellspacing="0" border="0"><%

            // employee data
            Person manager = resource.getMyManager();
            CostAggregator cc = resource.getMyCostAggregator();
            %><tr>
              <td><%=I18n.get("BOSS")%>: <b><%=manager!=null ? JSP.w(manager.getDisplayName()) : ""%></b></td>
              <td><%=I18n.get("COST_CENTER")%>: <b><%=cc!=null ? JSP.w(cc.getName()) : ""%></b></td>
              <td><%=I18n.get("JOB_DESCRIPTION")%>: <b><%=JSP.w(resource.getJobDescription())%></b></td>
            </tr>
            <tr>
              <td colspan="<%=isPerson? "1" : "3"%>"><%=I18n.get("NOTE")%>: <b><%=JSP.w(resource.getNotes())%></b></td>
              <%
              if (isPerson) {
                %><td><%=I18n.get("HIRING_DATE")%>: <b><%=JSP.w(((Person)resource).getHiringDate())%></b></td><%
                %><td><%=I18n.get("HOURLY_COST")%>: <b><%=JSP.w(((Person)resource).getHourlyCost())%></b></td><%
              }

               %>
            </tr>
            </table>
          </td>
        </tr>
     </table>