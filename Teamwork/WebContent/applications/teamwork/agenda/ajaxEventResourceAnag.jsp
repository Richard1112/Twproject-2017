<%@ page
        import="com.twproject.resource.Resource, org.jblooming.anagraphicalData.AnagraphicalData, org.jblooming.persistence.PersistenceHome,
        org.jblooming.utilities.JSP, org.jblooming.waf.constants.AnagraphicalDataConstants, org.jblooming.waf.view.PageState" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  String resId = pageState.getEntry("resId").stringValueNullIfEmpty();
  if (resId != null) {
    Resource resource = (Resource) PersistenceHome.findByPrimaryKey(Resource.class, resId);

    %><%=resource.getDisplayName()%> R#<%=resource.getMnemonicCode()%>#
<%    boolean somePrint = false;
    if (resource.getAnagraphicalDatas() != null && resource.getAnagraphicalDatas().size()>0) {
      AnagraphicalData ad = resource.getAnagraphicalDatas().iterator().next();

      if (!"".equals(JSP.w(ad.getAddress()).trim())) {
        %><%=pageState.getI18n(AnagraphicalDataConstants.FLD_ADDRESS)%>: <%=JSP.w(ad.getAddress())%>
<%
    somePrint = true;
  }
  if (!"".equals(JSP.w(ad.getCity()).trim())) {
    %><%=pageState.getI18n(AnagraphicalDataConstants.FLD_CITY)%>: <%=JSP.w(ad.getCity())%>
<%
    somePrint = true;
  }
  if (!"".equals(JSP.w(ad.getState()).trim())) {
%><%=pageState.getI18n(AnagraphicalDataConstants.FLD_STATE)%>: <%=JSP.w(ad.getState())%>
<%
    somePrint = true;
  }
  if (!"".equals(JSP.w(ad.getZip()).trim())) {
%><%=pageState.getI18n(AnagraphicalDataConstants.FLD_ZIP)%>: <%=JSP.w(ad.getZip())%>
<%
    somePrint = true;
  }
  if (!"".equals(JSP.w(ad.getCountry()).trim())) {
%><%=pageState.getI18n(AnagraphicalDataConstants.FLD_COUNTRY)%>: <%=JSP.w(ad.getCountry())%>
<%
    somePrint = true;
  }
  if (!"".equals(JSP.w(ad.getTelephone()).trim())) {
%><%=pageState.getI18n(AnagraphicalDataConstants.FLD_TELEPHONE)%>: <%=JSP.w(ad.getTelephone())%>
<%
    somePrint = true;
  }
  if (!"".equals(JSP.w(ad.getFax()).trim())) {
%><%=pageState.getI18n(AnagraphicalDataConstants.FLD_FAX)%>: <%=JSP.w(ad.getFax())%>
<%
    somePrint = true;
  }
  if (!JSP.ex(ad.getMobile())) {
%><%=pageState.getI18n(AnagraphicalDataConstants.FLD_MOBILE)%>: <%=JSP.w(ad.getMobile())%>
<%
    somePrint = true;
  }
  if (!JSP.ex(ad.getEmail())) {
%><%=pageState.getI18n(AnagraphicalDataConstants.FLD_EMAIL)%>: <%=JSP.w(ad.getEmail())%>
<%
    somePrint = true;
  }
  }
  if (!somePrint) {
%><%=pageState.getI18n("NO_ANAGRAPHICAL_DATA")%><%

    }
  }
%>