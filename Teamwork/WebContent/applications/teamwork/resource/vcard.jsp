<%@ page import="com.twproject.resource.Person, org.jblooming.anagraphicalData.AnagraphicalData, org.jblooming.persistence.PersistenceHome, org.jblooming.utilities.JSP, org.jblooming.waf.view.PageState"%><%

  response.setContentType("text/x-vcard");

  PageState pageState = PageState.getCurrentPageState(request);

  Person p = (Person) PersistenceHome.findByPrimaryKey(Person.class,pageState.mainObjectId);

  if (p.getAnagraphicalDatas().size()>0) {
  AnagraphicalData ad = p.getAnagraphicalDataOrdered().get(0);

%>begin:vcard
fn:<%=JSP.w(p.getPersonName()) + " " + JSP.w(p.getPersonSurname())%>
TITLE:<%=JSP.w(p.getCourtesyTitle())%>
adr:<%=JSP.w(ad.getAddress())%> <%=JSP.w(ad.getZip())%> <%=JSP.w(ad.getCity())%>   <%=JSP.w(ad.getProvince())%> <%=JSP.w(ad.getCountry())%>
TEL;type=mobile:<%=JSP.w(ad.getMobile())%>
TEL;WORK;VOICE:<%=JSP.w(ad.getTelephone())%>
TEL;WORK;FAX:<%=JSP.w(ad.getFax())%>
EMAIL;INTERNET:<%=JSP.w(p.getDefaultEmail())%>
URL:<%=JSP.w(ad.getUrl())%>
end:vcard
<%}%>