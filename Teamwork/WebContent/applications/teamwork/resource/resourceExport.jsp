<%@ page import="com.twproject.resource.Company, com.twproject.resource.Person, com.twproject.resource.Resource,
org.jblooming.anagraphicalData.AnagraphicalData, org.jblooming.oql.QueryHelper, org.jblooming.utilities.DateUtilities,
org.jblooming.utilities.JSP, org.jblooming.waf.view.PageState, java.util.Date, java.util.List" %><%
  response.setContentType("text/plain");
  response.setHeader("content-disposition", "attachment; filename=teamworkContacts" + DateUtilities.dateToString(new Date(),"ddMMyyyy") + ".csv");

  PageState pageState = PageState.getCurrentPageState(request);
  QueryHelper ql = (QueryHelper) pageState.sessionState.getAttribute("EXPORT_RESOURCES");
  List<Resource> res = ql.toHql().list();
  for (Resource re : res) {

    if (re.getAnagraphicalDatas() == null || re.getAnagraphicalDatas().size() <= 1) {

      //,"Suffix","Company","Department","Job Title","Business Street","Business Street 2","Business Street 3","Business City","Business State","Business Postal Code","Business Country","Home Street","Home Street 2","Home Street 3","Home City","Home State","Home Postal Code","Home Country","Other Street","Other Street 2","Other Street 3","Other City","Other State","Other Postal Code","Other Country","Assistant's Phone","Business Fax","Business Phone","Business Phone 2","Callback","Car Phone","Company Main Phone","Home Fax","Home Phone","Home Phone 2","ISDN","Mobile Phone","Other Fax","Other Phone","Pager","Primary Phone","Radio Phone","TTY/TDD Phone","Telex","Account","Anniversary","Assistant's Name","Billing Information","Birthday","Business Address PO Container","Categories","Children","Directory Server","E-mail Address","E-mail Type","E-mail Display Name","E-mail 2 Address","E-mail 2 Type","E-mail 2 Display Name","E-mail 3 Address","E-mail 3 Type","E-mail 3 Display Name","Gender","Government ID Number","Hobby","Home Address PO Container","Initials","Internet Free Busy","Keywords","Language","Location","Manager's Name","Mileage","Notes","Office Location","Organizational ID Number","Other Address PO Container","Priority","Private","Profession","Referred By","Sensitivity","Spouse","User 1","User 2","User 3","User 4","Web Page"

      if (re instanceof Person) {
        Person p = (Person) re;
%><%=JSP.w(p.getCourtesyTitle())%>,<%=JSP.w(p.getPersonName())%>,<%=JSP.w(p.getPersonSurname())%><%

    } else if (re instanceof Company) {
        %>,,<%=JSP.w(re.getName())%><%
    }

    if (re.getAnagraphicalDatas()!=null && re.getAnagraphicalDatas().size() == 1) {
      AnagraphicalData ad =  re.getAnagraphicalDatas().iterator().next();
      %>,<%=JSP.w(ad.getEmail())%>,<%=JSP.w(ad.getAddress())%>,<%=JSP.w(ad.getCity())%>,<%=JSP.w(ad.getState())%>,<%=JSP.w(ad.getZip())%>,<%=JSP.w(ad.getCountry())%><%
    }
    %>
<%
  }

  }
  pageState.sessionState.setAttribute("EXPORT_RESOURCES",null);
%>