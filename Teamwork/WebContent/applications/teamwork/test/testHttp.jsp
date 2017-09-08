<%@ page import="org.apache.http.HttpResponse, org.apache.http.StatusLine, org.apache.http.client.CookieStore, org.apache.http.client.HttpClient, org.apache.http.client.methods.HttpGet, org.apache.http.client.protocol.ClientContext, org.apache.http.impl.client.BasicCookieStore, org.apache.http.impl.client.DefaultHttpClient, org.apache.http.protocol.BasicHttpContext, org.apache.http.protocol.HttpContext, org.jblooming.utilities.file.FileUtilities" %><%
  HttpClient httpClient = new DefaultHttpClient();
  CookieStore cookieStore = new BasicCookieStore();
  HttpContext httpContext = new BasicHttpContext();
  httpContext.setAttribute(ClientContext.COOKIE_STORE, cookieStore);

  HttpGet httpGet=null;

  HttpResponse resp;
  StatusLine statusLine;
    // loogin call
   /* httpGet = new HttpGet(ApplicationState.serverURL+ "/applications/teamwork/security/login.jsp?" +
      "FLD_LOGIN_NAME=" + URLEncoder.encode(importerUsername, "UTF-8") +
      "&FLD_PWD=" + URLEncoder.encode(importerPassword, "UTF-8") + "&CM=SV");

     resp = httpClient.execute(httpGet, httpContext)N;
     statusLine = response.getStatusLine();
    if (statusLine.getStatusCode()!=200){
      throw new Exception("Error calling "+httpGet.getURI());
    }

    httpGet.releaseConnection();
*/

    // json API call
    httpGet = new HttpGet("http://olpc007:8080/applications/gioia/site/API/ajaxController.jsp?CM=GET_POSTS");

    resp = httpClient.execute(httpGet, httpContext);
    statusLine = resp.getStatusLine();
    if (statusLine.getStatusCode()!=200){
      throw new Exception("Error calling "+httpGet.getURI());
    } else {
%><%=FileUtilities.readInputStream(resp.getEntity().getContent(),new StringBuffer())%><%
}


  %><hr><%
    // json API call
    httpGet = new HttpGet("http://olpc007:8080/applications/gioia/site/API/ajaxController.jsp?CM=GET_POSTS");

    resp = httpClient.execute(httpGet, httpContext);
    statusLine = resp.getStatusLine();
    if (statusLine.getStatusCode()!=200){
      throw new Exception("Error calling "+httpGet.getURI());
    } else {
      %><%=FileUtilities.readInputStream(resp.getEntity().getContent(),new StringBuffer())%><%
    }




%>