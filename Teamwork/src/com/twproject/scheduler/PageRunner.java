package com.twproject.scheduler;

import org.apache.http.HttpResponse;
import org.apache.http.StatusLine;
import org.apache.http.client.CookieStore;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.protocol.ClientContext;
import org.apache.http.impl.client.BasicCookieStore;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.protocol.BasicHttpContext;
import org.apache.http.protocol.HttpContext;
import org.jblooming.scheduler.ExecutableSupport;
import org.jblooming.scheduler.JobLogData;
import org.jblooming.scheduler.Parameter;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.settings.ApplicationState;

import java.net.URLEncoder;

public class PageRunner extends ExecutableSupport {

  @Parameter("[/applications/teamwork/plugins/customers/TEST/myTestPage.jsp]")
  public String  pageUrlFromRoot;

  @Parameter("[teamwork user used to login]")
  public String importerUsername="a_user";

  @Parameter("[teamwork user password: ]")
  public String importerPassword="";



  public JobLogData run(JobLogData jobLogData) {

    HttpClient httpClient = new DefaultHttpClient();
    CookieStore cookieStore = new BasicCookieStore();
    HttpContext httpContext = new BasicHttpContext();
    httpContext.setAttribute(ClientContext.COOKIE_STORE, cookieStore);

    HttpGet httpGet=null;

    try {

      if (!JSP.ex(pageUrlFromRoot))
        throw new Exception("Missing page url.");

      // login call
      httpGet = new HttpGet(ApplicationState.serverURL+ "/applications/teamwork/security/login.jsp?" +
              "FLD_LOGIN_NAME=" + URLEncoder.encode(importerUsername, "UTF-8") +
              "&FLD_PWD=" + URLEncoder.encode(importerPassword, "UTF-8") + "&CM=SV");

      HttpResponse response = httpClient.execute(httpGet, httpContext);
      StatusLine statusLine = response.getStatusLine();
      if (statusLine.getStatusCode()!=200){
        throw new Exception("Error calling "+httpGet.getURI());
      }

      httpGet.releaseConnection();

      // json API call
      httpGet = new HttpGet(ApplicationState.serverURL + pageUrlFromRoot) ;
      response = httpClient.execute(httpGet, httpContext);
      statusLine = response.getStatusLine();
      if (statusLine.getStatusCode()!=200){
        throw new Exception("Error calling "+httpGet.getURI());
      }





    } catch (Throwable t) {
      Tracer.jobLogger.error("PageRunner error", t);
      jobLogData.successfull = false;
    } finally {
      if (httpGet!=null){
        try {
          httpGet.releaseConnection();
        } catch (Throwable t){
          Tracer.jobLogger.error("PageRunner error releasing http connection", t);
          jobLogData.successfull = false;
        }
      }
    }


    return jobLogData;
  }



}
