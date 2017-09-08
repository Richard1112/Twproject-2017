package com.twproject.webService;

import org.apache.commons.httpclient.util.DateUtil;
import org.apache.http.client.CookieStore;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.protocol.ClientContext;
import org.apache.http.impl.client.BasicCookieStore;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.protocol.BasicHttpContext;
import org.apache.http.protocol.HttpContext;
import org.jblooming.utilities.DateUtilities;

import java.net.URLEncoder;
import java.util.Date;

/**
 * Created by IntelliJ IDEA.
 * User: sChelazzi
 * Date: 12-12-20
 * Time: 4:28 PM
 * To change this template use File | Settings | File Templates.
 */
public class TestJsonAPICall {


  public static void main (String[] args){



    HttpClient httpClient = new DefaultHttpClient();
    CookieStore cookieStore = new BasicCookieStore();
    HttpContext httpContext = new BasicHttpContext();
    httpContext.setAttribute(ClientContext.COOKIE_STORE, cookieStore);

    String serverUrl = "http://localhost:8081";

    try {

      // loogin call
      String username = "administrator";
      String pwd = "";
      HttpGet httpGet = new HttpGet(serverUrl + "/applications/teamwork/security/login.jsp?" +
              "FLD_LOGIN_NAME=" + URLEncoder.encode(username, "UTF-8") +
              "&FLD_PWD=" + URLEncoder.encode(pwd, "UTF-8") + "&CM=SV");
      httpClient.execute(httpGet, httpContext);
      httpGet.releaseConnection();

      // json API call
      String assignmentId = "4";
      String duration = "1:00"; // oppure 10h  20m 3.5
      String description = "action worklog";
      Date when = new Date();


      httpGet = new HttpGet(serverUrl + "/applications/teamwork/task/worklog/worklogAjaxController.jsp?" +
              "assId=" + URLEncoder.encode(assignmentId, "UTF-8") +
              "&WORKLOG_DURATION=" + URLEncoder.encode(duration, "UTF-8") +
              "&WORKLOG_ACTION=" + URLEncoder.encode(description, "UTF-8") +
              "&WORKLOG_INSERTIONDATE=" + DateUtilities.dateToString(when) + "&CM=SV");

      httpClient.execute(httpGet, httpContext);
      httpGet.releaseConnection();

    } catch (Throwable e) {
      e.printStackTrace();
    }




  }






}
