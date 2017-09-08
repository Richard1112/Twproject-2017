package org.jblooming.utilities;

import org.jblooming.messaging.MailHelper;
import org.jblooming.scheduler.ExecutableSupport;
import org.jblooming.scheduler.JobLogData;
import org.jblooming.scheduler.Parameter;
import org.jblooming.tracer.Tracer;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Date;
import java.util.Set;

public class SiteAliveTester extends ExecutableSupport {

  @Parameter("[comma separated domains]")
  public String domainsSer;
  @Parameter("[comma separated emails]")
  public String emailsSer;



  public JobLogData run(JobLogData jobLogData) throws Exception {
    try {
      if (domainsSer != null && emailsSer != null) {
        Set<String> domains = StringUtilities.splitToSet(domainsSer, ",");
        Set<String> emails = StringUtilities.splitToSet(emailsSer, ",");
        for (String domain : domains) {
          try {
            if (!answersWithContent(domain))
              throw new Exception();
          } catch (Exception e) {
            MailHelper.sendHtmlMail(null,emails, domain, domain + " could not be contacted at " + DateUtilities.dateAndHourToString(new Date()));
          }
        }
      }

      jobLogData.notes = jobLogData.notes + "SiteAliveTester executed on " + DateUtilities.dateAndHourToString(new Date());
    } catch (Exception e) {
      Tracer.platformLogger.error("SiteAliveTester error", e);
        jobLogData.successfull = false;
    }

    return jobLogData;
  }

  private static boolean answersWithContent(String urlToCall) throws IOException {

    StringBuffer str = new StringBuffer(512);
    URL server = new URL(urlToCall);
    HttpURLConnection connection = (HttpURLConnection)server.openConnection();
    connection.setReadTimeout(8*1000);
    connection.setDoInput( true );
    connection.setDoOutput( true );
    DataOutputStream output = new DataOutputStream( connection.getOutputStream() );
    int queryLength = urlToCall.length();
    output.writeBytes( urlToCall );
    output.close();
    DataInputStream input = new DataInputStream( connection.getInputStream() );
    for( int c = input.read(); c != -1; c = input.read() ) {
      str.append((char)c) ;
    }
    input.close();
    return str.length()>0;
  }

}
