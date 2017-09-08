package com.twproject.security;

import net.sf.json.JSONObject;
import org.jblooming.operator.Operator;
import org.jblooming.scheduler.ExecutableSupport;
import org.jblooming.scheduler.JobLogData;
import org.jblooming.security.License;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.RestState;

import java.io.DataInputStream;
import java.net.HttpURLConnection;
import java.net.URL;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class LicenseUpdater extends ExecutableSupport {

  public static final String key= "134a7dc8d13249837a5b22df878182a0";

  public JobLogData run(JobLogData jobLogData) throws Exception {

    RestState restState = new RestState((Operator)null);
    try {

      if(ApplicationState.isHostMode)
        return jobLogData;

      License license = License.fromFile();


      String urlToCall = "https://shop.twproject.com/utils/licenseUpdate.jsp";

      PageSeed ps = new PageSeed(urlToCall);
      ps.addClientEntry("lopf", StringUtilities.encryptBase64(license.getLicenseWrapped(),key));  //double encryption


      final String finalUTC = ps.toLinkToHref();

      StringBuffer str = new StringBuffer(512);
      URL hp = new URL(finalUTC);
      HttpURLConnection connection = (HttpURLConnection) hp.openConnection();
      connection.setDoInput(true);
      //connection.setDoOutput(true);
      //DataOutputStream output = new DataOutputStream(connection.getOutputStream());
      //output.writeBytes(finalUTC);
      //output.close();
      DataInputStream input = new DataInputStream(connection.getInputStream());
      for (int c = input.read(); c != -1; c = input.read()) {
        str.append((char) c);
      }
      input.close();

      String result = str.toString();

      JSONObject json = JSONObject.fromObject(result);

      if (json.getBoolean("ok")) {
        License newLicense = License.fromString(StringUtilities.decryptBase64(json.getString("nl"), key));
        if (newLicense.loadOk) {
          newLicense.storeLicense();
          jobLogData.notes = "License correctly updated: " + newLicense.getLicenseWrapped(); //questo mi serve quando chiamo il metodo fuori dallo schedule
          restState.initializeEntries("div");
        }
        jobLogData.successfull = true;  //questo mi serve quando chiao il metodo fuori dallo schedule
      } else {
        throw new Exception("Messages from controller:"+ json.getJSONArray("messagesFromController").toString());
      }

    } catch (Throwable e) {
      Tracer.platformLogger.error("LicenseUpdater: error updating license.", e);
      jobLogData.successfull = false;
    }

    return jobLogData;
  }

}
