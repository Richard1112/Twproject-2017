package org.jblooming.utilities;

import net.sf.json.JSONObject;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.tracer.Tracer;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.view.PageState;

import javax.servlet.jsp.PageContext;
import java.io.IOException;
import java.net.URL;

/**
 * Created by rbicchierai on 15/06/2016.
 */
public class RecaptchaV2 {
  String siteKey = null;
  String secretKey = null;
  public JSONObject captchResponse=null;

  public RecaptchaV2(String siteKey, String secretKey) {
    this.siteKey = siteKey;
    this.secretKey = secretKey;
  }

  public void toHtml(PageContext pageContext) {
    try {
      pageContext.getOut().println("<script src=\"https://www.google.com/recaptcha/api.js\" async defer></script><div class=\"g-recaptcha\" data-sitekey=\""+siteKey+"\"></div>");


    } catch (IOException e) {
      throw new PlatformRuntimeException(e);
    }

  }



  public boolean isValid(PageState pageState,boolean checkHostToo){
    if (captchResponse==null){
      JSONObject jo = new JSONObject();
      jo.element("secret",secretKey);
      jo.element("response",pageState.getEntry("g-recaptcha-response").stringValueNullIfEmpty()+"");
      //valida sul server google
      try {
        //captchResponse=JSONObject.fromObject(HttpUtilities.postRestCall("http://olpc009:90/html/applications/teamwork/test/test2.jsp",jo));
        captchResponse=JSONObject.fromObject(HttpUtilities.postRestCall("https://www.google.com/recaptcha/api/siteverify",jo));
      } catch (IOException e) {
        Tracer.platformLogger.error("RecaptchaV2",e);
      }

    }

    boolean captchaOk = captchResponse.getBoolean("success");

    //eventually
    if (captchaOk && checkHostToo &&  captchResponse.has("hostname") )
      captchaOk=ApplicationState.serverURL.contains(captchResponse.getString("hostname"));

    return captchaOk;
  }
}

