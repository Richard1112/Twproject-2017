package org.jblooming.waf.html.display;

import net.sf.json.JSONArray;
import net.sf.json.JSONObject;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.RestState;

public class FeedbackFromController  {

  public static JSONArray getPageStateMessages(RestState pageState){
    JSONArray ret=new JSONArray();

    //create error messages from CE
    for (ClientEntry ce: pageState.getClientEntries().getClientEntries()){
      if (JSP.ex(ce.errorCode)){
        pageState.addMessageError(I18n.get("ERROR_ON_FIELD")+" \""+I18n.get(ce.name)+"\": "+I18n.get(ce.errorCode));
      }
    }
    if (pageState instanceof PageState){
      String specialError = (String)((PageState)pageState).sessionState.getAttributes().remove("__ERROR__");
      if (JSP.ex(specialError))
         pageState.addMessageError(I18n.get(specialError));
    }
    if (JSP.ex(pageState.messagesFromController) ){
    //fill json object
      for (PageState.Message m:pageState.messagesFromController){
        JSONObject msg= new JSONObject();
        msg.element("type",m.type);
        msg.element("title",m.title);
        msg.element("message",m.message);
        ret.add(msg);
      }
    }
    return ret;
  }

}