package org.jblooming.waf;

import net.sf.json.JSONObject;
import net.sf.json.JSONArray;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.settings.I18n;
import org.jblooming.tracer.Tracer;
import org.jblooming.waf.view.RestState;

import javax.servlet.jsp.PageContext;
import javax.servlet.jsp.JspWriter;
import java.io.IOException;

public class JSONHelper {
  public JSONObject json;
  public PageState pageState= PageState.getCurrentPageState();

  public JSONHelper(){
    json = new JSONObject();
    json.element("ok", true);
  }


  public void error(Throwable t) {
    Tracer.platformLogger.error(t);
    String stackTrace = PlatformRuntimeException.getStackTrace(t);
    //Tracer.platformLogger.error(stackTrace);

    pageState.setError("JSON Error"); //force FCF to rollback

    JSONObject ret = new JSONObject();
    ret.element("ok", false);
    ret.element("stackTrace",stackTrace);

    JSONArray messagesFromController = new JSONArray();
    JSONObject msg = new JSONObject();
    msg.element("type", PageState.MessageType.ERROR.toString());
    msg.element("title", I18n.get("ERROR_APOLOGIES"));

    //si mette la vera causa dell'errore
    Throwable t1 = Tracer.getRootCause(t);

    if (JSP.ex(t1.getMessage()))
      msg.element("message", I18n.get(t1.getMessage()));
    else
      msg.element("message", I18n.get("ERROR_GENERIC_EXCEPTION"));

    messagesFromController.add(msg);
    ret.element("messagesFromController", messagesFromController);

    json = ret;
  }


  public void close(PageContext pageContext) throws IOException {

    JSONArray ceErrors= new JSONArray();
    for (ClientEntry ce : pageState.getClientEntriesSet()) {
      if (ce.errorCode!=null && !"__ERROR".equals(ce.name)){
        JSONObject ceE = new JSONObject();
        ceE.element("name",ce.name);
        ceE.element("label",I18n.get(ce.name));
        ceE.element("error",I18n.get(ce.errorCode));
        ceErrors.add(ceE);
      }
    }
    if (ceErrors.size()>0){
      json.element("clientEntryErrors",ceErrors);
      json.element("ok", false);
      pageState.setError("JSON Error"); //force FCF to rollback
    }

    //fill with feedback
    JSONArray messagesFromController;
    if (json.has("messagesFromController"))
      messagesFromController = json.getJSONArray("messagesFromController");
    else
      messagesFromController = new JSONArray();

    if (JSP.ex(pageState.messagesFromController) ){
    //fill json object
      for (PageState.Message m:pageState.messagesFromController){
        if (PageState.MessageType.ERROR.equals(m.type)) { // if there is an error message from controller the respons is always ok=false
          json.element("ok", false);
          pageState.setError("JSON Error"); //force FCF to rollback aggiunto a mano
        }
        JSONObject msg= new JSONObject();
        msg.element("type",m.type);
        msg.element("title",m.title);
        msg.element("message",m.message);
        messagesFromController.add(msg);
      }
      json.element("messagesFromController",messagesFromController);
    }


    // JSONP OBJECT
    JspWriter out = pageContext.getOut();
    if (JSP.ex(pageState.getEntry("__jsonp_callback"))) {
      out.print(pageState.getEntry("__jsonp_callback").stringValueNullIfEmpty() + "(");
      out.print(json.toString());
      out.print(");");

      // JSON OBJECT
    } else {
      out.print(json.toString());
    }

  }


}
