package org.jblooming.waf.html.button;

import org.jblooming.PlatformRuntimeException;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.html.core.UrlComposer;
import org.jblooming.waf.html.display.Img;
import org.jblooming.waf.html.state.Form;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;

import javax.servlet.jsp.PageContext;
import java.io.Serializable;

public class ButtonSubmit extends ButtonSupport {

  public Form form;
  public PageSeed variationsFromForm = new PageSeed();
  public boolean preserveFormStatus = false;

  public boolean alertOnRequired = false;

  public String additionalScript;
  public String additionalOnClickScript;

  public boolean alertOnChange = true;

  // used to ajax submit style
  public boolean ajaxEnabled = false;
  public String ajaxDomIdToReload = null;
  /**
   * create confirm popup
   */
  public boolean confirmRequire = false;
  public String confirmQuestion;


  public ButtonSubmit(Form form) {
    super();
    this.form = form;
    if (form != null && form.url != null && form.url.getCommand() != null)
      variationsFromForm.setCommand(form.url.getCommand());
  }

  public ButtonSubmit(String label, String command, Form form) {
    this(form);
    this.label = label;
    this.variationsFromForm.command = command;
  }


  public String generateLaunchJs() {

    StringBuffer sb = new StringBuffer();
    if (enabled) {
      sb.append(" onClick=\"stopBubble(event);");
      sb.append(generateJs());
      sb.append("\" ");// close onclick string

      if (additionalScript != null && additionalScript.trim().length() > 0)
        sb.append(' ').append(additionalScript);

    }
    return sb.toString();
  }

  public StringBuffer generateJs() {
    StringBuffer sb = new StringBuffer();

    if (confirmRequire)
      sb.append("$(this).confirm(function(){");

    Link fake = new Link(variationsFromForm);
    fake.outputModality = UrlComposer.OUTPUT_AS_JS_SUBMIT;
    fake.id = form.id;
    sb.append(fake.getHref());

    if (preserveFormStatus)
      sb.append("saveFormValues('" + form.getUniqueName() + "');  ");

    if (target != null && target.trim().length() > 0)
      sb.append("obj('").append(form.getUniqueName()).append("').target='").append(JSP.javascriptEncode(target)).append("'; ");

    if (variationsFromForm.getHref() != null && variationsFromForm.getHref().length() > 0 && !variationsFromForm.getHref().equals(form.url.getHref()))
      sb.append("obj('").append(form.getUniqueName()).append("').action='" + variationsFromForm.getHref() + "'; ");

    if (additionalOnClickScript != null)
      sb.append(additionalOnClickScript);

    String submitMethod;
    // if the button is ajax style change the submit method
    if (ajaxEnabled) {
      submitMethod = "ajaxSubmit('" + form.getUniqueName() + "'" + (ajaxDomIdToReload == null ? ");" : ",'" + ajaxDomIdToReload + "');");

    } else
      submitMethod = "try {obj('" + form.getUniqueName() + "').submit();} catch(e){};"; // by bicch matti per eccezione onUnload in caso cancel per IE

    submitMethod = (alertOnChange ? "" : "muteAlertOnChange=true;") + submitMethod;

    if (alertOnRequired)
      sb.append("if (canSubmitForm('").append(form.getUniqueName()).append("'))  {$(':focus').blur();" + submitMethod + "} ");
    else
      sb.append(submitMethod);

    if (preserveFormStatus)
      sb.append("restoreFormValues('" + form.getUniqueName() + "');  ");

    if (confirmRequire) {
      sb.append("}"); // close function block
      if (JSP.ex(confirmQuestion))
        sb.append(",'"+JSP.javascriptEncode(confirmQuestion)+"'");
      sb.append(");"); // close confirm block
    }

    return sb;
  }

  public String getLabel() {
    return label;
  }

  public static ButtonSubmit getSaveInstance(PageState pageState) {
    if (pageState.getForm() == null)
      throw new PlatformRuntimeException("getSaveInstance(PageState pageState) assumes that form is on pageState, but here is null");
    return getSaveInstance(pageState.getForm(), I18n.get("SAVE"), false);
  }

  public static ButtonSubmit getSaveInstance(Form form, String label) {
    return getSaveInstance(form, label, false);
  }

  public static ButtonSubmit getSaveInstance(Form form, String label, boolean boldify) {
    ButtonSubmit bs = new ButtonSubmit(form);
    bs.variationsFromForm.setCommand(Commands.SAVE);
    if (boldify)
      bs.label = JSP.makeTag("b", null, label);
    else
      bs.label = label;
    bs.alertOnRequired = true;
    bs.alertOnChange = false;
    bs.additionalCssClass="first";
    return bs;
  }

  public static ButtonSubmit getSearchInstance(Form form, PageState pageState) {
    ButtonSubmit bs = new ButtonSubmit(form);
    bs.variationsFromForm.setCommand(Commands.FIND);
    bs.label = I18n.get("FLD_SEARCH");
    //bs.additionalCssClass = "first";
    //bs.label = JSP.makeTag("b", null, bs.label);
    return bs;
  }

  public static ButtonSubmit getTextualInstance(String label, Form form) {
    ButtonSubmit bl = new ButtonSubmit(form);
    bl.label = label;
    bl.outputModality = ButtonSupport.TEXT_ONLY;
    return bl;
  }


  public static ButtonSupport getSubmitInstanceInBlack(Form form, String actionHref, int w, int h) {
    ButtonJS pl = new ButtonJS();
    pl.onClickScript ="submitInBlack('" + form.getUniqueName() +"','" + actionHref + "','" + w + "','" + h + "');";
    return pl;
  }


  public static ButtonSubmit getAjaxButton(Form form, String domIdToReload) {
    ButtonSubmit pl = new ButtonSubmit(form);
    pl.ajaxEnabled = true;
    pl.ajaxDomIdToReload = domIdToReload;
    return pl;
  }

  public void setMainObjectId(Serializable id) {
    variationsFromForm.mainObjectId = id;
  }


  public static ButtonJS getBlackInstance(Form form, String href){
    return new ButtonJS("submitInBlack('" +form.id + "','" + href + "')");
  }

}

