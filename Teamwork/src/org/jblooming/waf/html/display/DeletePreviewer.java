package org.jblooming.waf.html.display;

import org.jblooming.waf.ActionController;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.html.button.ButtonJS;
import org.jblooming.waf.html.core.HtmlBootstrap;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.state.Form;
import org.jblooming.waf.view.PageState;
import org.jblooming.ontology.Identifiable;

import javax.servlet.ServletException;
import javax.servlet.jsp.PageContext;
import java.io.IOException;
import java.io.Serializable;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class DeletePreviewer extends JspHelper  implements HtmlBootstrap {

  public Form form;
  public Identifiable delendo;
  public Class normalizeInstanceToSuperclass;

  public Class<? extends ActionController> controllerClass;
  public String mainObjectClassName;


  public DeletePreviewer(String id,Class<? extends ActionController> controllerClass, PageState pageState){
    urlToInclude = "/commons/layout/deletePreviewer/partDeletePreview.jsp";
    this.id=id;
    this.controllerClass=controllerClass;
    pageState.sessionState.getAttributes().put(id,this);
  }

  public DeletePreviewer(String id,String  mainObjectClassName, PageState pageState){
    urlToInclude = "/commons/layout/deletePreviewer/partDeletePreview.jsp";
    this.id=id;
    this.mainObjectClassName=mainObjectClassName;
    pageState.sessionState.getAttributes().put(id,this);
  }

  public boolean validate(PageState ps) throws IOException, ServletException {
    return ps.initedElements.contains(getDiscriminator());
  }

  public void toHtml(PageContext pageContext) {
    PageState ps = PageState.getCurrentPageState(pageContext);

    if (delendo == null) {
      delendo = ps.getMainObject();
    }
    if (Commands.DELETE_PREVIEW.equals(ps.getCommand()) || Commands.DELETE.equals(ps.getCommand())) {
      pageContext.getRequest().setAttribute(ACTION, "BODY");
      super.toHtml(pageContext);
    }
  }

  public String getDiscriminator() {
    return DeletePreviewer.class.getName();
  }


  public ButtonJS getDeleteButton(String label, Serializable deleteCandidateId){
    return new ButtonJS(label,"deletePreview('"+id+"','"+deleteCandidateId+"');");
  }

  public static ButtonJS getDeleteButton(String deletePreviewerId,String label, Serializable deleteCandidateId){
    return new ButtonJS(label,"deletePreview('"+deletePreviewerId+"','"+deleteCandidateId+"');");
  }



}
